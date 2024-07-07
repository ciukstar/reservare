{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Booking
  ( getBookServicesR, postBookServicesR
  , getBookStaffR, postBookStaffR
  , getBookTimingR, postBookTimingR
  , getBookPaymentR, postBookPaymentR
  , getBookCheckoutR
  , getBookPayCompletionR
  , postBookPaymentIntentR
  , postBookPayR
  , postBookPaymentIntentCancelR
  ) where

import Control.Lens ((^?), (?~))

import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key, AsValue (_String))
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson as A (Value)
import Data.Bifunctor (Bifunctor(first))
import qualified Data.ByteString as BS (empty)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Text (pack, Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.LocalTime (utcToLocalTime, utc, localTimeToUTC)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , toSqlKey, val, where_
    )
import Database.Persist (Entity (Entity))
import qualified Database.Persist as P (exists, Filter)
import Database.Persist.Sql (fromSqlKey)
    
import Foundation
    ( Handler, Form, App (appSettings)
    , Route
      ( HomeR, BookServicesR, BookStaffR, BookTimingR, BookPaymentR
      , BookCheckoutR, BookPayCompletionR, BookPaymentIntentR
      , BookPaymentIntentCancelR, AuthR
      )
    , AppMessage
      ( MsgServices, MsgNext, MsgServices, MsgThereAreNoDataYet
      , MsgBack, MsgStaff, MsgBusiness, MsgWorkspace, MsgAppointmentTime
      , MsgPaymentMethod, MsgPayNow, MsgPayAtVenue, MsgCheckout
      , MsgPaymentAmount, MsgCancel, MsgPay, MsgPaymentIntentCancelled
      , MsgPaymentSuccessfullyCompleted, MsgSomethingWentWrong
      , MsgPaymentStatus
      )
    )
import Material3 (md3datetimeLocalField, md3mreq)
    
import Model
    ( statusSuccess, scriptRemoteStripe, endpointStripePaymentIntents
    , endpointStripePaymentIntentCancel
    , ServiceId, Service(Service)
    , Workspace (Workspace)
    , Business (Business)
    , Assignment
    , StaffId, Staff (Staff)
    , User (User), PayMethod (PayNow, PayAtVenue)
    , EntityField
      ( ServiceWorkspace, WorkspaceId, WorkspaceBusiness
      , BusinessId, ServiceName, AssignmentStaff, StaffId, StaffName
      , BusinessName, WorkspaceName, ServiceId, AssignmentService
      )
    )
    
import Network.Wreq
    ( postWith, responseBody, auth, defaults, basicAuth
    , FormParam ((:=)), getWith
    )

import Settings
    ( widgetFile, StripeConf (stripeConfPk, stripeConfSk)
    , AppSettings (appStripeConf)
    )

import Text.Hamlet (Html)
import Text.Julius (rawJS)
import Text.Read (readMaybe)

import Widgets (widgetSnackbar, widgetBanner)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core.Handler
    ( getMessages, newIdent, getYesod, getUrlRenderParams
    , getRequest, YesodRequest (reqGetParams), redirect, addMessageI
    , getMessageRender, setUltDestCurrent
    )
import Yesod.Core
    ( Yesod(defaultLayout), returnJson, MonadIO (liftIO), whamlet
    , addScriptRemote, MonadHandler (liftHandler), handlerToWidget
    )
import Yesod.Core.Types (HandlerFor)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form (FieldView(fvInput), FormResult (FormSuccess), Field (fieldView))
import Yesod.Form.Input (runInputGet, ireq, iopt)
import Yesod.Form.Fields
    ( textField, intField, radioField, optionsPairs, OptionList (olOptions)
    , Option (optionExternalValue, optionInternalValue), datetimeLocalField
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Monoid (Sum(Sum, getSum))
import Safe (headMay)


postBookPaymentIntentCancelR :: Handler ()
postBookPaymentIntentCancelR = do
    stati <- reqGetParams <$> getRequest
    intent <- runInputGet $ ireq textField "pi"
    let api = endpointStripePaymentIntentCancel intent
    sk <- encodeUtf8 . stripeConfSk . appStripeConf . appSettings <$> getYesod
    _ <- liftIO $ postWith (defaults & auth ?~ basicAuth sk BS.empty) api BS.empty
    addMessageI statusSuccess MsgPaymentIntentCancelled
    redirect (BookPaymentR,("pm",pack $ show PayNow) : stati)


postBookPayR :: Handler Html
postBookPayR = do
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        [whamlet| <h1>DONE |]
    


postBookPaymentIntentR :: Int -> Text -> Handler A.Value
postBookPaymentIntentR cents currency = do
    let api = unpack endpointStripePaymentIntents
    sk <- encodeUtf8 . stripeConfSk . appStripeConf . appSettings <$> getYesod
    let opts = defaults & auth ?~ basicAuth sk ""
    r <- liftIO $ postWith opts api [ "amount" := cents
                                    , "currency" := currency
                                    , "payment_method_types[]" := ("card" :: Text)
                                    , "payment_method_types[]" := ("paypal" :: Text)
                                    ]

    returnJson $ object [ "paymentIntentId" .= (r ^? responseBody . key "id")
                        , "clientSecret"    .= (r ^? responseBody . key "client_secret")
                        ]


getBookPayCompletionR :: Handler Html
getBookPayCompletionR = do

    intent <- runInputGet $ ireq textField "payment_intent"
    _ <- runInputGet $ ireq textField "payment_intent_client_secret"
    _ <- runInputGet $ ireq textField "redirect_status"

    
    let api = endpointStripePaymentIntents <> "/" <> intent
    sk <- encodeUtf8 . stripeConfSk . appStripeConf . appSettings <$> getYesod
    let opts = defaults & auth ?~ basicAuth sk ""
    r <- liftIO $ getWith opts (unpack api)

    let status = r ^? responseBody . key "status" . _String

    msgr <- getMessageRender
    
    let result = case status of
          Just "succeeded" -> msgr MsgPaymentSuccessfullyCompleted
          Just s -> s
          Nothing -> msgr MsgSomethingWentWrong
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentStatus
        $(widgetFile "book/completion/completion")


getBookCheckoutR :: Handler Html
getBookCheckoutR = do

    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    -- eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    -- tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )
    -- pid <- (readMaybe . unpack =<<) <$> runInputGet ( iopt textField "pid" )

    user <- maybeAuth
    case user of
      Nothing -> do
          setUltDestCurrent
          redirect $ AuthR LoginR
      Just (Entity _ (User email _ _ _ _ _ _ _)) -> do
    
          pk <- stripeConfPk . appStripeConf . appSettings <$> getYesod 

          services <- runDB ( select $ do
              x :& w <- from $ table @Service
                  `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
              case sid of
                Just y -> where_ $ x ^. ServiceId ==. val y
                Nothing -> where_ $ val False
              return (x, w) )

          let items = (\(Entity _ (Service _ name _ _),_) -> object ["id" .= name]) <$> services
          let cents = getSum $ mconcat $ (\(Entity _ (Service _ _ _ price),_) -> Sum price) <$> services
          let currency = maybe "USD" (\(_, Entity _ (Workspace _ _ _ _ c)) -> c) (headMay services)

          rndr <- getUrlRenderParams

          let confirmParams = encodeToLazyText $ object
                  [ "return_url"    .= rndr BookPayCompletionR stati
                  , "receipt_email" .= email
                  ]

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCheckout
              idButtonBack <- newIdent
              idBannerStripe <- newIdent
              idSectionPriceTag <- newIdent
              idFormPayment <- newIdent
              idElementPayment <- newIdent
              idButtonSubmitPayment <- newIdent
              idButtonCancelPayment <- newIdent
              addScriptRemote scriptRemoteStripe
              $(widgetFile "book/checkout/checkout")


postBookPaymentR :: Handler Html
postBookPaymentR = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )
    pid <- (readMaybe . unpack =<<) <$> runInputGet ( iopt textField "pid" )

    ((fr,fw),et) <- runFormPost $ formPayment sid eid tid pid

    case fr of
      FormSuccess (sid',eid',tid',pid') -> redirect ( BookCheckoutR
                                                    , [ ( "sid", pack $ show $ fromSqlKey sid')
                                                      , ( "eid", pack $ show $ fromSqlKey eid')
                                                      , ( "tid", pack $ show $ utcToLocalTime utc tid')
                                                      , ( "pid", pack $ show pid')
                                                      ]
                                                    )
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentMethod
              idFormPayment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/payment/payment")


getBookPaymentR :: Handler Html
getBookPaymentR = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )
    pid <- (readMaybe . unpack =<<) <$> runInputGet ( iopt textField "pid" )

    (fw,et) <- generateFormPost $ formPayment sid eid tid pid
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentMethod
        idFormPayment <- newIdent
        idFabNext <- newIdent
        $(widgetFile "book/payment/payment")


formPayment :: Maybe ServiceId -> Maybe StaffId -> Maybe UTCTime -> Maybe PayMethod
            -> Form (ServiceId,StaffId,UTCTime,PayMethod)
formPayment sid eid time method extra = do
        
    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>)
        <$> mreq intField "" (fromIntegral . fromSqlKey <$> sid)
        
    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>)
        <$> mreq intField "" (fromIntegral . fromSqlKey <$> eid)
        
    (timeR,timeV) <- mreq datetimeLocalField "" (utcToLocalTime utc <$> time)

    let methods = [((MsgPayNow,PayNow),"credit_card"),((MsgPayAtVenue,PayAtVenue),"point_of_sale")]
        
    (methodR,methodV) <- md3mreq (md3radioFieldList methods) "" method

    let r = (,,,) <$> serviceR <*> staffR <*> (localTimeToUTC utc <$> timeR) <*> methodR
    let w = [whamlet|#{extra} ^{fvInput serviceV} ^{fvInput staffV} ^{fvInput timeV} ^{fvInput methodV}|]

    return (r,w)

  where

      md3radioFieldList :: [((AppMessage,PayMethod),Text)] -> Field (HandlerFor App) PayMethod
      md3radioFieldList methods = (radioField (optionsPairs (fst <$> methods)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (fst <$> methods))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findMethod opt = find (\((_,m),_) -> m == optionInternalValue opt)
                
                [whamlet|
<md-list ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    $maybe ((msg,_),icon) <- findMethod opt methods
      <md-list-item type=text onclick="this.querySelector('md-radio').click()">
        <md-icon slot=start>
          #{icon}
        <div slot=headline>
          _{msg}
        <div slot=end>
          <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
            :sel x opt:checked touch-target=wrapper>
    <md-divider>
|]
              }


postBookTimingR :: Handler Html
postBookTimingR = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )

    ((fr,fw),et) <- runFormPost $ formTiming sid eid tid

    case fr of
      FormSuccess (sid',eid',tid') -> redirect ( BookPaymentR
                                               , [ ( "sid", pack $ show $ fromSqlKey sid')
                                                 , ( "eid", pack $ show $ fromSqlKey eid')
                                                 , ( "tid", pack $ show $ utcToLocalTime utc tid')
                                                 ]
                                               )
      _otherwise -> do    
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormTiming <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/timing/timing")


getBookTimingR :: Handler Html
getBookTimingR = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )

    (fw,et) <- generateFormPost $ formTiming sid eid tid
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFormTiming <- newIdent
        idFabNext <- newIdent
        $(widgetFile "book/timing/timing")


formTiming :: Maybe ServiceId -> Maybe StaffId -> Maybe UTCTime -> Form (ServiceId,StaffId,UTCTime)
formTiming sid eid time extra = do
        
    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>)
        <$> mreq intField "" (fromIntegral . fromSqlKey <$> sid)
        
    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>)
        <$> mreq intField "" (fromIntegral . fromSqlKey <$> eid)
        
    (timeR,timeV) <- md3mreq md3datetimeLocalField "" (utcToLocalTime utc <$> time)

    let r = (,,) <$> serviceR <*> staffR <*> (localTimeToUTC utc <$> timeR)
    let w = [whamlet|#{extra} ^{fvInput serviceV} ^{fvInput staffV} ^{fvInput timeV}|]

    return (r,w)


postBookStaffR :: Handler Html
postBookStaffR = do

    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )

    existsStaff <- runDB $ P.exists ([] :: [P.Filter Staff])

    ((fr,fw),et) <- runFormPost $ formStaff sid eid
    case fr of
      FormSuccess (sid',eid') -> redirect ( BookTimingR
                                          , [ ( "sid", pack $ show $ fromSqlKey sid')
                                            , ( "eid", pack $ show $ fromSqlKey eid')
                                            ]
                                          )
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              idFormStaff <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/staff/staff")


getBookStaffR :: Handler Html
getBookStaffR = do

    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )

    existsStaff <- runDB $ P.exists ([] :: [P.Filter Staff])

    (fw,et) <- generateFormPost $ formStaff sid eid
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        idFormStaff <- newIdent
        idFabNext <- newIdent
        $(widgetFile "book/staff/staff")


formStaff :: Maybe ServiceId -> Maybe StaffId -> Form (ServiceId,StaffId)
formStaff sid eid extra = do

    staff <- liftHandler $ runDB $ select $ do
        x :& e <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        case sid of
          Just y -> where_ $ x ^. AssignmentService ==. val y
          Nothing -> where_ $ val False
        orderBy [asc (e ^. StaffName), asc (e ^. StaffId)]
        return (x,e)
        
    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>)
        <$> mreq intField "" (fromIntegral . fromSqlKey <$> sid)
        
    (staffR,staffV) <- mreq (md3radioFieldList staff) "" eid

    let r = (,) <$> serviceR <*> staffR
    let w = [whamlet|#{extra} ^{fvInput serviceV} ^{fvInput staffV}|]

    return (r,w)

  where
      pairs staff = (\(_,Entity eid' (Staff name _ _ _)) -> (name, eid')) <$> staff

      md3radioFieldList :: [(Entity Assignment,Entity Staff)]
                        -> Field (HandlerFor App) StaffId
      md3radioFieldList staff = (radioField (optionsPairs (pairs staff)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs staff))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findStaff opt = find (\(_,Entity eid' _) -> eid' == optionInternalValue opt)
                
                [whamlet|
<md-list ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    $maybe (_,Entity _ (Staff ename _ _ _)) <- findStaff opt staff
      <md-list-item type=text onclick="this.querySelector('md-radio').click()">
        <div slot=headline>
          #{ename}
        <details slot=supporting-text>
          <summary style="padding:1rem">MsgWorkingHours
          <div>
            <a href=#>09:00, 
            <a href=#>09:30, 
            <a href=#>10:00,
            <a href=#>10:30
        <div slot=end>
          <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
            :sel x opt:checked touch-target=wrapper>
    <md-divider>
|]
              }


postBookServicesR :: Handler Html
postBookServicesR = do
    stati <- reqGetParams <$> getRequest
    
    ((fr,fw),et) <- runFormPost $ formService Nothing
    
    case fr of
      FormSuccess sid -> redirect (BookStaffR, [("sid",pack $ show $ fromSqlKey sid)])
      _otherwise -> do
          businesses <- runDB $ select $ do
              x <- from $ table @Business
              orderBy [asc (x ^. BusinessName)]
              return x

          workspaces <- runDB $ select $ do
              x <- from $ table @Workspace
              orderBy [asc (x ^. WorkspaceName)]
              return x

          existsService <- runDB $ P.exists ([] :: [P.Filter Service])

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormService <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/services")


getBookServicesR :: Handler Html
getBookServicesR = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    
    businesses <- runDB $ select $ do
        x <- from $ table @Business
        orderBy [asc (x ^. BusinessName)]
        return x

    workspaces <- runDB $ select $ do
        x <- from $ table @Workspace
        orderBy [asc (x ^. WorkspaceName)]
        return x
    
    existsService <- runDB $ P.exists ([] :: [P.Filter Service])

    (fw,et) <- generateFormPost $ formService sid
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormService <- newIdent
        idFabNext <- newIdent
        $(widgetFile "book/services")


formService :: Maybe ServiceId -> Form ServiceId
formService sid extra = do
    
    services <- liftHandler $ runDB $ select $ do
        x :& w :& b <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
        orderBy [asc (x ^. ServiceName), asc (x ^. ServiceId)]
        return (x,(w,b))
        
    (serviceR,serviceV) <- mreq (md3radioFieldList services) "" sid

    return (serviceR,[whamlet|#{extra} ^{fvInput serviceV}|])

  where
      pairs services = (\(Entity sid' (Service _ name _ _),_) -> (name, sid')) <$> services

      md3radioFieldList :: [(Entity Service,(Entity Workspace, Entity Business))]
                        -> Field (HandlerFor App) ServiceId
      md3radioFieldList services = (radioField (optionsPairs (pairs services)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs services))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findService opt = find (\(Entity sid' _,_) -> sid' == optionInternalValue opt)
                
                [whamlet|
<md-list ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    $maybe (Entity _ (Service _ sname _ price),(workspace,business)) <- findService opt services
      <md-list-item type=button onclick="this.querySelector('md-radio').click()">
        <div slot=headline>
          #{sname}
        $with (Entity _ (Workspace _ wname _ _ currency),Entity _ (Business _ bname)) <- (workspace,business)
          <div slot=supporting-text>
            #{wname} (#{bname})
          <div.currency slot=supporting-text data-value=#{price} data-currency=#{currency}>
            #{currency}#{price}
        <div slot=end>
          <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
            :sel x opt:checked touch-target=wrapper>
    <md-divider>
|]
              }
