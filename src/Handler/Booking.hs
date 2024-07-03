{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Booking
  ( getBookServicesR
  , postBookServicesR
  , getBookStaffR
  , getBookTimingR
  , getBookPaymentR
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
import qualified Data.ByteString as BS (empty)
import Data.Function ((&))
import Data.Text (pack, Text, unpack)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, on
    , (^.), (==.), (:&)((:&)), toSqlKey
    )
import Database.Persist (Entity (Entity))
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
      , MsgPaymentSuccessfullyCompleted, MsgSomethingWentWrong, MsgPaymentStatus
      )
    )
    
import Model
    ( statusSuccess, scriptRemoteStripe, endpointStripePaymentIntents
    , endpointStripePaymentIntentCancel
    , Service(Service)
    , Workspace (Workspace)
    , Business (Business)
    , Assignment (Assignment)
    , Staff (Staff)
    , User (User), PayMethod (PayNow)
    , EntityField
      ( ServiceWorkspace, WorkspaceId, WorkspaceBusiness
      , BusinessId, ServiceName, AssignmentStaff, StaffId, StaffName
      , BusinessName, WorkspaceName, ServiceId
      ), ServiceId
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

import Widgets (widgetSnackbar, widgetBanner)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core.Handler
    ( getMessages, newIdent, getYesod, getUrlRenderParams
    , getRequest, YesodRequest (reqGetParams), redirect, addMessageI
    , getMessageRender
    )
import Yesod.Core
    ( Yesod(defaultLayout), returnJson, MonadIO (liftIO), whamlet
    , addScriptRemote, MonadHandler (liftHandler)
    )
import Yesod.Core.Types (HandlerFor)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq, iopt)
import Yesod.Form.Fields
    ( textField, radioFieldList, intField
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form (FieldView(fvInput), FormResult (FormSuccess), Field (fieldView))


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

    user <- maybeAuth
    case user of
      Nothing -> redirect $ AuthR LoginR
      Just (Entity uid (User email _ _ _ _ _ _ _)) -> do
    
          pk <- stripeConfPk . appStripeConf . appSettings <$> getYesod 

          let items = [object ["id" .= ("Liesure Time" :: Text)]]
          let amount = 10 :: Double
          let cents = truncate $ 100 * amount
          let currency = "USD"

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


getBookPaymentR :: Handler Html
getBookPaymentR = do
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentMethod
        idFabNext <- newIdent
        $(widgetFile "book/payment/payment")


getBookTimingR :: Handler Html
getBookTimingR = do
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFabNext <- newIdent
        $(widgetFile "book/timing/timing")


getBookStaffR :: Handler Html
getBookStaffR = do

    stati <- reqGetParams <$> getRequest

    staff <- runDB $ select $ do
        x :& e <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        orderBy [asc (e ^. StaffName)]
        return (x,e)
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        idFabNext <- newIdent
        $(widgetFile "book/staff/staff")


postBookServicesR :: Handler Html
postBookServicesR = do

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

          services <- runDB $ select $ do
              x :& w :& b <- from $ table @Service
                  `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
                  `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
              orderBy [asc (x ^. ServiceName)]
              return (x,(w,b))

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormService <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/services")


getBookServicesR :: Handler Html
getBookServicesR = do

    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    
    businesses <- runDB $ select $ do
        x <- from $ table @Business
        orderBy [asc (x ^. BusinessName)]
        return x

    workspaces <- runDB $ select $ do
        x <- from $ table @Workspace
        orderBy [asc (x ^. WorkspaceName)]
        return x
    
    services <- runDB $ select $ from $ table @Service

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
      pairs services = (\(Entity sid' (Service _ name _),_) -> (name, sid')) <$> services

      md3radioFieldList :: [(Entity Service,(Entity Workspace, Entity Business))]
                        -> Field (HandlerFor App) ServiceId
      md3radioFieldList services = (radioFieldList (pairs services))
          { fieldView = \theId name attrs x isReq -> do
                let opts = zip [1 :: Int ..] services
                let sel (Left _) _ = False
                    sel (Right y) opt = opt == y
                let showVal = pack . show . fromSqlKey
                [whamlet|
<md-list ##{theId} *{attrs}>
  $forall (i,(Entity sid (Service _ sname _),(workspace,business))) <- opts
    <md-list-item type=text>
      <div slot=headline>
        #{sname}
      $with (Entity _ (Workspace _ wname address),Entity _ (Business _ bname)) <- (workspace,business)
        <div slot=supporting-text>
          #{wname} (#{bname})
        <div slot=supporting-text>
          #{address}
      <div slot=end>
        <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{showVal sid} :sel x sid:checked touch-target=wrapper>
    <md-divider>
|]
              }
