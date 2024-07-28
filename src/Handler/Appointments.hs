{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Appointments
  ( getAppointmentStaffR, postAppointmentStaffR
  , getAppointmentTimingR, postAppointmentTimingR
  , getAppointmentTimeSlotsR, postAppointmentTimeSlotsR
  , getAppointmentPaymentR, postAppointmentPaymentR
  , getAppointmentCheckoutR, getAppointmentPayCompletionR
  , getAppointmentPayAtVenueCompletionR
  , postAppointmentPaymentIntentR
  , postAppointmentPaymentIntentCancelR
  , getAppointmentDetailsR
  ) where


import Control.Lens ((^?), (?~))
import Control.Monad (when, unless, forM_)

import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key, AsValue (_String))
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson as A (Value)
import Data.Bifunctor (Bifunctor(first))
import qualified Data.ByteString as BS (empty)
import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.Map as M (member, Map, fromListWith)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(Sum, getSum))
import Data.Text (pack, Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
    ( UTCTime (utctDay), weekFirstDay, DayOfWeek (Monday)
    , DayPeriod (periodFirstDay, periodLastDay), addDays, toGregorian
    , Day, LocalTime (LocalTime, localDay)
    , addLocalTime, secondsToNominalDiffTime, getCurrentTime
    )
import Data.Time.Calendar.Month (addMonths, pattern YearMonth, Month)
import Data.Time.LocalTime (utcToLocalTime, utc, localTimeToUTC)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, on, in_
    , (^.), (==.), (:&)((:&)), (=.)
    , toSqlKey, val, where_, selectOne, Value (unValue), between, valList
    , update, set, just, distinct
    )
import Database.Persist (Entity (Entity), entityKey, insert)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, App (appSettings)
    , Route
      ( AuthR, HomeR, AppointmentCheckoutR
      , AppointmentPayCompletionR, AppointmentPaymentIntentR
      , AppointmentPaymentIntentCancelR, AppointmentPayAtVenueCompletionR
      , AppointmentStaffR, AppointmentTimingR, AppointmentTimeSlotsR
      , AppointmentPaymentR, AppointmentDetailsR
      )
    , AppMessage
      ( MsgServices, MsgNext, MsgServices, MsgThereAreNoDataYet
      , MsgBack, MsgStaff, MsgBusiness, MsgWorkspace, MsgAppointmentTime
      , MsgPaymentMethod, MsgPayNow, MsgPayAtVenue, MsgCheckout
      , MsgPaymentAmount, MsgCancel, MsgPay, MsgPaymentIntentCancelled
      , MsgSomethingWentWrong, MsgPaymentStatus, MsgReturnToHomePage
      , MsgYourBookingHasBeenCreatedSuccessfully, MsgViewBookingDetails
      , MsgFinish, MsgService, MsgEmployee, MsgSelect, MsgSelectTime
      , MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgAppointmentSetFor, MsgSelectAvailableDayAndTimePlease
      , MsgEmployeeScheduleNotGeneratedYet, MsgNoEmployeesAvailableNow
      , MsgPrevious, MsgInvalidFormData
      , MsgEmployeeWorkScheduleForThisMonthNotSetYet, MsgBookingDetails
      , MsgPrice, MsgMobile, MsgPhone, MsgLocation, MsgAddress, MsgFullName
      , MsgTheAppointment, MsgTheName, MsgUnpaid, MsgPaid, MsgUnknown
      , MsgMicrocopyPayNow, MsgMicrocopyPayAtVenue, MsgMicrocopySelectPayNow
      , MsgMicrocopySelectPayAtVenue, MsgError, MsgServiceAssignment
      , MsgCanceled, MsgPaymentIntent, MsgRole, MsgDuration
      )
    )

import Material3 (md3mreq)

import Model
    ( statusSuccess, statusError, scriptRemoteStripe
    , endpointStripePaymentIntents, endpointStripePaymentIntentCancel
    , ServiceId, Service(Service)
    , WorkspaceId, Workspace (Workspace)
    , BusinessId, Business (Business)
    , AssignmentId, Assignment (Assignment)
    , StaffId, Staff (Staff)
    , User (User), PayMethod (PayNow, PayAtVenue)
    , Schedule (Schedule)
    , BookId
    , Book
      ( Book, bookService, bookStaff, bookAppointment, bookPayMethod
      , bookStatus, bookMessage, bookCustomer, bookIntent
      )
    , PayStatus
      ( PayStatusUnpaid, PayStatusPaid, PayStatusCanceled, PayStatusError
      , PayStatusUnknown
      )
    , EntityField
      ( ServiceWorkspace, WorkspaceId, WorkspaceBusiness
      , BusinessId, ServiceName, AssignmentStaff, StaffId, StaffName
      , BusinessName, WorkspaceName, ServiceId, AssignmentService
      , AssignmentSlotInterval, ScheduleAssignment, AssignmentId
      , ScheduleDay, BookId, BookService, BookStaff, BookStatus, BookMessage, BookIntent, AssignmentRole, ServiceAvailable
      )
    )

import Network.Wreq
    ( postWith, responseBody, auth, defaults, basicAuth, getWith
    , FormParam ((:=))
    )

import Safe (headMay)

import Settings
    ( widgetFile, StripeConf (stripeConfPk, stripeConfSk)
    , AppSettings (appStripeConf)
    )

import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Julius (rawJS)
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (RenderMessage)

import Widgets (widgetSnackbar, widgetBanner)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core.Handler
    ( getMessages, newIdent, getYesod, getUrlRender, addMessageI
    , getRequest, YesodRequest (reqGetParams), redirect
    , getMessageRender, setUltDestCurrent, setUltDest
    )
import Yesod.Core
    ( Yesod(defaultLayout), returnJson, MonadIO (liftIO), whamlet
    , addScriptRemote, MonadHandler (liftHandler), handlerToWidget
    , SomeMessage (SomeMessage)
    )
import Yesod.Core.Types (HandlerFor)
import Yesod.Core.Widget (setTitleI, toWidget)
import Yesod.Form
    ( FieldView(fvInput), Field (fieldView)
    , FormResult (FormSuccess, FormFailure, FormMissing)    
    , FieldSettings (fsLabel, fsTooltip, fsId, fsName, fsAttrs, FieldSettings)
    , Option (optionDisplay), checkboxesFieldList, runFormGet, mopt
    )
import Yesod.Form.Input (runInputGet, ireq, iopt)
import Yesod.Form.Fields
    ( textField, intField, radioField, optionsPairs, OptionList (olOptions)
    , Option (optionExternalValue, optionInternalValue), datetimeLocalField
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))


getAppointmentDetailsR :: BookId -> Handler Html
getAppointmentDetailsR bid = do

    book <- runDB $ selectOne $ do
        x :& s :& w :& e <- from $ table @Book
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. BookService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Staff `on` (\(x :& _ :& _ :& e) -> x ^. BookStaff ==. e ^. StaffId)
        where_ $ x ^. BookId ==. val bid
        where_ $ s ^. ServiceAvailable ==. val True
        return (x,(s,(w,e)))

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentStatus
        $(widgetFile "appointments/details/details")


getAppointmentPayAtVenueCompletionR :: BookId -> Handler Html
getAppointmentPayAtVenueCompletionR bid = do

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentStatus
        $(widgetFile "appointments/completion/venue/completion")


postAppointmentPaymentIntentCancelR :: BookId -> Handler ()
postAppointmentPaymentIntentCancelR bid = do
    stati <- reqGetParams <$> getRequest
    intent <- runInputGet $ ireq textField "pi"
    let api = endpointStripePaymentIntentCancel intent
    sk <- encodeUtf8 . stripeConfSk . appStripeConf . appSettings <$> getYesod
    _ <- liftIO $ postWith (defaults & auth ?~ basicAuth sk BS.empty) api BS.empty

    runDB $ update $ \x -> do
        set x [BookStatus =. val PayStatusCanceled, BookIntent =. just (val intent)]
        where_ $ x ^. BookId ==. val bid
        
    addMessageI statusSuccess MsgPaymentIntentCancelled
    redirect (AppointmentPaymentR, stati)


postAppointmentPaymentIntentR :: BookId -> Int -> Text -> Handler A.Value
postAppointmentPaymentIntentR bid cents currency = do
    let api = unpack endpointStripePaymentIntents
    sk <- encodeUtf8 . stripeConfSk . appStripeConf . appSettings <$> getYesod
    let opts = defaults & auth ?~ basicAuth sk ""
    r <- liftIO $ postWith opts api [ "amount" := cents
                                    , "currency" := currency
                                    , "payment_method_types[]" := ("card" :: Text)
                                    , "payment_method_types[]" := ("paypal" :: Text)
                                    ]

    let intent = r ^? responseBody . key "id" . _String

    runDB $ update $ \x -> do
        set x [BookIntent =. val intent]
        where_ $ x ^. BookId ==. val bid

    returnJson $ object [ "paymentIntentId" .= intent
                        , "clientSecret"    .= (r ^? responseBody . key "client_secret")
                        ]


getAppointmentPayCompletionR :: BookId -> Handler Html
getAppointmentPayCompletionR bid = do

    intent <- runInputGet $ ireq textField "payment_intent"
    _ <- runInputGet $ ireq textField "payment_intent_client_secret"
    _ <- runInputGet $ ireq textField "redirect_status"

    sk <- encodeUtf8 . stripeConfSk . appStripeConf . appSettings <$> getYesod
    let api = endpointStripePaymentIntents <> "/" <> intent
    let opts = defaults & auth ?~ basicAuth sk ""
    r <- liftIO $ getWith opts (unpack api)

    msgr <- getMessageRender

    result <- case r ^? responseBody . key "status" . _String of
      Just s@"succeeded" -> do
          runDB $ update $ \x -> do
              set x [BookStatus =. val PayStatusPaid, BookMessage =. val (Just s)]
              where_ $ x ^. BookId ==. val bid
          return $ msgr MsgYourBookingHasBeenCreatedSuccessfully
      Just s -> do
          runDB $ update $ \x -> do
              set x [BookStatus =. val PayStatusError, BookMessage =. val (Just s)]
              where_ $ x ^. BookId ==. val bid
          return s
      Nothing -> do
          runDB $ update $ \x -> do
              set x [BookStatus =. val PayStatusUnknown, BookMessage =. val (Just $ msgr MsgUnknown)]
              where_ $ x ^. BookId ==. val bid
          return $ msgr MsgSomethingWentWrong

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentStatus
        $(widgetFile "appointments/completion/completion")


getAppointmentCheckoutR :: BookId -> Handler Html
getAppointmentCheckoutR bid = do

    stati <- reqGetParams <$> getRequest

    user <- maybeAuth
    case user of
      Nothing -> do
          setUltDestCurrent
          redirect $ AuthR LoginR
      Just (Entity _ (User email _ _ _ _ _ _ _)) -> do

          pk <- stripeConfPk . appStripeConf . appSettings <$> getYesod

          services <- runDB ( select $ do
              x :& s :& w <- from $ table @Book
                  `innerJoin` table @Service `on` (\(x :& s) -> x ^. BookService ==. s ^. ServiceId)
                  `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
              where_ $ x ^. BookId ==. val bid
              where_ $ s ^. ServiceAvailable ==. val True
              return (s,w) )

          let items = (\(Entity _ (Service _ name _ _ _ _),_) -> object ["id" .= name]) <$> services
          let cents = getSum $ mconcat $ (\(Entity _ (Service _ _ _ price _ _),_) -> Sum price) <$> services
          let currency = maybe "USD" (\(_, Entity _ (Workspace _ _ _ _ c)) -> c) (headMay services)

          rndr <- getUrlRender

          let confirmParams = encodeToLazyText $ object
                  [ "return_url"    .= (rndr $ AppointmentPayCompletionR bid :: Text)
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
              $(widgetFile "appointments/checkout/checkout")


postAppointmentPaymentR :: Handler Html
postAppointmentPaymentR = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )
    pid <- (readMaybe . unpack =<<) <$> runInputGet ( iopt textField "pid" )

    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    let month = maybe today ((\(y,m,_) -> YearMonth y m) . toGregorian . utctDay) tid

    ((fr,fw),et) <- runFormPost $ formPayment aid eid sid tid pid

    msgr <- getMessageRender 

    user <- maybeAuth
    
    case (fr,user) of
      (FormSuccess (_,eid',sid',tid',pid'), Nothing) -> do
          setUltDest ( AppointmentPaymentR, [ ("sid", pack $ show $ fromSqlKey sid')
                                            , ("eid", pack $ show $ fromSqlKey eid')
                                            , ("tid", pack $ show (utcToLocalTime utc tid'))
                                            , ("pid", pack $ show pid')
                                            ]
                     )
          redirect $ AuthR LoginR
          
      (FormSuccess (_,eid',sid',tid',pid'@PayNow), Just (Entity uid _)) -> do
          bid <- runDB $ insert $ Book { bookCustomer = uid
                                       , bookService = sid'
                                       , bookStaff = eid'
                                       , bookAppointment = tid'
                                       , bookPayMethod = pid'
                                       , bookStatus = PayStatusUnpaid
                                       , bookMessage = Just (msgr MsgUnpaid)
                                       , bookIntent = Nothing
                                       }
          redirect (AppointmentCheckoutR bid, stati)
          
      (FormSuccess (_,eid',sid',tid',pid'@PayAtVenue), Just (Entity uid _)) -> do
          bid <- runDB $ insert $ Book { bookCustomer = uid
                                       , bookService = sid'
                                       , bookStaff = eid'
                                       , bookAppointment = tid'
                                       , bookPayMethod = pid'
                                       , bookStatus = PayStatusUnpaid
                                       , bookMessage = Just (msgr MsgUnpaid)
                                       , bookIntent = Nothing
                                       }          
          redirect $ AppointmentPayAtVenueCompletionR bid
          
      (FormFailure errs, _) -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentMethod
              idFormPayment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/payment/payment")
              
      (FormMissing, _) -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentMethod
              idFormPayment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/payment/payment")


getAppointmentPaymentR :: Handler Html
getAppointmentPaymentR = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )
    pid <- (readMaybe . unpack =<<) <$> runInputGet ( iopt textField "pid" )

    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    let month = maybe today ((\(y,m,_) -> YearMonth y m) . toGregorian . utctDay) tid

    (fw,et) <- generateFormPost $ formPayment aid eid sid tid pid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentMethod
        idFormPayment <- newIdent
        idFabNext <- newIdent
        $(widgetFile "appointments/payment/payment")


formPayment :: Maybe AssignmentId -> Maybe StaffId -> Maybe ServiceId -> Maybe UTCTime -> Maybe PayMethod
            -> Form (AssignmentId,StaffId,ServiceId,UTCTime,PayMethod)
formPayment aid eid sid time method extra = do

    msgr <- getMessageRender

    (assignmentR,assignmentV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgServiceAssignment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgServiceAssignment),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> aid)

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgService),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)

    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgEmployee),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> eid)

    (timeR,timeV) <- mreq datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAppointmentTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAppointmentTime),("hidden","hidden")]
        } (utcToLocalTime utc <$> time)

    let methods = [ ((MsgPayNow,PayNow),("credit_card",(MsgMicrocopyPayNow,MsgMicrocopySelectPayNow)))
                  , ((MsgPayAtVenue,PayAtVenue),("point_of_sale",(MsgMicrocopyPayAtVenue,MsgMicrocopySelectPayAtVenue)))
                  ]

    (methodR,methodV) <- md3mreq (md3radioFieldList methods) "" method

    let r = (,,,,) <$> assignmentR <*> staffR <*> serviceR <*> (localTimeToUTC utc <$> timeR) <*> methodR
    let w = [whamlet|
                    #{extra}
                    ^{fvInput assignmentV}
                    ^{fvInput staffV}
                    ^{fvInput serviceV}
                    ^{fvInput timeV}
                    ^{fvInput methodV}
                    |]

    return (r,w)

  where

      md3radioFieldList :: [((AppMessage,PayMethod),(Text,(AppMessage,AppMessage)))] -> Field (HandlerFor App) PayMethod
      md3radioFieldList methods = (radioField (optionsPairs (fst <$> methods)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (fst <$> methods))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findMethod opt = find (\((_,m),_) -> m == optionInternalValue opt)

                [whamlet|
<md-list ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    $maybe ((label,_),(icon,(micro,title))) <- findMethod opt methods
      <md-list-item type=button onclick="this.querySelector('md-radio').click()" title=_{title}>
        <md-icon slot=start>
          #{icon}
        <div slot=headline>
          _{label}
        <div slot=supporting-text>
          _{micro}
        <div slot=end>
          <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
            :sel x opt:checked touch-target=wrapper>
    <md-divider>
|]
              }


postAppointmentTimeSlotsR :: Day -> Handler Html
postAppointmentTimeSlotsR day = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day

    slots <- querySlots day aid

    ((fr,fw),et) <- runFormPost $ formTimeSlot slots aid eid sid tid
    case fr of
      FormSuccess (aid',eid',sid',tid') -> redirect
          ( AppointmentTimingR month
          , [ ( "aid", pack $ show $ fromSqlKey aid')
            , ( "eid", pack $ show $ fromSqlKey eid')
            , ( "sid", pack $ show $ fromSqlKey sid')
            , ( "tid", pack $ show tid')
            ]
          )
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormSlots <- newIdent
              $(widgetFile "appointments/timing/slots/slots")
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormSlots <- newIdent
              $(widgetFile "appointments/timing/slots/slots")


getAppointmentTimeSlotsR :: Day -> Handler Html
getAppointmentTimeSlotsR day = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day

    slots <- querySlots day aid

    (fw,et) <- generateFormPost $ formTimeSlot slots aid eid sid tid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFormSlots <- newIdent
        $(widgetFile "appointments/timing/slots/slots")


querySlots :: Day -> Maybe AssignmentId -> Handler [LocalTime]
querySlots day aid = do

    interval <- maybe (secondsToNominalDiffTime (15 * 60)) unValue <$> liftHandler ( runDB $ selectOne $ do
        x <- from $ table @Assignment
        case aid of
          Just y -> where_ $ x ^. AssignmentId ==. val y
          Nothing -> where_ $ val False
        return (x ^. AssignmentSlotInterval) )

    schedule <- liftHandler $ runDB $ select $ do
        x :& a <- from $ table @Schedule
            `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
        case aid of
          Just y -> where_ $ a ^. AssignmentId ==. val y
          Nothing -> where_ $ val False
        where_ $ x ^. ScheduleDay ==. val day
        return x

    return $ concatMap
            (\(Entity _ (Schedule _ d s e)) -> let end = LocalTime d e in
                takeWhile (< end) [ addLocalTime (fromInteger i * interval) (LocalTime d s) | i <- [0..] ]
            )
            schedule


formTimeSlot :: [LocalTime] -> Maybe AssignmentId -> Maybe StaffId -> Maybe ServiceId -> Maybe LocalTime
             -> Form (AssignmentId,StaffId,ServiceId,LocalTime)
formTimeSlot slots aid eid sid tid extra = do

    msgr <- getMessageRender

    (assignmentR,assignmentV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgServiceAssignment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgServiceAssignment),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> aid)

    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgEmployee),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> eid)

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgService),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)


    (slotsR,slotsV) <- mreq (md3radioFieldList slots) "" tid

    let r = (,,,) <$> assignmentR <*> staffR <*> serviceR <*> slotsR
    let w = [whamlet|#{extra} ^{fvInput assignmentV} ^{fvInput staffV} ^{fvInput serviceV} ^{fvInput slotsV}|]

    return (r,w)

  where
      pairs xs = (\x -> (pack $ show x, x)) <$> xs

      md3radioFieldList :: [LocalTime] -> Field (HandlerFor App) LocalTime
      md3radioFieldList items = (radioField (optionsPairs . pairs $ items))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs . pairs $ items)

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                [whamlet|
<md-list ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    <md-list-item type=button onclick="this.querySelector('md-radio').click()">
      $with dt <- optionDisplay opt
        <time.time-slot slot=headline datetime=#{dt}>
          #{optionDisplay opt}
      <div slot=end>
        <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
          :sel x opt:checked touch-target=wrapper>
    <md-divider>
|]
              }


groupByKey :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupByKey f = M.fromListWith (<>) . fmap (\x -> (f x,[x]))


formSearchAssignments :: Text -> Form ( Maybe [BusinessId]
                                      , Maybe [WorkspaceId]
                                      , Maybe [ServiceId]
                                      , Maybe [Text]
                                      )
formSearchAssignments idFormSearch extra = do

    msgr <- getMessageRender

    businesses <- liftHandler $ runDB $ select $ do
        x <- from $ table @Business
        orderBy [asc (x ^. BusinessName)]
        return x

    let businessItem (Entity bid (Business _ name)) = (name,bid)

    (bR,bV) <- mopt (chipsFieldList idFormSearch MsgBusiness (businessItem <$> businesses))
        FieldSettings { fsLabel = SomeMessage MsgBusiness
                      , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
                      , fsAttrs = [("label", msgr MsgBusiness),("hidden","hidden")]
                      } Nothing

    workspaces <- liftHandler $ runDB $ select $ do
        x <- from $ table @Workspace
        unless (null businesses) $ where_ $ x ^. WorkspaceBusiness `in_` valList (entityKey <$> businesses)
        when (null businesses) $ where_ $ val False
        case bR of
          FormSuccess (Just bids@(_:_)) -> where_ $ x ^. WorkspaceBusiness `in_` valList bids
          _otherwise -> where_ $ val False
        orderBy [asc (x ^. WorkspaceName)]
        return x

    let workspaceItem (Entity wid (Workspace _ name _ _ _)) = (name,wid)

    (wR,wV) <- mopt (chipsFieldList idFormSearch MsgWorkspace (workspaceItem <$> workspaces))
        FieldSettings { fsLabel = SomeMessage MsgWorkspace
                      , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
                      , fsAttrs = [("label", msgr MsgWorkspace),("hidden","hidden")]
                      } Nothing

    services <- liftHandler $ runDB $ select $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceAvailable ==. val True
        unless (null workspaces) $ where_ $ x ^. ServiceWorkspace `in_` valList (entityKey <$> workspaces)
        when (null workspaces) $ where_ $ val False
        case wR of
          FormSuccess (Just wids@(_:_)) -> where_ $ x ^. ServiceWorkspace `in_` valList wids
          _otherwise -> where_ $ val False
        orderBy [asc (x ^. ServiceName)]
        return x

    let serviceItem (Entity sid (Service _ name _ _ _ _)) = (name,sid)

    (sR,sV) <- mopt (chipsFieldList idFormSearch MsgService (serviceItem <$> services))
        FieldSettings { fsLabel = SomeMessage MsgService
                      , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
                      , fsAttrs = [("label", msgr MsgService),("hidden","hidden")]
                      } Nothing

    roles <- liftHandler $ (unValue <$>) <$> runDB ( select $ distinct $ do
        x <- from $ table @Assignment
        unless (null services) $ where_ $ x ^. AssignmentService `in_` valList (entityKey <$> services)
        when (null services) $ where_ $ val False
        case sR of
          FormSuccess (Just sids@(_:_)) -> where_ $ x ^. AssignmentService `in_` valList sids
          _otherwise -> where_ $ val False
        orderBy [asc (x ^. AssignmentRole)]
        return $ x ^. AssignmentRole )

    (rR,rV) <- mopt (chipsFieldList idFormSearch MsgRole ((\x -> (x,x)) <$> roles))
        FieldSettings { fsLabel = SomeMessage MsgRole
                      , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
                      , fsAttrs = [("label", msgr MsgRole),("hidden","hidden")]
                      } Nothing

    let r = (,,,) <$> bR <*> wR <*> sR <*> rR
    let w = do
            idFilterChips <- newIdent
            toWidget [cassius|
                             ##{idFilterChips}
                                 display: flex
                                 flex-direction: column

                                 details
                                     summary
                                         position: relative
                                         padding: 1rem
                                         display: flex
                                         align-items: center
                                         justify-content: flex-start

                                     summary::before
                                         margin-right: 0.5rem
                                         content: 'filter_alt'
                                         font-family: 'Material Symbols Outlined'
                                         font-size: 24px

                                     summary::after
                                         margin-left: auto
                                         content: 'arrow_drop_down'
                                         font-family: 'Material Symbols Outlined'
                                         font-size: 32px
                                         transition: 0.2s

                                     p
                                         padding: 0 1rem

                                 details[open] > summary::after
                                     transform: rotate(180deg)

            |]
            [whamlet|
                    <div ##{idFilterChips}>
                      #{extra}
                      ^{fvInput bV}
                      $case bR
                        $of FormSuccess (Just ((:) _ _))
                          <md-divider>
                          ^{fvInput wV}
                        $of _
                      $case wR
                        $of FormSuccess (Just ((:) _ _))
                          <md-divider>
                          ^{fvInput sV}
                        $of _
                      $case sR
                        $of FormSuccess (Just ((:) _ _))
                          <md-divider>
                          ^{fvInput rV}
                        $of _
            |]

    return (r,w)
  where
      chipsFieldList :: (Eq a, RenderMessage App msg) => Text -> AppMessage -> [(msg,a)] -> Field Handler [a]
      chipsFieldList idForm label items = (checkboxesFieldList items)
          { fieldView = \theId name attrs x _isReq -> do
                opts <- olOptions <$> handlerToWidget (optionsPairs items)
                let sele (Left _) _ = False
                    sele (Right vals) opt = optionInternalValue opt `elem` vals
                [whamlet|
                  <details :any (sele x) opts:open>
                    <summary.md-typescale-label-large #summary#{theId}>
                      <md-ripple for=summary#{theId}>
                      _{label}
                    <p>
                      <md-chip-set ##{theId}>
                        $forall opt <- opts
                          <md-filter-chip label=#{optionDisplay opt} :sele x opt:selected
                            onclick="this.querySelector('input').click();document.getElementById('#{idForm}').submit()">
                            <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :sele x opt:checked>
                |]
          }


postAppointmentTimingR :: Month -> Handler Html
postAppointmentTimingR month = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    ((fr,fw),et) <- runFormPost $ formTiming aid eid sid tid

    case fr of
      FormSuccess (aid',eid',sid',tid') -> redirect ( AppointmentPaymentR
                                                    , [ ( "aid", pack $ show $ fromSqlKey aid' )
                                                      , ( "eid", pack $ show $ fromSqlKey eid' )
                                                      , ( "sid", pack $ show $ fromSqlKey sid' )
                                                      , ( "tid", pack $ show tid' )
                                                      ]
                                                    )
      FormFailure errs -> do

          let start = weekFirstDay Monday (periodFirstDay month)
          let end = addDays 41 start
          let page = [start .. end]
          let next = addMonths 1 month
          let prev = addMonths (-1) month

          today <- utctDay <$> liftIO getCurrentTime

          slots <- groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$> runDB ( select $ do
              x :& a <- from $ table @Schedule
                  `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
              case aid of
                Just assignment -> where_ $ a ^. AssignmentId ==. val assignment
                Nothing -> where_ $ val False
              where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
              return x )

          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormTiming <- newIdent
              idCalendarPage <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/timing/timing")
              
      FormMissing -> do

          let start = weekFirstDay Monday (periodFirstDay month)
          let end = addDays 41 start
          let page = [start .. end]
          let next = addMonths 1 month
          let prev = addMonths (-1) month

          today <- utctDay <$> liftIO getCurrentTime

          slots <- groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$> runDB ( select $ do
              x :& a <- from $ table @Schedule
                  `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
              case aid of
                Just assignment -> where_ $ a ^. AssignmentId ==. val assignment
                Nothing -> where_ $ val False
              where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
              return x )

          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormTiming <- newIdent
              idCalendarPage <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/timing/timing")


getAppointmentTimingR :: Month -> Handler Html
getAppointmentTimingR month = do
    stati <- reqGetParams <$> getRequest
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    (fw,et) <- generateFormPost $ formTiming aid eid sid tid

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    today <- utctDay <$> liftIO getCurrentTime

    slots <- groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$> runDB ( select $ do
        x :& a <- from $ table @Schedule
            `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
        case aid of
          Just assignment -> where_ $ a ^. AssignmentId ==. val assignment
          Nothing -> where_ $ val False
        where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
        return x )

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFormTiming <- newIdent
        idCalendarPage <- newIdent
        idFabNext <- newIdent
        $(widgetFile "appointments/timing/timing")


formTiming :: Maybe AssignmentId -> Maybe StaffId -> Maybe ServiceId -> Maybe LocalTime
           -> Form (AssignmentId, StaffId, ServiceId,LocalTime)
formTiming aid eid sid time extra = do

    msgr <- getMessageRender

    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgEmployee),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> eid)

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgService),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)

    (assignmentR,assignmentV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgServiceAssignment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgServiceAssignment),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> aid)

    (timeR,timeV) <- mreq datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAppointmentTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAppointmentTime),("hidden","hidden")]
        } time

    let r = (,,,) <$> assignmentR <*> staffR <*> serviceR <*> timeR
    let w = [whamlet|#{extra} ^{fvInput staffV} ^{fvInput serviceV} ^{fvInput assignmentV} ^{fvInput timeV}|]

    return (r,w)


postAppointmentStaffR :: Handler Html
postAppointmentStaffR = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )

    idFormSearch <- newIdent

    ((fr0,fw0),et0) <- runFormGet $ formSearchAssignments idFormSearch

    let (bids,wids,sids,roles) = case fr0 of
          FormSuccess r -> r
          _otherwise -> (Just [],Just [],Just [],Just [])

    ((fr,fw),et) <- runFormPost $ formAssignments bids wids sids roles aid

    case fr of
      FormSuccess (aid',(eid',sid')) -> do
          today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
          month <- fromMaybe today . (readMaybe . unpack =<<) <$> runInputGet ( iopt textField "month" )
          redirect (AppointmentTimingR month, [ ("aid",pack $ show $ fromSqlKey aid')
                                              , ("eid",pack $ show $ fromSqlKey eid')
                                              , ("sid",pack $ show $ fromSqlKey sid')
                                              ])
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormAssignment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/assignments")
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormAssignment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/assignments")


getAppointmentStaffR :: Handler Html
getAppointmentStaffR = do

    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )

    idFormSearch <- newIdent

    ((fr0,fw0),et0) <- runFormGet $ formSearchAssignments idFormSearch

    let (bids,wids,sids,roles) = case fr0 of
          FormSuccess r -> r
          _otherwise -> (Just [],Just [],Just [],Just [])

    (fw,et) <- generateFormPost $ formAssignments bids wids sids roles aid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        idFormAssignment <- newIdent
        idFabNext <- newIdent
        $(widgetFile "appointments/assignments")


formAssignments :: Maybe [BusinessId] -> Maybe [WorkspaceId] -> Maybe [ServiceId] -> Maybe [Text]
                -> Maybe AssignmentId -> Form (AssignmentId,(StaffId,ServiceId))
formAssignments bids wids sids roles aid extra = do

    assignments <- liftHandler $ runDB $ select $ do
        x :& e :& s :& w :& b <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
            `innerJoin` table @Service `on` (\(x :& _ :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& _ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)

        where_ $ s ^. ServiceAvailable ==. val True
        
        case roles of
          Just xs@(_:_) -> where_ $ x ^. AssignmentRole `in_` valList xs
          _otherwise -> where_ $ val True
        case sids of
          Just xs@(_:_) -> where_ $ s ^. ServiceId `in_` valList xs
          _otherwise -> where_ $ val True
        case wids of
          Just xs@(_:_) -> where_ $ w ^. WorkspaceId `in_` valList xs
          _otherwise -> where_ $ val True
        case bids of
          Just xs@(_:_) -> where_ $ b ^. BusinessId `in_` valList xs
          _otherwise -> where_ $ val True
        orderBy [asc (e ^. StaffName), asc (e ^. StaffId)]
        return (x,(e,(s,(w,b))))

    let getDefault :: Maybe AssignmentId
                   -> [(Entity Assignment,(Entity Staff,(Entity Service,(Entity Workspace,Entity Business))))]
                   -> Maybe (AssignmentId,(StaffId,ServiceId))
        getDefault (Just aid') assignments' =
            ((\(Entity aid'' _,(Entity eid'' _,(Entity sid'' _,_))) -> (aid'',(eid'',sid''))) <$>)
               . find (\(Entity aid'' _,(_,_)) -> aid'' == aid') $ assignments'
        getDefault Nothing _ = Nothing

    (assigmentR,assigmentV) <- mreq (md3radioFieldList assignments) "" (getDefault aid assignments)

    let r = assigmentR
    let w = [whamlet|#{extra} ^{fvInput assigmentV}|]

    return (r,w)

  where
      
      pairs assignments = (\(Entity aid' _,(Entity eid' (Staff name _ _ _),(Entity sid' _,_))) -> (name, (aid',(eid',sid'))))
          <$> assignments

      md3radioFieldList :: [(Entity Assignment,(Entity Staff,(Entity Service,(Entity Workspace,Entity Business))))]
                        -> Field (HandlerFor App) (AssignmentId,(StaffId,ServiceId))
      md3radioFieldList assignments = (radioField (optionsPairs (pairs assignments)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs assignments))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findAssigment opt = find (\(Entity aid' _,(_,_)) -> aid' == fst (optionInternalValue opt))

                [whamlet|
$if null opts
    <figure style="text-align:center">
      <span.on-secondary style="font-size:4rem">&varnothing;
      <figcaption.md-typescale-body-large>
        _{MsgNoEmployeesAvailableNow}.
$else
  <md-list ##{theId} *{attrs}>
    $forall (i,opt) <- opts
      $maybe (assignment,(Entity _ (Staff ename _ _ _),(service,(workspace,business)))) <- findAssigment opt assignments
        <md-list-item type=text onclick="this.querySelector('md-radio').click()">
          <div slot=headline>
            #{ename}
          <div slot=supporting-text>
            $with Entity _ (Assignment _ _ role _ _ _) <- assignment
              #{role}
            $with (Entity _ (Business _ bname),Entity _ (Workspace _ wname _ _ _)) <- (business,workspace)
              \ (#{bname} - #{wname})
          <div slot=supporting-text>
            $with (Entity _ (Workspace _ _ _ _ currency),Entity _ (Service _ name _ price _ _)) <- (workspace,service)
              #{name} (
              <span.currency data-value=#{price} data-currency=#{currency}>
                #{show price} #{currency}
              )
          <div slot=end>
            <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
              :sel x opt:checked touch-target=wrapper>
      <md-divider>
|]
              }
