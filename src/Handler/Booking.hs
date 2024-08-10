{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Handler.Booking
  ( getBookServicesR, postBookServicesR
  , getBookStaffR, postBookStaffR
  , getBookTimingR, postBookTimingR
  , getBookTimeSlotsR, postBookTimeSlotsR
  , getBookPaymentR, postBookPaymentR
  , getBookDetailsR
  ) where

import qualified AtVenue.Data as V (Route(CheckoutR))

import Control.Monad (when, unless, forM_)

import Data.Bifunctor (Bifunctor(first,bimap))
import Data.Foldable (find)
import qualified Data.Map as M (member, Map, fromListWith)
import Data.Maybe (fromMaybe)
import Data.Text (pack, Text, unpack)
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
    , (^.), (==.), (:&)((:&))
    , toSqlKey, val, where_, selectOne, Value (unValue), between
    , valList, just, subSelectList
    )
import Database.Persist (Entity (Entity), entityKey, insert)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, App
    , Route
      ( HomeR, BookServicesR, BookStaffR, BookTimingR, BookTimeSlotsR
      , BookPaymentR, AuthR, StripeR, YookassaR, AtVenueR
      )
    , AppMessage
      ( MsgServices, MsgNext, MsgServices, MsgThereAreNoDataYet
      , MsgBack, MsgStaff, MsgBusiness, MsgWorkspace, MsgAppointmentTime
      , MsgPaymentOption, MsgCancel, MsgPaymentStatus, MsgReturnToHomePage
      , MsgService, MsgEmployee, MsgSelect, MsgSelectTime
      , MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgAppointmentSetFor, MsgSelectAvailableDayAndTimePlease
      , MsgEmployeeScheduleNotGeneratedYet, MsgNoEmployeesAvailableNow
      , MsgPrevious, MsgNoServicesWereFoundForSearchTerms, MsgInvalidFormData
      , MsgEmployeeWorkScheduleForThisMonthNotSetYet, MsgBookingDetails
      , MsgPrice, MsgMobile, MsgPhone, MsgLocation, MsgAddress, MsgFullName
      , MsgTheAppointment, MsgTheName, MsgDuration, MsgPaymentGatewayNotSpecified
      , MsgNoPaymentsHaveBeenMadeYet, MsgPayments, MsgTotalCharge, MsgError
      , MsgNoPaymentOptionSpecified, MsgWorkspaceWithoutPaymentOptions
      )
    )

import Material3 (md3mreq)

import Model
    ( statusError
    , ServiceId, Service(Service)
    , WorkspaceId, Workspace (Workspace)
    , BusinessId, Business (Business)
    , Assignment
    , StaffId, Staff (Staff)
    , PayMethod (PayNow, PayAtVenue)
    , Schedule (Schedule)
    , BookId
    , Book
      ( Book, bookService, bookStaff, bookAppointment
      , bookCustomer, bookCharge, bookCurrency
      )
    , PayOptionId, PayOption (PayOption)
    , Payment (Payment)
    , PayGateway (PayGatewayStripe, PayGatewayYookassa)
    , EntityField
      ( ServiceWorkspace, WorkspaceId, WorkspaceBusiness
      , BusinessId, ServiceName, AssignmentStaff, StaffId, StaffName
      , BusinessName, WorkspaceName, ServiceId, AssignmentService
      , AssignmentSlotInterval, ScheduleAssignment, AssignmentId
      , ScheduleDay, BookId, BookService, BookStaff, ServiceAvailable
      , PayOptionWorkspace, PayOptionId, ServicePrice, WorkspaceCurrency
      , PaymentBook, PaymentOption
      )
    )

import Settings ( widgetFile )
    
import qualified Stripe.Data as S (Route(CheckoutR))

import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (RenderMessage)

import Widgets (widgetSnackbar, widgetBanner)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core.Handler
    ( getMessages, newIdent, addMessageI, getRequest
    , YesodRequest (reqGetParams), redirect, getMessageRender
    , setUltDestCurrent, setUltDest, setUltDestReferer
    )
import Yesod.Core
    ( Yesod(defaultLayout), MonadIO (liftIO), whamlet
    , MonadHandler (liftHandler), handlerToWidget
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
import Yesod.Form.Input (runInputGet, iopt, ireq)
import Yesod.Form.Fields
    ( textField, intField, radioField, optionsPairs, OptionList (olOptions)
    , Option (optionExternalValue, optionInternalValue), datetimeLocalField
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))

import qualified Yookassa.Data as Y (Route(CheckoutR))


getBookDetailsR :: BookId -> Handler Html
getBookDetailsR bid = do

    book <- runDB $ selectOne $ do
        x :& s :& w :& e <- from $ table @Book
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. BookService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Staff `on` (\(x :& _ :& _ :& e) -> x ^. BookStaff ==. e ^. StaffId)
        where_ $ x ^. BookId ==. val bid
        where_ $ s ^. ServiceAvailable ==. val True
        return (x,(s,(w,e)))

    payments <- runDB $ select $ do
        x :& o <- from $ table @Payment
            `innerJoin` table @PayOption `on` (\(x :& o) -> x ^. PaymentOption ==. o ^. PayOptionId)
        where_ $ x ^. PaymentBook ==. val bid
        return (x,o)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentStatus 
        $(widgetFile "book/details/details")


postBookPaymentR :: Handler Html
postBookPaymentR = do
    
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )
    oid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "oid" )

    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    let month = maybe today ((\(y,m,_) -> YearMonth y m) . toGregorian . utctDay) tid
    
    ((fr,fw),et) <- runFormPost $ formPayment sid eid tid oid

    user <- maybeAuth
    
    case (fr,user) of
      (FormSuccess (sid',eid',tid',oid'), Nothing) -> do
          setUltDest ( BookPaymentR, [ ("sid", pack $ show $ fromSqlKey sid')
                                     , ("eid", pack $ show $ fromSqlKey eid')
                                     , ("tid", pack $ show (utcToLocalTime utc tid'))
                                     , ("oid", pack $ show $ fromSqlKey oid')
                                     ]
                     )
          redirect $ AuthR LoginR
          
      (FormSuccess (sid',eid',tid',oid'), Just (Entity uid _)) -> do

          charge' <- (bimap unValue unValue <$>) <$> runDB ( selectOne $ do
              x :& w <- from $ table @Service
                  `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
              where_ $ x ^. ServiceId ==. val sid'
              return (x ^. ServicePrice, w ^. WorkspaceCurrency) )
          
          option' <- runDB $ selectOne $ do
              x <- from $ table @PayOption
              where_ $ x ^. PayOptionId ==. val oid'
              return x
              
          case (charge',option') of
            (Just (charge,currency),Just (Entity _ (PayOption _ PayNow _ (Just PayGatewayStripe) _ _))) -> do
                bid <- runDB $ insert $ Book { bookCustomer = uid
                                             , bookService = sid'
                                             , bookStaff = eid'
                                             , bookAppointment = tid'
                                             , bookCharge = charge
                                             , bookCurrency = currency
                                             }
                let params = [ ("sid", pack $ show $ fromSqlKey sid')
                             , ("eid", pack $ show $ fromSqlKey eid')
                             , ("tid", pack $ show (utcToLocalTime utc tid'))
                             , ("oid", pack $ show $ fromSqlKey oid')
                             ]
                setUltDest (BookPaymentR, params)
                redirect (StripeR $ S.CheckoutR bid oid', params)
                    
            (Just (charge,currency),Just (Entity _ (PayOption _ PayNow _ (Just PayGatewayYookassa) _ _))) -> do
                bid <- runDB $ insert $ Book { bookCustomer = uid
                                             , bookService = sid'
                                             , bookStaff = eid'
                                             , bookAppointment = tid'
                                             , bookCharge = charge
                                             , bookCurrency = currency
                                             }
                let params = [ ("sid", pack $ show $ fromSqlKey sid')
                             , ("eid", pack $ show $ fromSqlKey eid')
                             , ("tid", pack $ show (utcToLocalTime utc tid'))
                             , ("oid", pack $ show $ fromSqlKey oid')
                             ]
                setUltDest (BookPaymentR, params)
                redirect (YookassaR $ Y.CheckoutR bid oid', params)
                    
            (Just (charge,currency),Just (Entity _ (PayOption _ PayAtVenue _ _ _ _))) -> do
                bid <- runDB $ insert $ Book { bookCustomer = uid
                                             , bookService = sid'
                                             , bookStaff = eid'
                                             , bookAppointment = tid'
                                             , bookCharge = charge
                                             , bookCurrency = currency
                                             }
                redirect (AtVenueR $ V.CheckoutR bid oid')
                
            (_,Just (Entity _ (PayOption _ PayNow _ Nothing _ _))) -> do
                addMessageI statusError MsgPaymentGatewayNotSpecified
                msgs <- getMessages
                defaultLayout $ do
                    setTitleI MsgPaymentOption
                    idFormPayment <- newIdent
                    idFabNext <- newIdent
                    $(widgetFile "book/payment/payment")
                
            _otherwise -> do
                addMessageI statusError MsgInvalidFormData
                msgs <- getMessages
                defaultLayout $ do
                    setTitleI MsgPaymentOption
                    idFormPayment <- newIdent
                    idFabNext <- newIdent
                    $(widgetFile "book/payment/payment")
          
      (FormFailure errs, _) -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idFormPayment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/payment/payment")
              
      (FormMissing, _) -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idFormPayment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/payment/payment")


getBookPaymentR :: Handler Html
getBookPaymentR = do
    stati <- reqGetParams <$> getRequest
    sid <- toSqlKey <$> runInputGet ( ireq intField "sid" )
    eid <- toSqlKey <$> runInputGet ( ireq intField "eid" )
    tid <- localTimeToUTC utc <$> runInputGet ( ireq datetimeLocalField "tid" )
    oid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "oid" )

    let month = ((\(y,m,_) -> YearMonth y m) . toGregorian . utctDay) tid

    user <- maybeAuth

    charge' <- (bimap unValue unValue <$>) <$> runDB ( selectOne $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. ServiceId ==. val sid
        return (x ^. ServicePrice, w ^. WorkspaceCurrency) )
    
    options <- runDB $ select $ do
        x <- from $ table @PayOption
        where_ $ x ^. PayOptionWorkspace `in_` subSelectList
            ( do
                  s <- from $ table @Service
                  where_ $ s ^. ServiceId ==. val sid
                  return $ s ^. ServiceWorkspace
            )
        return x

    case (options,charge',user) of
      (_,_,Nothing) -> do
          setUltDestCurrent
          redirect $ AuthR LoginR
          
      ([],_,_) -> do
          addMessageI statusError MsgNoPaymentOptionSpecified
          addMessageI statusError MsgWorkspaceWithoutPaymentOptions
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgError
              $(widgetFile "book/payment/error")
          
      ([Entity oid' (PayOption _ PayNow _ (Just PayGatewayStripe) _ _)],Just (charge,currency),Just (Entity uid _)) -> do
          bid <- runDB $ insert $ Book { bookCustomer = uid
                                       , bookService = sid
                                       , bookStaff = eid
                                       , bookAppointment = tid
                                       , bookCharge = charge
                                       , bookCurrency = currency
                                       }
          setUltDestReferer
          redirect ( StripeR $ S.CheckoutR bid oid', [ ("sid", pack $ show $ fromSqlKey sid)
                                                     , ("eid", pack $ show $ fromSqlKey eid)
                                                     , ("tid", pack $ show (utcToLocalTime utc tid))
                                                     , ("oid", pack $ show $ fromSqlKey oid')
                                                     ]
                   )
              
      ([Entity oid' (PayOption _ PayNow _ (Just PayGatewayYookassa) _ _)],Just (charge,currency),Just (Entity uid _)) -> do
          bid <- runDB $ insert $ Book { bookCustomer = uid
                                       , bookService = sid
                                       , bookStaff = eid
                                       , bookAppointment = tid
                                       , bookCharge = charge
                                       , bookCurrency = currency
                                       }
          setUltDestReferer
          redirect ( YookassaR $ Y.CheckoutR bid oid', [ ("sid", pack $ show $ fromSqlKey sid)
                                                       , ("eid", pack $ show $ fromSqlKey eid)
                                                       , ("tid", pack $ show (utcToLocalTime utc tid))
                                                       , ("oid", pack $ show $ fromSqlKey oid')
                                                       ]
                   )
          
      ([Entity _ (PayOption _ PayNow _ Nothing _ _)],Just _,Just _) -> do
          addMessageI statusError MsgPaymentGatewayNotSpecified
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgError
              $(widgetFile "book/payment/error")
          
      ([Entity oid' (PayOption _ PayAtVenue _ _ _ _)],Just (charge,currency),Just (Entity uid _)) -> do
          bid <- runDB $ insert $ Book { bookCustomer = uid
                                       , bookService = sid
                                       , bookStaff = eid
                                       , bookAppointment = tid
                                       , bookCharge = charge
                                       , bookCurrency = currency
                                       }
          redirect (AtVenueR $ V.CheckoutR bid oid')
          
      _ -> do
          
          (fw,et) <- generateFormPost $ formPayment (Just sid) (Just eid) (Just tid) oid

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idFormPayment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/payment/payment")


formPayment :: Maybe ServiceId -> Maybe StaffId -> Maybe UTCTime -> Maybe PayOptionId
            -> Form (ServiceId,StaffId,UTCTime,PayOptionId)
formPayment sid eid time option extra = do

    msgr <- getMessageRender

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

    options <- liftHandler $ runDB $ select $ do
        x <- from $ table @PayOption
        where_ $ x ^. PayOptionWorkspace `in_` subSelectList
            ( do
                  s <- from $ table @Service
                  where_ $ just (s ^. ServiceId) ==. val sid
                  return $ s ^. ServiceWorkspace
            )
        return x

    (optionR,optionV) <- md3mreq (md3radioFieldList options) "" option

    let r = (,,,) <$> serviceR <*> staffR <*> (localTimeToUTC utc <$> timeR) <*> optionR
    let w = [whamlet|#{extra} ^{fvInput serviceV} ^{fvInput staffV} ^{fvInput timeV} ^{fvInput optionV}|]

    return (r,w)

  where

      pairs (Entity oid' (PayOption _ _ name _ _ _)) = (name, oid')
      
      md3radioFieldList :: [Entity PayOption] -> Field (HandlerFor App) PayOptionId
      md3radioFieldList options = (radioField (optionsPairs (pairs <$> options)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs <$> options))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findOption opt = find (\(Entity oid _) -> oid == optionInternalValue opt)

                [whamlet|
<md-list ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    $maybe Entity _ (PayOption _ _ oname _ descr icon) <- findOption opt options
      <md-list-item type=button onclick="this.querySelector('md-radio').click()">
        $maybe icon <- icon
          <md-icon slot=start>
            #{icon}
        <div slot=headline>
          #{oname}
        $maybe descr <- descr
          <div slot=supporting-text>
            #{descr}
        <div slot=end>
          <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
            :sel x opt:checked touch-target=wrapper>
    <md-divider>
|]
              }


postBookTimeSlotsR :: Day -> Handler Html
postBookTimeSlotsR day = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day

    slots <- querySlots day sid eid

    ((fr,fw),et) <- runFormPost $ formTimeSlot slots sid eid tid
    case fr of
      FormSuccess (sid',eid',tid') -> redirect
          ( BookTimingR month
          , [ ( "sid", pack $ show $ fromSqlKey sid')
            , ( "eid", pack $ show $ fromSqlKey eid')
            , ( "tid", pack $ show tid')
            ]
          )
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormSlots <- newIdent
              $(widgetFile "book/timing/slots/slots")
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormSlots <- newIdent
              $(widgetFile "book/timing/slots/slots")


getBookTimeSlotsR :: Day -> Handler Html
getBookTimeSlotsR day = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day

    slots <- querySlots day sid eid

    (fw,et) <- generateFormPost $ formTimeSlot slots sid eid tid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFormSlots <- newIdent
        $(widgetFile "book/timing/slots/slots")


querySlots :: Day -> Maybe ServiceId -> Maybe StaffId -> Handler [LocalTime]
querySlots day sid eid = do

    interval <- maybe (secondsToNominalDiffTime (15 * 60)) unValue <$> liftHandler ( runDB $ selectOne $ do
        x <- from $ table @Assignment
        case sid of
          Just y -> where_ $ x ^. AssignmentService ==. val y
          Nothing -> where_ $ val False
        case eid of
          Just y -> where_ $ x ^. AssignmentStaff ==. val y
          Nothing -> where_ $ val False
        return (x ^. AssignmentSlotInterval) )

    schedule <- liftHandler $ runDB $ select $ do
        x :& a <- from $ table @Schedule
            `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
        case eid of
          Just y -> where_ $ a ^. AssignmentStaff ==. val y
          Nothing -> where_ $ val False
        case sid of
          Just y -> where_ $ a ^. AssignmentService ==. val y
          Nothing -> where_ $ val False
        where_ $ x ^. ScheduleDay ==. val day
        return x

    return $ concatMap
            (\(Entity _ (Schedule _ d s e)) -> let end = LocalTime d e in
                takeWhile (< end) [ addLocalTime (fromInteger i * interval) (LocalTime d s) | i <- [0..] ]
            )
            schedule


formTimeSlot :: [LocalTime] -> Maybe ServiceId -> Maybe StaffId -> Maybe LocalTime
             -> Form (ServiceId,StaffId,LocalTime)
formTimeSlot slots sid eid tid extra = do

    msgr <- getMessageRender

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


    (slotsR,slotsV) <- mreq (md3radioFieldList slots) "" tid

    let r = (,,) <$> serviceR <*> staffR <*> slotsR
    let w = [whamlet|#{extra} ^{fvInput serviceV} ^{fvInput staffV} ^{fvInput slotsV}|]

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


postBookTimingR :: Month -> Handler Html
postBookTimingR month = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    ((fr,fw),et) <- runFormPost $ formTiming sid eid tid

    case fr of
      FormSuccess (sid',eid',tid') -> redirect ( BookPaymentR
                                               , [ ( "sid", pack $ show $ fromSqlKey sid' )
                                                 , ( "eid", pack $ show $ fromSqlKey eid' )
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
              case sid of
                Just service -> where_ $ a ^. AssignmentService ==. val service
                Nothing -> where_ $ val False
              case eid of
                Just employee -> where_ $ a ^. AssignmentStaff ==. val employee
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
              $(widgetFile "book/timing/timing")
              
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
              case sid of
                Just service -> where_ $ a ^. AssignmentService ==. val service
                Nothing -> where_ $ val False
              case eid of
                Just employee -> where_ $ a ^. AssignmentStaff ==. val employee
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
              $(widgetFile "book/timing/timing")


getBookTimingR :: Month -> Handler Html
getBookTimingR month = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    (fw,et) <- generateFormPost $ formTiming sid eid tid

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    today <- utctDay <$> liftIO getCurrentTime

    slots <- groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$> runDB ( select $ do
        x :& a <- from $ table @Schedule
            `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
        case sid of
          Just service -> where_ $ a ^. AssignmentService ==. val service
          Nothing -> where_ $ val False
        case eid of
          Just employee -> where_ $ a ^. AssignmentStaff ==. val employee
          Nothing -> where_ $ val False
        where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
        return x )


    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFormTiming <- newIdent
        idCalendarPage <- newIdent
        idFabNext <- newIdent
        $(widgetFile "book/timing/timing")


groupByKey :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupByKey f = M.fromListWith (<>) . fmap (\x -> (f x,[x]))


formTiming :: Maybe ServiceId -> Maybe StaffId -> Maybe LocalTime -> Form (ServiceId,StaffId,LocalTime)
formTiming sid eid time extra = do

    msgr <- getMessageRender

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
        } time

    let r = (,,) <$> serviceR <*> staffR <*> timeR
    let w = [whamlet|#{extra} ^{fvInput serviceV} ^{fvInput staffV} ^{fvInput timeV}|]

    return (r,w)


postBookStaffR :: Handler Html
postBookStaffR = do

    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )

    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    month <- fromMaybe today . (readMaybe . unpack =<<) <$> runInputGet ( iopt textField "month" )

    ((fr,fw),et) <- runFormPost $ formStaff sid eid
    case fr of
      FormSuccess (sid',eid') -> redirect ( BookTimingR month
                                          , [ ( "sid", pack $ show $ fromSqlKey sid')
                                            , ( "eid", pack $ show $ fromSqlKey eid')
                                            ]
                                          )
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              idFormStaff <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/staff/staff")
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
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

    msgr <- getMessageRender

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgService),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)

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
$if null opts
    <figure style="text-align:center">
      <span.on-secondary style="font-size:4rem">&varnothing;
      <figcaption.md-typescale-body-large>
        _{MsgNoEmployeesAvailableNow}.
$else
  <md-list ##{theId} *{attrs}>
    $forall (i,opt) <- opts
      $maybe (_,Entity _ (Staff ename _ _ _)) <- findStaff opt staff
        <md-list-item type=text onclick="this.querySelector('md-radio').click()">
          <div slot=headline>
            #{ename}
          <div slot=end>
            <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
              :sel x opt:checked touch-target=wrapper>
      <md-divider>
|]
              }


postBookServicesR :: Handler Html
postBookServicesR = do
    stati <- reqGetParams <$> getRequest

    idFormSearch <- newIdent

    ((fr0,fw0),et0) <- runFormGet $ formSearchServices idFormSearch

    let (bids,wids) = case fr0 of
          FormSuccess r -> r
          _otherwise -> (Just [],Just [])

    ((fr,fw),et) <- runFormPost $ formService bids wids Nothing

    case fr of
      FormSuccess sid -> redirect (BookStaffR, [("sid",pack $ show $ fromSqlKey sid)])
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormService <- newIdent
              idFabNext <- newIdent
              $(widgetFile "book/services")
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
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

    idFormSearch <- newIdent

    ((fr0,fw0),et0) <- runFormGet $ formSearchServices idFormSearch

    let (bids,wids) = case fr0 of
          FormSuccess r -> r
          _otherwise -> (Just [],Just [])

    (fw,et) <- generateFormPost $ formService bids wids sid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormService <- newIdent
        idFabNext <- newIdent
        $(widgetFile "book/services")


formSearchServices :: Text -> Form (Maybe [BusinessId],Maybe [WorkspaceId])
formSearchServices idFormSearch extra = do

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

    let r = (,) <$> bR <*> wR
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



formService :: Maybe [BusinessId] -> Maybe [WorkspaceId] -> Maybe ServiceId -> Form ServiceId
formService bids wids sid extra = do

    services <- liftHandler $ runDB $ select $ do
        x :& w :& b <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            
        where_ $ x ^. ServiceAvailable ==. val True
        
        case wids of
          Just xs@(_:_) -> where_ $ w ^. WorkspaceId `in_` valList xs
          _otherwise -> where_ $ val True

        case bids of
          Just xs@(_:_) -> where_ $ b ^. BusinessId `in_` valList xs
          _otherwise -> where_ $ val True

        orderBy [asc (x ^. ServiceName), asc (x ^. ServiceId)]
        return (x,(w,b))

    (serviceR,serviceV) <- mreq (md3radioFieldList services) "" sid

    return (serviceR,[whamlet|#{extra} ^{fvInput serviceV}|])

  where
      pairs services = (\(Entity sid' (Service _ name _ _ _ _ _),_) -> (name, sid')) <$> services

      md3radioFieldList :: [(Entity Service,(Entity Workspace, Entity Business))]
                        -> Field (HandlerFor App) ServiceId
      md3radioFieldList services = (radioField (optionsPairs (pairs services)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs services))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findService opt = find (\(Entity sid' _,_) -> sid' == optionInternalValue opt)

                [whamlet|
$if null opts
  <figure style="text-align:center">
    <span.on-secondary style="font-size:4rem">&varnothing;
    <figcaption.md-typescale-body-large>
      _{MsgNoServicesWereFoundForSearchTerms}.
$else
  <md-list ##{theId} *{attrs}>
    $forall (i,opt) <- opts
      $maybe (Entity _ (Service _ sname _ price _ _ _),(workspace,business)) <- findService opt services
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
