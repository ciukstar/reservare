{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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
  , widgetFilterChips
  , paramSector, paramBusiness, paramWorkspace
  ) where

import qualified AtVenue.Data as V (Route(CheckoutR))

import Control.Monad (unless, forM_, when, join)

import Data.Bifunctor (Bifunctor(first,bimap, second))
import Data.Foldable (find)
import qualified Data.Map as M
    ( member, Map, fromListWith, findWithDefault, fromList, insert, toList
    )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (pack, unpack, Text)
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
    , (^.), (?.), (==.), (:&)((:&))
    , toSqlKey, val, where_, selectOne, Value (unValue), between
    , valList, just, subSelectList, justList, isNothing_, leftJoin
    )
import Database.Persist (Entity (Entity), entityKey, insert)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, App, Widget, widgetSnackbar
    , Route
      ( HomeR, BookServicesR, BookStaffR, BookTimingR, BookTimeSlotsR
      , BookPaymentR, AuthR, StripeR, YookassaR, AtVenueR, StaffPhotoR
      , CatalogServicePhotoDefaultR
      )
    , AppMessage
      ( MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgServices, MsgNext, MsgServices, MsgThereAreNoDataYet, MsgClose
      , MsgBack, MsgStaff, MsgAppointmentTime, MsgPaymentOption, MsgCancel
      , MsgPaymentStatus, MsgReturnToHomePage, MsgService, MsgEmployee
      , MsgSelect, MsgSelectTime, MsgAppointmentSetFor, MsgPrevious
      , MsgSelectAvailableDayAndTimePlease, MsgEmployeeScheduleNotGeneratedYet
      , MsgNoEmployeesAvailableNow, MsgNoServicesWereFoundForSearchTerms
      , MsgInvalidFormData, MsgEmployeeWorkScheduleForThisMonthNotSetYet
      , MsgBookingDetails, MsgPrice, MsgMobile, MsgPhone, MsgLocation
      , MsgAddress, MsgFullName, MsgTheAppointment, MsgTheName, MsgDuration
      , MsgPaymentGatewayNotSpecified, MsgNoPaymentsHaveBeenMadeYet
      , MsgPayments, MsgTotalCharge, MsgError, MsgNoPaymentOptionSpecified
      , MsgWorkspaceWithoutPaymentOptions, MsgBusinesses, MsgWorkspaces
      , MsgSectors, MsgSelectServiceToBookPlease, MsgPhoto
      )
    )

import Model
    ( statusError, keyBacklink, keyBacklinkAuth
    , ServiceId, Service(Service)
    , WorkspaceId, Workspace (Workspace)
    , BusinessId, Business (Business)
    , Assignment (Assignment)
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
    , SectorId, Sector (Sector)
    , StaffPhoto
    , EntityField
      ( ServiceWorkspace, WorkspaceId, WorkspaceBusiness, BusinessId
      , ServiceName, AssignmentStaff, StaffId, StaffName, ServiceId
      , AssignmentService, AssignmentSlotInterval, ScheduleAssignment
      , AssignmentId, ScheduleDay, BookId, BookService, BookStaff
      , ServiceAvailable, PayOptionWorkspace, PayOptionId, ServicePrice
      , WorkspaceCurrency, PaymentBook, PaymentOption, ServiceType
      , SectorParent, SectorName, BusinessName, StaffPhotoAttribution
      , WorkspaceName, StaffPhotoStaff
      )
    )

import Settings ( widgetFile )

import qualified Stripe.Data as S (Route(CheckoutR))

import Text.Cassius (cassius)
import Text.Julius (julius, RawJS (rawJS))
import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core.Handler
    ( getMessages, newIdent, addMessageI, getRequest
    , YesodRequest (reqGetParams), redirect, getMessageRender
    , setUltDestCurrent, setUltDest, setUltDestReferer
    )
import Yesod.Core
    ( Yesod(defaultLayout), MonadIO (liftIO), whamlet
    , MonadHandler (liftHandler), handlerToWidget
    , SomeMessage (SomeMessage), ToWidget (toWidget)
    )
import Yesod.Core.Types (HandlerFor)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( FieldView(fvInput), Field (fieldView)
    , FormResult (FormSuccess, FormFailure, FormMissing)
    , FieldSettings (fsLabel, fsTooltip, fsId, fsName, fsAttrs, FieldSettings)
    , Option (optionDisplay)
    )
import Yesod.Form.Input (runInputGet, iopt, ireq)
import Yesod.Form.Fields
    ( textField, intField, radioField', optionsPairs, OptionList (olOptions)
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
        idHeader <- newIdent
        idMain <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
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
      (FormSuccess (_sid',_eid',_tid',oid'), Nothing) -> do
          setUltDest ( BookPaymentR, ("oid", pack $ show $ fromSqlKey oid') : stati
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
                let params = listUpsert [ ("sid", pack $ show $ fromSqlKey sid')
                                        , ("eid", pack $ show $ fromSqlKey eid')
                                        , ("tid", pack $ show (utcToLocalTime utc tid'))
                                        , ("oid", pack $ show $ fromSqlKey oid')
                                        ] stati
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
                    idHeader <- newIdent
                    idMain <- newIdent
                    idFormPayment <- newIdent
                    $(widgetFile "common/css/header")
                    $(widgetFile "common/css/main")
                    $(widgetFile "book/payment/payment")

            _otherwise -> do
                addMessageI statusError MsgInvalidFormData
                msgs <- getMessages
                defaultLayout $ do
                    setTitleI MsgPaymentOption
                    idHeader <- newIdent
                    idMain <- newIdent
                    idFormPayment <- newIdent
                    $(widgetFile "common/css/header")
                    $(widgetFile "common/css/main")
                    $(widgetFile "book/payment/payment")

      (FormFailure errs, _) -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idHeader <- newIdent
              idMain <- newIdent
              idFormPayment <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "book/payment/payment")

      (FormMissing, _) -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idHeader <- newIdent
              idMain <- newIdent
              idFormPayment <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
              idHeader <- newIdent
              idMain <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
              idHeader <- newIdent
              idMain <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
              idHeader <- newIdent
              idMain <- newIdent
              idFormPayment <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "book/payment/payment")


formPayment :: Maybe ServiceId -> Maybe StaffId -> Maybe UTCTime -> Maybe PayOptionId
            -> Form (ServiceId,StaffId,UTCTime,PayOptionId)
formPayment sid eid time option extra = do

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)

    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> eid)

    (timeR,timeV) <- mreq datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAppointmentTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
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

    (optionR,optionV) <- mreq (md3radioFieldList options) "" option

    let r = (,,,) <$> serviceR <*> staffR <*> (localTimeToUTC utc <$> timeR) <*> optionR
    let w = [whamlet|#{extra} ^{fvInput serviceV} ^{fvInput staffV} ^{fvInput timeV} ^{fvInput optionV}|]

    return (r,w)

  where

      pairs (Entity oid' (PayOption _ _ name _ _ _)) = (name, oid')

      md3radioFieldList :: [Entity PayOption] -> Field (HandlerFor App) PayOptionId
      md3radioFieldList options = (radioField' (optionsPairs (pairs <$> options)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs <$> options))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findOption opt = find (\(Entity oid _) -> oid == optionInternalValue opt)

                classHeadline <- newIdent
                classSupportingText <- newIdent
     
                toWidget [cassius|
                                 ol.list li

                                      div.max
                                          min-width: 0

                                          .#{classHeadline}, .#{classSupportingText}
                                              white-space: nowrap
                                              overflow: hidden
                                              text-overflow: ellipsis
                                 |]
                [whamlet|
                    <ol.list.border ##{theId} *{attrs}>
                      $forall (i,opt) <- opts
                        $maybe Entity _ (PayOption _ _ oname _ descr icon) <- findOption opt options
                          <li.wave onclick="this.querySelector('label.radio').click()">
                          
                            $maybe icon <- icon
                              <i.extra>#{icon}
                              
                            <div.max>
                              <div.#{classHeadline}.large-text>
                                #{oname}
                              $maybe descr <- descr
                                <div.#{classSupportingText}.secondary-text>
                                  #{descr}
                                  
                            <label.radio>
                              <input type=radio ##{theId}-#{i} name=#{name}  value=#{optionExternalValue opt}
                                     :isReq:required=true :sel x opt:checked aria-label=#{oname}>
                              <span>
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
      FormSuccess (_sid',_eid',tid') -> redirect
          ( BookTimingR month
          , ("tid", pack $ show tid') : stati
          )
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idHeader <- newIdent
              idMain <- newIdent
              idFormSlots <- newIdent
              idFabSelect <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "book/timing/slots/slots")
              
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idHeader <- newIdent
              idMain <- newIdent
              idFormSlots <- newIdent
              idFabSelect <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
        idHeader <- newIdent
        idMain <- newIdent
        idFormSlots <- newIdent
        idFabSelect <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
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

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)

    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> eid)


    (slotsR,slotsV) <- mreq (md3radioFieldList slots) "" tid

    let r = (,,) <$> serviceR <*> staffR <*> slotsR
    let w = [whamlet|#{extra} ^{fvInput serviceV} ^{fvInput staffV} ^{fvInput slotsV}|]

    return (r,w)

  where
      pairs xs = (\x -> (pack $ show x, x)) <$> xs

      md3radioFieldList :: [LocalTime] -> Field (HandlerFor App) LocalTime
      md3radioFieldList items = (radioField' (optionsPairs . pairs $ items))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs . pairs $ items)

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                [whamlet|
                    <ol.list.border ##{theId} *{attrs}>
                      $forall (i,opt) <- opts
                        <li.wave onclick="this.querySelector('label.radio').click()">
                          $with dt <- optionDisplay opt
                            <time.time-slot.large-text.max datetime=#{dt}>
                              #{optionDisplay opt}
                              
                          <label.radio>
                            <input type=radio ##{theId}-#{i} name=#{name} value=#{optionExternalValue opt}
                                   :isReq:required=true :sel x opt:checked aria-label=#{optionDisplay opt}>
                            <span>
                        
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
      FormSuccess (_sid',_eid',tid') -> redirect ( BookPaymentR
                                                 , ("tid", pack $ show tid') : stati
                                                 )
      FormFailure errs -> do

          let start = weekFirstDay Monday (periodFirstDay month)
          let end = addDays 41 start
          let page = [start .. end]
          let next = addMonths 1 month
          let prev = addMonths (-1) month

          today <- utctDay <$> liftIO getCurrentTime

          slots <- (groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$>) $ runDB $ select $ do
              x :& a <- from $ table @Schedule
                  `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
              case sid of
                Just service -> where_ $ a ^. AssignmentService ==. val service
                Nothing -> where_ $ val False
              case eid of
                Just employee -> where_ $ a ^. AssignmentStaff ==. val employee
                Nothing -> where_ $ val False
              where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
              return x

          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idHeader <- newIdent
              idMain <- newIdent
              idFormTiming <- newIdent
              idCalendarPage <- newIdent
              idFabNext <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "book/timing/timing")

      FormMissing -> do

          let start = weekFirstDay Monday (periodFirstDay month)
          let end = addDays 41 start
          let page = [start .. end]
          let next = addMonths 1 month
          let prev = addMonths (-1) month

          today <- utctDay <$> liftIO getCurrentTime

          slots <- (groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$>) $ runDB $ select $ do
              x :& a <- from $ table @Schedule
                  `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
              case sid of
                Just service -> where_ $ a ^. AssignmentService ==. val service
                Nothing -> where_ $ val False
              case eid of
                Just employee -> where_ $ a ^. AssignmentStaff ==. val employee
                Nothing -> where_ $ val False
              where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
              return x

          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idHeader <- newIdent
              idMain <- newIdent
              idFormTiming <- newIdent
              idCalendarPage <- newIdent
              idFabNext <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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

    slots <- (groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$>) $ runDB $ select $ do
        x :& a <- from $ table @Schedule
            `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
        case sid of
          Just service -> where_ $ a ^. AssignmentService ==. val service
          Nothing -> where_ $ val False
        case eid of
          Just employee -> where_ $ a ^. AssignmentStaff ==. val employee
          Nothing -> where_ $ val False
        where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
        return x 

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idHeader <- newIdent
        idMain <- newIdent
        idFormTiming <- newIdent
        idCalendarPage <- newIdent
        idFabNext <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
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
      FormSuccess (_sid',eid') -> redirect ( BookTimingR month
                                           , ("eid", pack $ show $ fromSqlKey eid') : stati
                                           )
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              idHeader <- newIdent
              idMain <- newIdent
              idFormStaff <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "book/staff/staff")
              
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              idHeader <- newIdent
              idMain <- newIdent
              idFormStaff <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
        idHeader <- newIdent
        idMain <- newIdent
        idFormStaff <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "book/staff/staff")


formStaff :: Maybe ServiceId -> Maybe StaffId -> Form (ServiceId,StaffId)
formStaff sid eid extra = do

    staff <- (second (join . unValue) <$>) <$> liftHandler ( runDB ( select $ do
        x :& e :& f <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
            `leftJoin` table @StaffPhoto `on` (\(_ :& e :& f) -> just (e ^. StaffId) ==. f ?. StaffPhotoStaff)
        case sid of
          Just y -> where_ $ x ^. AssignmentService ==. val y
          Nothing -> where_ $ val False
        orderBy [asc (e ^. StaffName), asc (e ^. StaffId)]
        return ((x,e),f ?. StaffPhotoAttribution) ))

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
      pairs staff = (\((_,Entity eid' (Staff name _ _ _)),_) -> (name, eid')) <$> staff

      md3radioFieldList :: [((Entity Assignment,Entity Staff),Maybe Html)]
                        -> Field (HandlerFor App) StaffId
      md3radioFieldList staff = (radioField' (optionsPairs (pairs staff)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs staff))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findStaff opt = find (\((_,Entity eid' _),_) -> eid' == optionInternalValue opt)

                classAttribution <- newIdent
                classHeadline <- newIdent
                classSupportingText <- newIdent
                
                toWidget [cassius|
                                 ol.list li

                                     div.max
                                         min-width: 0

                                         .#{classHeadline}, .#{classSupportingText}
                                             white-space: nowrap
                                             overflow: hidden
                                             text-overflow: ellipsis

                                 .#{classAttribution}
                                     position: absolute
                                     bottom: 0
                                     left: 0.4rem
                                     line-height: 1
                                     font-size: 0.5rem
                                 |]
                [whamlet|
$if null opts
  <figure.center-align>
    <i.extra.secondary-text>folder_open
    <figcaption.large-text>
      _{MsgNoEmployeesAvailableNow}
        
$else
  <ol.list.border ##{theId} *{attrs}>
    $forall (i,opt) <- opts
      $maybe ((Entity _ (Assignment _ _ role _ _ _),Entity eid (Staff ename _ _ _)),attrib) <- findStaff opt staff
        <li.wave onclick="this.querySelector('label.radio').click()">
          <img.circle src=@{StaffPhotoR eid} loading=lazy alt=_{MsgPhoto}>
          
          <div.max>
            <div.#{classHeadline}.large-text>
              #{ename}
            <div.#{classSupportingText}.secondary-text>
              #{role}
              
          <label.radio>
            <input type=radio ##{theId}-#{i} name=#{name} value=#{optionExternalValue opt}
                   :isReq:required=true :sel x opt:checked aria-label=#{ename}>
            <span>
              
        $maybe attrib <- attrib
          <div.#{classAttribution}>
            #{attrib}
      
|]
              }


listUpsert :: Ord k => [(k,a)] -> [(k,a)] -> [(k,a)]
listUpsert [] xs = xs
listUpsert ((k,a):ks) xs = listUpsert ks (M.toList (M.insert k a (M.fromList xs)))


postBookServicesR :: Handler Html
postBookServicesR = do
    stati <- reqGetParams <$> getRequest

    let inputSectors = filter (\(x,_) -> x == paramSector) stati
        selectedSectors = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputSectors

    let inputBusinesses = filter (\(x,_) -> x == paramBusiness) stati
        selectedBusinesses = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputBusinesses

    let inputWorkspaces = filter (\(x,_) -> x == paramWorkspace) stati
        selectedWorkspaces = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputWorkspaces

    ((fr,fw),et) <- runFormPost $ formService
        selectedSectors
        selectedBusinesses
        selectedWorkspaces
        Nothing

    case fr of
      FormSuccess sid -> redirect ( BookStaffR
                                  , ("sid",pack $ show $ fromSqlKey sid) : filter ((/= "sid") . fst) stati
                                  )
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idHeader <- newIdent
              idMain <- newIdent
              idFormService <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "book/services")
              
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idHeader <- newIdent
              idMain <- newIdent
              idFormService <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "book/services")


getBookServicesR :: Handler Html
getBookServicesR = do
    stati <- reqGetParams <$> getRequest
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )

    let inputSectors = filter (\(x,_) -> x == paramSector) stati
        selectedSectors = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputSectors

    let inputBusinesses = filter (\(x,_) -> x == paramBusiness) stati
        selectedBusinesses = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputBusinesses

    let inputWorkspaces = filter (\(x,_) -> x == paramWorkspace) stati
        selectedWorkspaces = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputWorkspaces

    (fw,et) <- generateFormPost $ formService
        selectedSectors
        selectedBusinesses
        selectedWorkspaces
        sid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idHeader <- newIdent
        idMain <- newIdent
        idFormService <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "book/services")


formService :: [SectorId] -> [BusinessId] -> [WorkspaceId] -> Maybe ServiceId -> Form ServiceId
formService tids bids wids sid extra = do

    services <- liftHandler $ runDB $ select $ do
        x :& w :& b <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)

        where_ $ x ^. ServiceAvailable ==. val True
        unless (null tids) $ where_ $ x ^. ServiceType `in_` justList (valList tids)
        unless (null bids) $ where_ $ b ^. BusinessId `in_` valList bids
        unless (null wids) $ where_ $ w ^. WorkspaceId `in_` valList wids

        orderBy [asc (x ^. ServiceName), asc (x ^. ServiceId)]
        return (x,(w,b))

    (serviceR,serviceV) <- mreq (md3radioFieldList services) "" sid

    return (serviceR,[whamlet|#{extra} ^{fvInput serviceV}|])

  where
      pairs services = (\(Entity sid' (Service _ name _ _ _ _ _),_) -> (name, sid')) <$> services

      md3radioFieldList :: [(Entity Service,(Entity Workspace, Entity Business))]
                        -> Field (HandlerFor App) ServiceId
      md3radioFieldList services = (radioField' (optionsPairs (pairs services)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs services))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findService opt = find (\(Entity sid' _,_) -> sid' == optionInternalValue opt)

                classHeadline <- newIdent
                classSupportingText <- newIdent
                classCurrency <- newIdent
     
                toWidget [cassius|
                                 ol.list li

                                      div.max
                                          min-width: 0

                                          .#{classHeadline}, .#{classSupportingText}
                                              white-space: nowrap
                                              overflow: hidden
                                              text-overflow: ellipsis
                                 |]
                toWidget [julius|
                    document.querySelectorAll('.#{rawJS classCurrency}[data-value][data-currency]').forEach(function (x) {
                      x.textContent = Intl.NumberFormat(navigator.language, {
                        style: 'currency',
                        currency: x.dataset.currency,
                        minimumFractionDigits: 0,
                        maximumFractionDigits: 2,
                        useGrouping: true
                      }).format(x.dataset.value / 100);
                    });
                |]
                [whamlet|
$if null opts
  <figure.center-align>
    <i.extra.secondary-text>folder_open
    <figcaption.large-text>
      _{MsgNoServicesWereFoundForSearchTerms}
$else
  <ol.list.border ##{theId} *{attrs}>
    $forall (i,opt) <- opts
      $maybe (Entity sid (Service _ sname _ price _ _ _),(workspace,business)) <- findService opt services
        <li.wave onclick="this.querySelector('label.radio').click()">
          <img.circle src=@{CatalogServicePhotoDefaultR sid} loading=lazy alt=_{MsgPhoto}>
          <div.max>
            <div.#{classHeadline}.large-text>
              #{sname}
              
            $with (Entity _ (Workspace _ wname _ _ currency),Entity _ (Business _ bname _ _)) <- (workspace,business)
              <div.#{classSupportingText}.secondary-text>
                #{wname} (#{bname})
              <div.#{classCurrency}.#{classSupportingText}.secondary-text data-value=#{price} data-currency=#{currency}>
                #{currency}#{price}
                
          <label.radio>
            <input type=radio ##{theId}-#{i} name=#{name} value=#{optionExternalValue opt}
                   :isReq:required=true :sel x opt:checked>
            <span>
|]
              }



widgetFilterChips :: Route App -> Widget
widgetFilterChips route = do

    stati <- reqGetParams <$> getRequest
    let inputSectors = filter (\(x,_) -> x == paramSector) stati
    let selectedSectors = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputSectors

    sectors <- liftHandler $ runDB $ select $ do
        x <- from $ table @Sector
        where_ $ isNothing_ $ x ^. SectorParent
        orderBy [asc (x ^. SectorName)]
        return x

    let inputBusinesses = filter (\(x,_) -> x == paramBusiness) stati
    let selectedBusinesses = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputBusinesses

    businesses <- liftHandler $ runDB $ select $ do
        x <- from $ table @Business
        orderBy [asc (x ^. BusinessName)]
        return x

    workspaces <- liftHandler $ runDB $ select $ do
        x <- from $ table @Workspace
        unless (null selectedBusinesses) $ where_ $ x ^. WorkspaceBusiness `in_` valList selectedBusinesses
        when (null selectedBusinesses) $ where_ $ val False
        orderBy [asc (x ^. WorkspaceName)]
        return x

    let inputWorkspaces = filter (\(x,_) -> x == paramWorkspace) stati
    let selectedWorkspaces = let wids = (entityKey <$> workspaces) in
          filter (`elem` wids) $ mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputWorkspaces

    let toMap = M.fromListWith (<>) . (second (:[]) <$>)

    businessWorkspaces <- liftHandler $ toMap . (bimap unValue unValue <$>) <$> runDB ( select $ do
        x :& b <- from $ table @Workspace
            `innerJoin` table @Business `on` (\(x :& b) -> x ^. WorkspaceBusiness ==. b ^. BusinessId)
        unless (null selectedBusinesses) $ where_ $ x ^. WorkspaceBusiness `in_` valList selectedBusinesses
        when (null selectedBusinesses) $ where_ $ val False
        return (b ^. BusinessId,x ^. WorkspaceId) )

    let paramsWokspaces bid = (\x -> (paramWorkspace,pack $ show $ fromSqlKey x))
            <$> filter (\x -> x `notElem` M.findWithDefault [] bid businessWorkspaces) selectedWorkspaces

    idFilterChips <- newIdent
    idDetailsSectors <- newIdent
    idChipSetSectors <- newIdent
    idChipSetBusinesses <- newIdent
    idChipSetWorkspaces <- newIdent
    classSummaryLabel <- newIdent
    classSummaryEndIcon <- newIdent
    $(widgetFile "book/widgets/chips")


paramSector :: Text
paramSector = "t"

paramBusiness :: Text
paramBusiness = "b"

paramWorkspace :: Text
paramWorkspace = "w"


keyScrollLeftX3 :: Text
keyScrollLeftX3 = "scrollLeftX3"

keyScrollLeftX2 :: Text
keyScrollLeftX2 = "scrollLeftX2"

keyScrollLeftX1 :: Text
keyScrollLeftX1 = "scrollLeftX1"


keyScrollTop5 :: Text
keyScrollTop5 = "scrollTop5"

keyScrollTop4 :: Text
keyScrollTop4 = "scrollTop4"

keyScrollTop3 :: Text
keyScrollTop3 = "scrollTop3"

keyScrollTop2 :: Text
keyScrollTop2 = "scrollTop2"

keyScrollTop :: Text
keyScrollTop = "scrollTop"
