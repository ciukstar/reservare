{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Handler.Appointments
  ( getAppointmentStaffR, postAppointmentStaffR
  , getAppointmentTimingR, postAppointmentTimingR
  , getAppointmentTimeSlotsR, postAppointmentTimeSlotsR
  , getAppointmentPaymentR, postAppointmentPaymentR
  ) where

import qualified AtVenue.Data as V (Route(CheckoutR))

import Control.Monad (when, unless, forM_, join)

import Data.Bifunctor (Bifunctor(first,bimap, second))
import Data.Foldable (find)
import qualified Data.Map as M (member, Map, fromListWith, findWithDefault)
import Data.Maybe (fromMaybe, mapMaybe)
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
    ( select, from, table, orderBy, asc, innerJoin, leftJoin, on, in_
    , (^.), (?.), (==.), (:&)((:&))
    , toSqlKey, val, where_, selectOne, Value (unValue), between, valList
    , distinct, subSelectList, just, isNothing_, justList
    )
import Database.Persist (Entity (Entity), entityKey, insert)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, App, Widget, widgetSnackbar
    , Route
      ( AuthR, HomeR
      , AppointmentStaffR, AppointmentTimingR, AppointmentTimeSlotsR
      , AppointmentPaymentR, StripeR, YookassaR, AtVenueR, StaffPhotoR
      )
    , AppMessage
      ( MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgServices, MsgNext, MsgServices, MsgThereAreNoDataYet
      , MsgBack, MsgStaff, MsgAppointmentTime
      , MsgPaymentOption, MsgCancel, MsgService, MsgEmployee, MsgSelect
      , MsgSelectTime, MsgPaymentGatewayNotSpecified
      , MsgAppointmentSetFor, MsgSelectAvailableDayAndTimePlease
      , MsgEmployeeScheduleNotGeneratedYet, MsgNoEmployeesAvailableNow
      , MsgPrevious, MsgInvalidFormData, MsgServiceAssignment
      , MsgNoPaymentOptionSpecified, MsgWorkspaceWithoutPaymentOptions
      , MsgError, MsgEmployeeWorkScheduleForThisMonthNotSetYet, MsgWorkspaces
      , MsgSectors, MsgBusinesses, MsgRoles, MsgSelectAppointeePlease, MsgPhoto
      )
    )

import Model
    ( statusError, keyBacklink, keyBacklinkAuth
    , ServiceId, Service(Service)
    , WorkspaceId, Workspace (Workspace)
    , BusinessId, Business (Business)
    , AssignmentId, Assignment (Assignment)
    , StaffId, Staff (Staff)
    , PayMethod (PayNow, PayAtVenue)
    , Schedule (Schedule)
    , PayOptionId, PayOption (PayOption)
    , Book
      ( Book, bookService, bookStaff, bookAppointment, bookCustomer
      , bookCharge, bookCurrency
      )
    , PayGateway (PayGatewayStripe, PayGatewayYookassa)
    , SectorId, Sector (Sector)
    , StaffPhoto
    , EntityField
      ( ServiceWorkspace, WorkspaceId, WorkspaceBusiness
      , BusinessId, ServiceName, AssignmentStaff, StaffId, StaffName
      , BusinessName, WorkspaceName, ServiceId, AssignmentService
      , AssignmentSlotInterval, ScheduleAssignment, AssignmentId
      , ScheduleDay, PayOptionId, PayOptionWorkspace, AssignmentRole
      , ServiceAvailable, ServicePrice, WorkspaceCurrency, SectorParent
      , SectorName, ServiceType, StaffPhotoStaff, StaffPhotoAttribution
      )
    )

import Settings (widgetFile)
    
import qualified Stripe.Data as S (Route(CheckoutR))

import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Julius (rawJS, julius)
import Text.Read (readMaybe)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core.Handler
    ( getMessages, newIdent, addMessageI, getRequest, redirect, lookupGetParams
    , YesodRequest (reqGetParams), getMessageRender, setUltDestCurrent
    , setUltDest, setUltDestReferer
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


postAppointmentPaymentR :: Handler Html
postAppointmentPaymentR = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )
    oid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "oid" )

    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    let month = maybe today ((\(y,m,_) -> YearMonth y m) . toGregorian . utctDay) tid

    ((fr,fw),et) <- runFormPost $ formPayment aid eid sid tid oid

    user <- maybeAuth
    
    case (fr,user) of
      (FormSuccess (_,(eid',(sid',(tid',oid')))), Nothing) -> do
          setUltDest ( AppointmentPaymentR, [ ("sid", pack $ show $ fromSqlKey sid')
                                            , ("eid", pack $ show $ fromSqlKey eid')
                                            , ("tid", pack $ show (utcToLocalTime utc tid'))
                                            , ("oid", pack $ show oid')
                                            ]
                     )
          redirect $ AuthR LoginR
          
      (FormSuccess (aid',(eid',(sid',(tid',oid')))), Just (Entity uid _)) -> do

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
                let params = [ ("aid", pack $ show $ fromSqlKey aid')
                             , ("sid", pack $ show $ fromSqlKey sid')
                             , ("eid", pack $ show $ fromSqlKey eid')
                             , ("tid", pack $ show (utcToLocalTime utc tid'))
                             , ("oid", pack $ show $ fromSqlKey oid')
                             ]
                setUltDest (AppointmentPaymentR, params)
                redirect (StripeR $ S.CheckoutR bid oid', params)
                    
            (Just (charge,currency),Just (Entity _ (PayOption _ PayNow _ (Just PayGatewayYookassa) _ _))) -> do
                bid <- runDB $ insert $ Book { bookCustomer = uid
                                             , bookService = sid'
                                             , bookStaff = eid'
                                             , bookAppointment = tid'
                                             , bookCharge = charge
                                             , bookCurrency = currency
                                             }
                let params = [ ("aid", pack $ show $ fromSqlKey aid')
                             , ("sid", pack $ show $ fromSqlKey sid')
                             , ("eid", pack $ show $ fromSqlKey eid')
                             , ("tid", pack $ show (utcToLocalTime utc tid'))
                             , ("oid", pack $ show $ fromSqlKey oid')
                             ]
                setUltDest (AppointmentPaymentR, params)
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
                    $(widgetFile "appointments/payment/payment")
                
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
                    $(widgetFile "appointments/payment/payment")
          
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
              $(widgetFile "appointments/payment/payment")
              
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
              $(widgetFile "appointments/payment/payment")


getAppointmentPaymentR :: Handler Html
getAppointmentPaymentR = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- toSqlKey <$> runInputGet ( ireq intField "eid" )
    sid <- toSqlKey <$> runInputGet ( ireq intField "sid" )
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
              $(widgetFile "appointments/payment/error")
          
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
              $(widgetFile "appointments/payment/error")
          
      ([Entity oid' (PayOption _ PayAtVenue _ _ _ _)],Just (charge,currency),Just (Entity uid _)) -> do
          bid <- runDB $ insert $ Book { bookCustomer = uid
                                       , bookService = sid
                                       , bookStaff = eid
                                       , bookAppointment = tid
                                       , bookCharge = charge
                                       , bookCurrency = currency
                                       }
          redirect (AtVenueR $ V.CheckoutR bid oid')
          
      _otherwise -> do
          
          (fw,et) <- generateFormPost $ formPayment aid (Just eid) (Just sid) (Just tid) oid

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idHeader <- newIdent
              idMain <- newIdent
              idFormPayment <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "appointments/payment/payment")


formPayment :: Maybe AssignmentId -> Maybe StaffId -> Maybe ServiceId -> Maybe UTCTime -> Maybe PayOptionId
            -> Form (AssignmentId,(StaffId,(ServiceId,(UTCTime,PayOptionId))))
formPayment aid eid sid time option extra = do

    (assignmentR,assignmentV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgServiceAssignment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> aid)

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

    let r = (,) <$> assignmentR <*> ((,) <$> staffR <*> ((,) <$> serviceR <*> ((,) . localTimeToUTC utc <$> timeR <*> optionR)))
    let w = [whamlet|
                    #{extra}
                    ^{fvInput assignmentV}
                    ^{fvInput staffV}
                    ^{fvInput serviceV}
                    ^{fvInput timeV}
                    ^{fvInput optionV}
                    |]

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
                                 ol.list
                                     li

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
                              <input type=radio ##{theId}-#{i} name=#{name} value=#{optionExternalValue opt}
                                     :isReq:required=true :sel x opt:checked aria-label=#{optionDisplay opt}>
                              <span>
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
      FormSuccess (aid',(eid',(sid',tid'))) -> redirect
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
              idHeader <- newIdent
              idButtonBack <- newIdent
              idMain <- newIdent
              idFormSlots <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "appointments/timing/slots/slots")
              
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idHeader <- newIdent
              idButtonBack <- newIdent
              idMain <- newIdent
              idFormSlots <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
        idHeader <- newIdent
        idButtonBack <- newIdent
        idMain <- newIdent
        idFormSlots <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
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
             -> Form (AssignmentId,(StaffId,(ServiceId,LocalTime)))
formTimeSlot slots aid eid sid tid extra = do

    (assignmentR,assignmentV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgServiceAssignment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> aid)

    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> eid)

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)


    (slotsR,slotsV) <- mreq (md3radioFieldList slots) "" tid

    let r = (,) <$> assignmentR <*> ((,) <$> staffR <*> ((,) <$> serviceR <*> slotsR))
    let w = [whamlet|#{extra} ^{fvInput assignmentV} ^{fvInput staffV} ^{fvInput serviceV} ^{fvInput slotsV}|]

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


groupByKey :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupByKey f = M.fromListWith (<>) . fmap (\x -> (f x,[x]))


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
              idHeader <- newIdent
              idButtonBack <- newIdent
              idMain <- newIdent
              idFormTiming <- newIdent
              idCalendarPage <- newIdent
              idFabNext <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
              idHeader <- newIdent
              idButtonBack <- newIdent
              idMain <- newIdent
              idFormTiming <- newIdent
              idCalendarPage <- newIdent
              idFabNext <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
        idHeader <- newIdent
        idButtonBack <- newIdent
        idMain <- newIdent
        idFormTiming <- newIdent
        idCalendarPage <- newIdent
        idFabNext <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
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

    let inputSectors = filter (\(x,_) -> x == paramSector) stati
        selectedSectors = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputSectors

    let inputBusinesses = filter (\(x,_) -> x == paramBusiness) stati
        selectedBusinesses = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputBusinesses

    let inputWorkspaces = filter (\(x,_) -> x == paramWorkspace) stati
        selectedWorkspaces = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputWorkspaces

    let inputServices = filter (\(x,_) -> x == paramWorkspace) stati
        selectedServices = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputServices

    selectedRoles <- lookupGetParams paramRole

    ((fr,fw),et) <- runFormPost $ formAssignments 
               selectedSectors
               selectedBusinesses
               selectedWorkspaces
               selectedServices
               selectedRoles
               aid

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
              idHeader <- newIdent
              idMain <- newIdent
              idFormAssignment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "appointments/assignments")
              
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idHeader <- newIdent
              idMain <- newIdent
              idFormAssignment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "appointments/assignments")


getAppointmentStaffR :: Handler Html
getAppointmentStaffR = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )

    let inputSectors = filter (\(x,_) -> x == paramSector) stati
        selectedSectors = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputSectors

    let inputBusinesses = filter (\(x,_) -> x == paramBusiness) stati
        selectedBusinesses = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputBusinesses

    let inputWorkspaces = filter (\(x,_) -> x == paramWorkspace) stati
        selectedWorkspaces = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputWorkspaces

    let inputServices = filter (\(x,_) -> x == paramWorkspace) stati
        selectedServices = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputServices

    selectedRoles <- lookupGetParams paramRole

    (fw,et) <- generateFormPost $ formAssignments
               selectedSectors
               selectedBusinesses
               selectedWorkspaces
               selectedServices
               selectedRoles
               aid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        idHeader <- newIdent
        idMain <- newIdent
        idFormAssignment <- newIdent
        idFabNext <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "appointments/assignments")


formAssignments :: [SectorId] -> [BusinessId] -> [WorkspaceId] -> [ServiceId] -> [Text]
                -> Maybe AssignmentId -> Form (AssignmentId,(StaffId,ServiceId))
formAssignments tids bids wids sids roles aid extra = do

    assignments <- (second (join . unValue) <$>) <$> liftHandler ( runDB ( select $ do
        x :& e :& s :& w :& b :& f <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
            `innerJoin` table @Service `on` (\(x :& _ :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& _ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `leftJoin` table @StaffPhoto `on` (\(_ :& e :& _ :& _ :& _ :& f) -> just (e ^. StaffId) ==. f ?. StaffPhotoStaff)

        where_ $ s ^. ServiceAvailable ==. val True

        unless (null roles) $ where_ $ x ^. AssignmentRole `in_` valList roles
        unless (null sids) $ where_ $ s ^. ServiceId `in_` valList sids
        unless (null wids) $ where_ $ w ^. WorkspaceId `in_` valList wids
        unless (null bids) $ where_ $ b ^. BusinessId `in_` valList bids
        unless (null tids) $ where_ $ s ^. ServiceType `in_` justList (valList tids)
        
        orderBy [asc (e ^. StaffName), asc (e ^. StaffId)]
        return ((x,(e,(s,(w,b)))),f ?. StaffPhotoAttribution) ))

    let getDefault :: Maybe AssignmentId
                   -> [((Entity Assignment,(Entity Staff,(Entity Service,(Entity Workspace,Entity Business)))),Maybe Html)]
                   -> Maybe (AssignmentId,(StaffId,ServiceId))
        getDefault (Just aid') assignments' =
            ((\((Entity aid'' _,(Entity eid'' _,(Entity sid'' _,_))),_) -> (aid'',(eid'',sid''))) <$>)
               . find (\((Entity aid'' _,(_,_)),_) -> aid'' == aid') $ assignments'
        getDefault Nothing _ = Nothing

    (assigmentR,assigmentV) <- mreq (md3radioFieldList assignments) "" (getDefault aid assignments)

    let r = assigmentR
    let w = [whamlet|#{extra} ^{fvInput assigmentV}|]

    return (r,w)

  where
      
      pairs assignments = (\((Entity aid' _,(Entity eid' (Staff name _ _ _),(Entity sid' _,_))),_) -> (name, (aid',(eid',sid'))))
          <$> assignments

      md3radioFieldList :: [((Entity Assignment,(Entity Staff,(Entity Service,(Entity Workspace,Entity Business)))),Maybe Html)]
                        -> Field (HandlerFor App) (AssignmentId,(StaffId,ServiceId))
      md3radioFieldList assignments = (radioField' (optionsPairs (pairs assignments)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs assignments))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findAssigment opt = find (\((Entity aid' _,(_,_)),_) -> aid' == fst (optionInternalValue opt))

                classHeadline <- newIdent
                classSupportingText <- newIdent
                classCurrency <- newIdent
                classAttribution <- newIdent

                toWidget [julius|
                                document.querySelectorAll(
                                  '.#{rawJS classCurrency}[data-value][data-currency]'
                                ).forEach(function (x) {
                                  x.textContent = Intl.NumberFormat(navigator.language, {
                                    style: 'currency',
                                    currency: x.dataset.currency,
                                    minimumFractionDigits: 0,
                                    maximumFractionDigits: 2,
                                    useGrouping: true
                                  }).format(x.dataset.value / 100);
                                });
                |]
                toWidget [cassius|
                                 ol.list
                                     li

                                         div.max
                                             min-width: 0

                                             .#{classHeadline}, .#{classSupportingText}
                                                 white-space: nowrap
                                                 overflow: hidden
                                                 text-overflow: ellipsis
                                     li.none
                                         min-block-size: 0
                                         
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
      $maybe ((assignment,(Entity eid (Staff ename _ _ _),(srv,(workspace,business)))),attrib) <- findAssigment opt assignments
        <li.wave onclick="this.querySelector('label.radio').click()">
          <img.circle src=@{StaffPhotoR eid} loading=lazy alt=_{MsgPhoto}>

          <div.max>
            <div.#{classHeadline}.large-text>
              #{ename}
              
            <div.#{classSupportingText}.secondary-text>
              $with Entity _ (Assignment _ _ role _ _ _) <- assignment
                #{role}
              $with (Entity _ (Business _ bname _ _),Entity _ (Workspace _ wname _ _ _)) <- (business,workspace)
                \ (#{bname} - #{wname})
                
            $with (Entity _ (Workspace _ _ _ _ currency),Entity _ (Service _ name _ price _ _ _)) <- (workspace,srv)
              <div.#{classSupportingText}.secondary-text>
                #{name}
                
              <div.#{classSupportingText}.secondary-text>
                <span.#{classCurrency} data-value=#{price} data-currency=#{currency}>
                  #{show price} #{currency}
                  
          <label.radio>
            <input type=radio ##{theId}-#{i} name=#{name} value=#{optionExternalValue opt}
                   :sel x opt:checked :isReq:required=true aria-label=#{ename}>
              <span>
              
        $maybe attrib <- attrib
          <li.none>
            <div.#{classAttribution}>
              #{attrib}
      
|]
              }


widgetFilterChips :: Route App -> Widget
widgetFilterChips route = do

    stati <- reqGetParams <$> getRequest
    let inputSectors = filter (\(x,_) -> x == paramSector) stati
        selectedSectors = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputSectors

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
        selectedWorkspaces = let wids = (entityKey <$> workspaces) in
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
    
    services <- liftHandler $ runDB $ select $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceAvailable ==. val True
        unless (null workspaces) $ where_ $ x ^. ServiceWorkspace `in_` valList (entityKey <$> workspaces)
        when (null workspaces) $ where_ $ val False
        orderBy [asc (x ^. ServiceName)]
        return x

    let inputServices = filter (\(x,_) -> x == paramService) stati
        selectedServices = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputServices

    roles <- liftHandler $ (unValue <$>) <$> runDB ( select $ distinct $ do
        x <- from $ table @Assignment
        unless (null services) $ where_ $ x ^. AssignmentService `in_` valList (entityKey <$> services)
        when (null services) $ where_ $ val False
        orderBy [asc (x ^. AssignmentRole)]
        return $ x ^. AssignmentRole )

    let inputRoles = filter (\(x,_) -> x == paramRole) stati
        selectedRoles = snd <$> inputRoles

    idFilterChips <- newIdent
    idDetailsSectors <- newIdent
    classSummaryLabel <- newIdent
    classSummaryEndIcon <- newIdent
    idChipSetSectors <- newIdent
    idChipSetBusinesses <- newIdent
    idChipSetWorkspaces <- newIdent
    idChipSetServices <- newIdent
    idChipSetRoles <- newIdent
    
    $(widgetFile "appointments/widgets/chips")


paramRole :: Text
paramRole = "r"

paramService :: Text
paramService = "s"

paramSector :: Text
paramSector = "t"

paramBusiness :: Text
paramBusiness = "b"

paramWorkspace :: Text
paramWorkspace = "w"


keyScrollLeftX5 :: Text
keyScrollLeftX5 = "scrollLeftX5"

keyScrollLeftX4 :: Text
keyScrollLeftX4 = "scrollLeftX4"

keyScrollLeftX3 :: Text
keyScrollLeftX3 = "scrollLeftX3"

keyScrollLeftX2 :: Text
keyScrollLeftX2 = "scrollLeftX2"

keyScrollLeftX1 :: Text
keyScrollLeftX1 = "scrollLeftX1"

keyScrollTop3 :: Text
keyScrollTop3 = "scrollTop3"

keyScrollTop2 :: Text
keyScrollTop2 = "scrollTop2"

keyScrollTop :: Text
keyScrollTop = "scrollTop"
