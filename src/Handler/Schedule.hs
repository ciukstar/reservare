{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Handler.Schedule
  ( getUserScheduleR
  , getUserCalendarR
  ) where


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
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, Widget, widgetSnackbar
    , Route
      ( HomeR, UserCalendarR
      , StaffPhotoR, DataR, UserScheduleR, BookServicesR
      )
    , DataR (ServicePhotoDefaultR)
    , AppMessage
      ( MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgServices, MsgNext, MsgServices, MsgThereAreNoDataYet, MsgClose
      , MsgBack, MsgStaff, MsgPaymentOption, MsgCancel
      , MsgPaymentStatus, MsgReturnToHomePage, MsgService, MsgEmployee
      , MsgSelect, MsgSelectTime, MsgPrevious
      , MsgSelectAvailableDayAndTimePlease, MsgEmployeeScheduleNotGeneratedYet
      , MsgNoEmployeesAvailableNow, MsgNoServicesWereFoundForSearchTerms
      , MsgInvalidFormData, MsgEmployeeWorkScheduleForThisMonthNotSetYet
      , MsgPrice, MsgMobile, MsgPhone, MsgLocation
      , MsgFullName, MsgTheAppointment, MsgTheName, MsgDuration
      , MsgPaymentGatewayNotSpecified, MsgNoPaymentsHaveBeenMadeYet
      , MsgPayments, MsgTotalCharge, MsgError, MsgNoPaymentOptionSpecified
      , MsgWorkspaceWithoutPaymentOptions, MsgCalendar, MsgAppointments
      , MsgSectors, MsgSelectServiceToBookPlease, MsgPhoto, MsgMySchedule
      , MsgMyReservationsAndAppointments, MsgYouHaveNotBookedAnyServicesYet
      , MsgBookAService, MsgAdd, MsgHome
      )
    )

import Model
    ( statusError, keyPrevServices, keyBacklinkAuth
    , ServiceId, Service(Service)
    , WorkspaceId, Workspace (Workspace)
    , BusinessId, Business (Business)
    , Assignment (Assignment)
    , StaffId, Staff (Staff)
    , PayMethod (PayNow, PayAtVenue)
    , Schedule (Schedule)
    , BookId, UserId
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
      , WorkspaceName, StaffPhotoStaff, BookCustomer
      )
    )

import Settings ( widgetFile )

import Text.Julius (RawJS (rawJS))
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
import Yesod.Persist.Core (YesodPersist(runDB))


getUserCalendarR :: UserId -> Month -> Handler Html
getUserCalendarR uid month = do
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMyReservationsAndAppointments 
        idHeader <- newIdent
        idMain <- newIdent
        classHeadline <- newIdent
        classSupportingText <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "common/css/rows")
        $(widgetFile "schedule/calendar/calendar")


getUserScheduleR :: UserId -> Handler Html
getUserScheduleR uid = do
    
    books <- runDB $ select $ do
        x :& s :& w :& e <- from $ table @Book
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. BookService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Staff `on` (\(x :& _ :& _ :& e) -> x ^. BookStaff ==. e ^. StaffId)
        where_ $ x ^. BookCustomer ==. val uid
        return (x,(s,(w,e)))

    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime

    setUltDestCurrent
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMyReservationsAndAppointments 
        idHeader <- newIdent
        idMain <- newIdent
        idButtonBookService <- newIdent
        classHeadline <- newIdent
        classSupportingText <- newIdent
        classDaytime <- newIdent
        classCurrency <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "common/css/rows")
        $(widgetFile "schedule/schedule")
