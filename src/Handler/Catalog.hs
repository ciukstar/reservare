{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Catalog
  ( getCatalogR
  , getCatalogServiceR
  , getCatalogServiceBusinessR
  , getCatalogServicePhotoDefaultR
  , getStaffPhotoR
  , getCatalogServicePhotoR
  , getCatalogServiceAssignmentsR
  , getCatalogServiceAssignmentR
  , getCatalogStaffScheduleR
  , getCatalogStaffScheduleSlotsR
  , getCatalogBusinessLogoR
  ) where

import Control.Monad (unless, join)

import Data.Bifunctor (Bifunctor(second, first))
import qualified Data.Map as M
    ( Map, fromListWith, member, notMember, lookup ) 
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (unpack, pack)
import Data.Time.Calendar
    ( Day, toGregorian, DayPeriod (periodFirstDay, periodLastDay)
    , weekFirstDay, DayOfWeek (Monday), addDays
    )
import Data.Time.Calendar.Month (pattern YearMonth, Month, addMonths)
import Data.Time.Clock
    ( UTCTime(utctDay), nominalDiffTimeToSeconds, getCurrentTime
    )
import Data.Text.Encoding (encodeUtf8)
import Data.Time.LocalTime (diffLocalTime, LocalTime (LocalTime))
import Data.Time.Format (defaultTimeLocale, formatTime)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, in_
    , (^.), (?.), (==.), (:&) ((:&))
    , justList, valList, orderBy, desc, leftJoin, just, between
    , Value (unValue)
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Foundation
    ( Handler, widgetSnackbar
    , Route
      ( CatalogR, HomeR, StaticR, CatalogServicePhotoR, CatalogServiceR
      , CatalogServiceBusinessR, CatalogServiceAssignmentsR
      , CatalogServicePhotoDefaultR, CatalogServiceAssignmentR
      , CatalogBusinessLogoR, StaffPhotoR, CatalogStaffScheduleR
      , CatalogStaffScheduleSlotsR, BookStaffR, AppointmentTimingR, AppointmentTimeSlotsR
      )
    , AppMessage
      ( MsgCatalogue, MsgServiceCatalog, MsgNoServicesWereFoundForSearchTerms
      , MsgBack, MsgService, MsgDetails, MsgDescription, MsgTheName, MsgPrice
      , MsgPhoto, MsgServiceAssignments, MsgDuration, MsgBookNow, MsgBusiness
      , MsgAddress, MsgTimeZone, MsgCurrency, MsgOwner, MsgEmail, MsgRegistrant
      , MsgNoStaffAssignedYet, MsgServiceAssignment, MsgWorkSchedule
      , MsgWorkspace, MsgRole, MsgEmployee, MsgMakeAnAppointment
      , MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgSymbolHour, MsgSymbolMinute, MsgWorkingHours, MsgNoWorkScheduledForToday
      )
    )

import Handler.Booking
    ( widgetFilterChips, paramSector, paramBusiness, paramWorkspace, paramScrollY
    )

import Model
    ( keyBacklink
    , ServiceId, Service (Service)
    , Workspace (Workspace)
    , ServicePhotoId, ServicePhoto (ServicePhoto)
    , BusinessId, Business (Business)
    , User (User)
    , AssignmentId, Assignment (Assignment)
    , StaffId, Staff (Staff)
    , BusinessLogo (BusinessLogo)
    , StaffPhoto (StaffPhoto)
    , Schedule (Schedule)
    , EntityField
      ( ServiceAvailable, ServicePhotoService, ServiceWorkspace, WorkspaceId
      , ServiceType, WorkspaceBusiness, ServiceId, ServicePhotoId, BusinessId
      , BusinessOwner, UserId, AssignmentStaff, StaffId, AssignmentService
      , BusinessLogoBusiness, StaffPhotoStaff, BusinessLogoAttribution
      , AssignmentId, StaffPhotoAttribution, ScheduleAssignment
      , ScheduleDay, SectorId
      ), Sector (Sector)
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    , img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg
    , img_broken_image_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Printf (printf)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, lookupGetParams, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect
    , newIdent, lookupGetParam, YesodRequest (reqGetParams), getRequest
    , MonadIO (liftIO), getMessageRender
    )
import Yesod.Persist (YesodPersist(runDB))
import Text.Julius (RawJS(rawJS))


getCatalogStaffScheduleSlotsR :: ServiceId -> StaffId -> AssignmentId -> Day -> Handler Html
getCatalogStaffScheduleSlotsR sid eid aid day = do

    stati <- reqGetParams <$> getRequest
    
    slots <- runDB $ select $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleAssignment ==. val aid
        where_ $ x ^. ScheduleDay ==. val day
        return x

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours 
        idButtonMakeAnAppointment <- newIdent
        $(widgetFile "catalog/assignments/schedule/slots/slots")


getCatalogStaffScheduleR :: ServiceId -> AssignmentId -> Month -> Handler Html
getCatalogStaffScheduleR sid aid month = do

    today <- liftIO $ utctDay <$> getCurrentTime
    
    assignment <- ((second (join . unValue) <$>) <$>) $ runDB $ selectOne $ do
        x :& f <- from $ table @Assignment
            `leftJoin` table @StaffPhoto `on` (\(x :& f) -> just (x ^. AssignmentStaff) ==. f ?. StaffPhotoStaff)
        where_ $ x ^. AssignmentId ==. val aid
        return (x, f ?. StaffPhotoAttribution) 
    
    let mapper (Entity _ (Schedule _ day start end)) = (day,diffLocalTime (LocalTime day end) (LocalTime day start))
    
    workdays <- (groupByKey mapper <$>) $ runDB $ select $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleAssignment ==. val aid
        where_ $ x ^. ScheduleDay `between` (val $ periodFirstDay month, val $ periodLastDay month)
        return x

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkSchedule
        idHeader <- newIdent
        idMain <- newIdent
        idCalendarPage <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "catalog/assignments/schedule/hours")
  where
      
      groupByKey :: (Ord k, Num a) => (v -> (k,a)) -> [v] -> M.Map k a
      groupByKey key = M.fromListWith (+) . fmap key

      rest diff = mod (div (truncate @_ @Integer $ nominalDiffTimeToSeconds diff) 60) 60


getCatalogServiceAssignmentR :: ServiceId -> AssignmentId -> Handler Html
getCatalogServiceAssignmentR sid aid = do

    stati <- reqGetParams <$> getRequest
    
    assignment <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& s :& w :& b :& e :& f <- from $ table @Assignment
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @Staff `on` (\(x :& _ :& _ :& _ :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
            `leftJoin` table @StaffPhoto `on` (\(_ :& _ :& _ :& _ :& e :& f) -> just (e ^. StaffId) ==. f ?. StaffPhotoStaff)
        where_ $ x ^. AssignmentId ==. val aid
        return ((x,(e,(s,(w,b)))),f ?. StaffPhotoAttribution) )

    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment 
        idHeader <- newIdent
        idMain <- newIdent
        idButtonMakeAppointment <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "catalog/assignments/assignment")


getCatalogServiceAssignmentsR :: ServiceId -> Handler Html
getCatalogServiceAssignmentsR sid = do
    stati <- reqGetParams <$> getRequest

    assignments <- runDB $ select $ do
        x :& e <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        where_ $ x ^. AssignmentService ==. val sid
        return (x,e)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idHeader <- newIdent
        idMain <- newIdent
        classHeadline <- newIdent
        classSupportingText <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "common/css/rows")
        $(widgetFile "catalog/assignments/assignments")


getCatalogServiceBusinessR :: ServiceId -> Handler Html
getCatalogServiceBusinessR sid = do
    stati <- reqGetParams <$> getRequest

    workspace <- (first (second (second (join . unValue))) <$>) <$> runDB ( selectOne $ do
        x :& w :& b :& l :& o <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `leftJoin` table @BusinessLogo `on` (\(_ :& _ :& b :& l) -> just (b ^. BusinessId) ==. l ?. BusinessLogoBusiness)
            `innerJoin` table @User `on` (\(_ :& _ :& b :& _ :& o) -> b ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. ServiceId ==. val sid
        return ((w,(b,l ?. BusinessLogoAttribution)),o) )

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idHeader <- newIdent
        idMain <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "catalog/business")



getCatalogServiceR :: ServiceId -> Handler Html
getCatalogServiceR sid = do

    stati <- reqGetParams <$> getRequest

    service <- do
        service <- runDB $ selectOne $ do
            x :& w <- from $ table @Service
                `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            where_ $ x ^. ServiceId ==. val sid
            where_ $ x ^. ServiceAvailable ==. val True
            orderBy [desc (x ^. ServiceId)]
            return (x,w)
            
        photos <- runDB $ select $ do
            x <- from $ table @ServicePhoto
            where_ $ x ^. ServicePhotoService ==. val sid
            return x
            
        return (service,photos)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idHeader <- newIdent
        idMain <- newIdent
        classCurrency <- newIdent
        classDuration <- newIdent        
        $(widgetFile "common/js/seconds2duration")
        $(widgetFile "common/css/header")
        $(widgetFile "catalog/service")


getCatalogR :: Handler Html
getCatalogR = do
    stati <- reqGetParams <$> getRequest
    scrollY <- lookupGetParam paramScrollY
    paramSid <- lookupGetParam "sid"

    selectedSectors <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramSector
    selectedBusinesses <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramBusiness
    selectedWorkspaces <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramWorkspace

    services <- runDB $ select $ do
        x :& w :& g <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `leftJoin` table @Sector `on` (\(x :& _ :& g) -> just (x ^. ServiceType) ==. just (g ?. SectorId))
        where_ $ x ^. ServiceAvailable ==. val True
        unless (null selectedSectors) $ where_ $ x ^. ServiceType `in_` justList (valList selectedSectors)
        unless (null selectedBusinesses) $ where_ $ w ^. WorkspaceBusiness `in_` valList selectedBusinesses
        unless (null selectedWorkspaces) $ where_ $ w ^. WorkspaceId `in_` valList selectedWorkspaces
        orderBy [desc (x ^. ServiceId)]
        return ((x,w),g)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceCatalog
        idHeader <- newIdent
        idMain <- newIdent
        classHeadline <- newIdent
        classSupportingText <- newIdent
        classCurrency <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "common/css/rows")
        $(widgetFile "catalog/services")


getStaffPhotoR :: StaffId -> Handler TypedContent
getStaffPhotoR eid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @StaffPhoto
        where_ $ x ^. StaffPhotoStaff ==. val eid
        return x
    case photo of
      Just (Entity _ (StaffPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg


getCatalogServicePhotoDefaultR :: ServiceId -> Handler TypedContent
getCatalogServicePhotoDefaultR sid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoService ==. val sid
        return x
    case photo of
      Just (Entity _ (ServicePhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> do
          logo <- runDB $ selectOne $ do
              x :& _ :& l <- from $ table @Service
                  `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
                  `innerJoin` table @BusinessLogo `on` (\(_ :& w :& l) -> w ^. WorkspaceBusiness ==. l ^. BusinessLogoBusiness)
              where_ $ x ^. ServiceId ==. val sid
              return l
          case logo of
            Just (Entity _ (BusinessLogo _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
            Nothing -> redirect $ StaticR img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg


getCatalogServicePhotoR :: ServiceId -> ServicePhotoId -> Handler TypedContent
getCatalogServicePhotoR sid fid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        where_ $ x ^. ServicePhotoService ==. val sid
        return x
    case photo of
      Just (Entity _ (ServicePhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg


getCatalogBusinessLogoR :: BusinessId -> Handler TypedContent
getCatalogBusinessLogoR bid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @BusinessLogo
        where_ $ x ^. BusinessLogoBusiness ==. val bid
        return x
    case photo of
      Just (Entity _ (BusinessLogo _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_broken_image_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
