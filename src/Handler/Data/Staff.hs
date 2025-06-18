{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Data.Staff
  ( getStaffR
  , getEmployeeR
  , getEmployeeNewR
  , postStaffR
  , getEmployeeEditR
  , postEmployeeDeleR
  , postEmployeeR
  , getEmployeePhotoR
  , getStaffAssignmentsR
  , postStaffAssignmentsR
  , getStaffAssignmentR
  , postStaffAssignmentR
  , getStaffAssignmentNewR
  , getStaffAssignmentEditR
  , postStaffAssignmentDeleR
  , getStaffScheduleR
  , getStaffScheduleSlotsR
  , postStaffScheduleSlotsR
  , getStaffScheduleSlotR
  , postStaffScheduleSlotR
  , getStaffScheduleSlotNewR
  , getStaffScheduleSlotEditR
  , postStaffScheduleSlotDeleR
  , postStaffScheduleFillFromWorkingHoursR
  , postStaffScheduleFillFromPreviousMonthR
  ) where

import Control.Monad (void, forM_, join)

import Data.Bifunctor (Bifunctor(second, bimap))
import qualified Data.Map as M (Map, fromListWith, member, notMember, lookup)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
    ( nominalDiffTimeToSeconds, secondsToNominalDiffTime
    , DayPeriod (periodFirstDay, periodLastDay), weekFirstDay
    , DayOfWeek (Monday), addDays, defaultTimeLocale, getCurrentTime
    , UTCTime (utctDay), fromGregorian
    )
import Data.Time.LocalTime
    ( utc, utcToLocalTime, localTimeToUTC, diffLocalTime, LocalTime (LocalTime) )
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, asc, selectOne, where_, val
    , (^.), (?.), (==.), (:&)((:&)), (=.)
    , innerJoin, on, between, subSelect, just, leftJoin, Value (unValue)
    , SqlExpr, subSelectCount, update, set
    )
import Database.Persist
    ( Entity (Entity), entityVal, entityKey, insert, insert_, upsert)
import qualified Database.Persist as P ((=.), delete, PersistStoreWrite (replace))
    
import Foundation
    ( Handler, Form, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (DataR, StaticR)
    , DataR
      ( EmployeeR, EmployeeNewR, StaffR, EmployeeEditR, EmployeeDeleR
      , StaffAssignmentsR, StaffAssignmentR, StaffAssignmentNewR
      , StaffAssignmentEditR, StaffAssignmentDeleR, StaffScheduleR
      , StaffScheduleSlotsR, StaffScheduleSlotR, StaffScheduleSlotNewR
      , StaffScheduleSlotEditR, StaffScheduleSlotDeleR, EmployeePhotoR
      , StaffScheduleFillFromWorkingHoursR, StaffScheduleFillFromPreviousMonthR
      , ServicePhotoDefaultR
      )
    , AppMessage
      ( MsgStaff, MsgAdd, MsgYouMightWantToAddAFew, MsgThereAreNoDataYet
      , MsgCancel, MsgSave, MsgBack, MsgFullName, MsgAccount, MsgMobile
      , MsgPhone, MsgRecordAdded, MsgEmployee, MsgServiceAssignments
      , MsgDetails, MsgDeleteAreYouSure, MsgEdit, MsgDele, MsgConfirmPlease
      , MsgInvalidFormData, MsgRecordDeleted, MsgRecordEdited, MsgWorkspace
      , MsgService, MsgAssignmentDate, MsgServiceAssignment, MsgTheStart
      , MsgBusiness, MsgSchedulingInterval, MsgUnitMinutes, MsgWorkSchedule
      , MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgSymbolHour, MsgSymbolMinute, MsgWorkingHours, MsgStartTime
      , MsgEndTime, MsgDay, MsgFillFromPreviousMonth, MsgFillFromWorkingSchedule
      , MsgPriority, MsgRole, MsgAlreadyExists, MsgAttribution, MsgUploadPhoto
      , MsgPhoto, MsgTakePhoto, MsgClose, MsgLogo
      )
    )
    
import Material3
    ( md3widget, md3widgetTextarea, md3widgetSelect, md3datetimeLocalField
    )

import Model
    ( statusError, statusSuccess
    , StaffId, Staff (Staff, staffName, staffMobile, staffPhone, staffAccount)
    , User (userName, userEmail)
    , AssignmentId
    , Assignment
      ( Assignment, assignmentService, assignmentTime, assignmentSlotInterval
      , assignmentPriority, assignmentRole
      )
    , ServiceId, Service (Service, serviceName)
    , Workspace (Workspace)
    , Business (Business)
    , ScheduleId, Schedule (Schedule, scheduleStart, scheduleEnd)
    , WorkingHours (WorkingHours), StaffPhoto (StaffPhoto)
    , EntityField
      ( StaffId, UserName, UserId, AssignmentService, ServiceId
      , ServiceWorkspace, WorkspaceId, WorkspaceBusiness, BusinessId
      , AssignmentStaff, AssignmentId, ServiceName, ScheduleAssignment
      , ScheduleDay, ScheduleId, WorkingHoursDay, WorkingHoursWorkspace
      , AssignmentRole, StaffPhotoStaff, StaffPhotoAttribution, StaffPhotoMime
      , StaffPhotoPhoto
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg
    , img_camera_24dp_0000F5_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)
import Text.Printf (printf)

import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages, whamlet, addMessageI, redirect
    , MonadHandler (liftHandler), SomeMessage (SomeMessage), MonadIO (liftIO)
    , TypedContent (TypedContent), ToContent (toContent), FileInfo (fileContentType)
    , fileSourceByteString
    )
import Yesod.Core.Handler (getMessageRender)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( optionsPairs, runFormPost
    , FieldSettings
      ( FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs
      )
    , Field, FormResult (FormSuccess)
    , FieldView (fvInput, fvId, fvErrors, fvRequired, fvLabel)
    )
import Yesod.Form.Fields
    ( textField, htmlField, selectField, fileField, timeField
    , intField
    )
import Yesod.Form.Functions (mopt, mreq, generateFormPost, checkM)
import Yesod.Persist (YesodPersist(runDB))


postStaffScheduleFillFromPreviousMonthR :: StaffId -> AssignmentId -> Month -> Handler Html
postStaffScheduleFillFromPreviousMonthR eid aid month = do
    ((fr,_),_) <- runFormPost formFillFromPreviousMonth
    case fr of
      FormSuccess () -> do
          slots <- runDB $ select $ do

              let prev = addMonths (-1) month
              
              x <- from $ table @Schedule
              where_ $ x ^. ScheduleAssignment ==. val aid
              where_ $ x ^. ScheduleDay `between` (val $ periodFirstDay prev, val $ periodLastDay prev)
              return x

          forM_ slots $ \(Entity _ (Schedule _ day start end)) -> do
              let (y,m,d) = toGregorian day
              let curr = fromGregorian y (m + 1) d
              runDB $ insert_ $ Schedule aid curr start end 
              
          redirect $ DataR $ StaffScheduleR eid aid month
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ StaffScheduleR eid aid month


postStaffScheduleFillFromWorkingHoursR :: StaffId -> AssignmentId -> Month -> Handler Html
postStaffScheduleFillFromWorkingHoursR eid aid month = do
    ((fr,_),_) <- runFormPost formFillFromWorkingHours
    case fr of
      FormSuccess () -> do
          slots <- runDB $ select $ do
              x <- from $ table @WorkingHours
              where_ $ just ( x ^. WorkingHoursWorkspace ) ==. subSelect
                  ( do
                        a :& s <- from $ table @Assignment
                            `innerJoin` table @Service `on` (\(a :& s) -> a ^. AssignmentService ==. s ^. ServiceId)
                        where_ $ a ^. AssignmentId ==. val aid
                        return $ s ^. ServiceWorkspace
                  )
              where_ $ x ^. WorkingHoursDay `between` (val $ periodFirstDay month, val $ periodLastDay month)
              return x

          forM_ slots $ \(Entity _ (WorkingHours _ day start end)) -> do
              runDB $ insert_ $ Schedule aid day start end 
              
          redirect $ DataR $ StaffScheduleR eid aid month
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ StaffScheduleR eid aid month


postStaffScheduleSlotDeleR :: StaffId -> AssignmentId -> Day -> ScheduleId -> Handler Html
postStaffScheduleSlotDeleR eid aid day sid = do
    
    ((fr,_),_) <- runFormPost formWorkingSlotDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ StaffScheduleSlotsR eid aid day
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ StaffScheduleSlotR eid aid day sid


getStaffScheduleSlotEditR :: StaffId -> AssignmentId -> Day -> ScheduleId -> Handler Html
getStaffScheduleSlotEditR eid aid day sid = do
    
    slot <- runDB $ selectOne $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleId ==. val sid
        return x
    
    (fw,et) <- generateFormPost $ formWorkSlot aid day slot
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idFormWorkSlot <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/staff/assignments/schedule/slots/edit")


getStaffScheduleSlotNewR :: StaffId -> AssignmentId -> Day -> Handler Html
getStaffScheduleSlotNewR eid aid day = do

    (fw,et) <- generateFormPost $ formWorkSlot aid day Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idFormWorkSlot <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/staff/assignments/schedule/slots/new")


formWorkSlot :: AssignmentId -> Day -> Maybe (Entity Schedule) -> Form Schedule
formWorkSlot wid day slot extra = do
    
    (startR,startV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgStartTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (scheduleStart . entityVal <$> slot)
        
    (endR,endV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgEndTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (scheduleEnd . entityVal <$> slot)

    let r = Schedule wid day <$> startR <*> endR
    let w = [whamlet|#{extra} ^{md3widget startV} ^{md3widget endV}|]
    return (r,w)


postStaffScheduleSlotR :: StaffId -> AssignmentId -> Day -> ScheduleId -> Handler Html
postStaffScheduleSlotR eid aid day sid = do
    
    slot <- runDB $ selectOne $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formWorkSlot aid day slot

    case fr of
      FormSuccess r -> do
          runDB $ P.replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ StaffScheduleSlotsR eid aid day
      _otherwise -> do    
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkingHours
              idFormWorkSlot <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/staff/assignments/schedule/slots/edit")


getStaffScheduleSlotR :: StaffId -> AssignmentId -> Day -> ScheduleId -> Handler Html
getStaffScheduleSlotR eid aid day sid = do
    
    slot <- runDB $ selectOne $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleId ==. val sid
        return x

    (fw0,et0) <- generateFormPost formWorkingSlotDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idFormDelete <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/staff/assignments/schedule/slots/slot")


formWorkingSlotDelete :: Form ()
formWorkingSlotDelete extra = return (pure (), [whamlet|#{extra}|]) 


postStaffScheduleSlotsR :: StaffId -> AssignmentId -> Day -> Handler Html
postStaffScheduleSlotsR eid aid day = do

    ((fr,fw),et) <- runFormPost $ formWorkSlot aid day Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ StaffScheduleSlotsR eid aid day
          
      _otherwise -> do    
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkingHours
              idFormWorkSlot <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/staff/assignments/schedule/slots/new") 


getStaffScheduleSlotsR :: StaffId -> AssignmentId -> Day -> Handler Html
getStaffScheduleSlotsR eid aid day = do
    
    slots <- runDB $ select $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleAssignment ==. val aid
        where_ $ x ^. ScheduleDay ==. val day
        return x

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        classHeadline <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/staff/assignments/schedule/slots/slots")


getStaffScheduleR :: StaffId -> AssignmentId -> Month -> Handler Html
getStaffScheduleR eid aid month = do

    let mapper (Entity _ (Schedule _ day start end)) = (day,diffLocalTime (LocalTime day end) (LocalTime day start))
    
    hours <- groupByKey mapper <$> runDB ( select $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleAssignment ==. val aid
        where_ $ x ^. ScheduleDay `between` (val $ periodFirstDay month, val $ periodLastDay month)
        return x ) :: Handler (M.Map Day NominalDiffTime)

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    (fw1,et1) <- generateFormPost formFillFromWorkingHours
    (fw2,et2) <- generateFormPost formFillFromPreviousMonth
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkSchedule
        idMenuActions <- newIdent
        idFormFillFromWorkingHours <- newIdent
        idFormFillFromPreviousMonth <- newIdent
        idCalendarPage <- newIdent
        $(widgetFile "common/css/header")  
        $(widgetFile "data/staff/assignments/schedule/hours")
  where
      
      groupByKey :: (Ord k, Num a) => (v -> (k,a)) -> [v] -> M.Map k a
      groupByKey key = M.fromListWith (+) . fmap key

      rest diff = mod (div (truncate @_ @Integer $ nominalDiffTimeToSeconds diff) 60) 60


formFillFromPreviousMonth :: Form ()
formFillFromPreviousMonth extra = return (pure (), [whamlet|#{extra}|])


formFillFromWorkingHours :: Form ()
formFillFromWorkingHours extra = return (pure (), [whamlet|#{extra}|])


postStaffAssignmentDeleR :: StaffId -> AssignmentId -> Handler Html
postStaffAssignmentDeleR eid aid = do

    ((fr,_),_) <- runFormPost formServiceAssignmentDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete aid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ StaffAssignmentsR eid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ StaffAssignmentR eid aid


getStaffAssignmentEditR :: StaffId -> AssignmentId -> Handler Html
getStaffAssignmentEditR eid aid = do

    assignment <- runDB $ selectOne $ do
        x <- from $ table @Assignment
        where_ $ x ^. AssignmentId ==. val aid
        return x
        
    (fw,et) <- generateFormPost $ formServiceAssignment eid assignment
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idFormAssignment <- newIdent
        $(widgetFile "data/staff/assignments/edit")


getStaffAssignmentNewR :: StaffId -> Handler Html
getStaffAssignmentNewR eid = do
    (fw,et) <- generateFormPost $ formServiceAssignment eid Nothing
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idFormAssignment <- newIdent
        $(widgetFile "data/staff/assignments/new")


postStaffAssignmentR :: StaffId -> AssignmentId -> Handler Html
postStaffAssignmentR eid aid = do

    assignment <- runDB $ selectOne $ do
        x <- from $ table @Assignment
        where_ $ x ^. AssignmentId ==. val aid
        return x
        
    ((fr,fw),et) <- runFormPost $ formServiceAssignment eid assignment

    case fr of
      FormSuccess r -> do
          runDB $ P.replace aid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ StaffAssignmentR eid aid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idFormAssignment <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/staff/assignments/edit")


getStaffAssignmentR :: StaffId -> AssignmentId -> Handler Html
getStaffAssignmentR eid aid = do

    assignment <- runDB $ selectOne $ do
        x :& s :& w :& b :& e <- from $ table @Assignment
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @Staff `on` (\(x :& _ :& _ :& _ :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        where_ $ x ^. AssignmentId ==. val aid
        return (x,(e,(s,(w,b))))

    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime

    (fw0,et0) <- generateFormPost formServiceAssignmentDelete
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idFormDelete <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/staff/assignments/assignment")

  where
      toMinutes interval = truncate @_ @Integer (nominalDiffTimeToSeconds interval / 60)


formServiceAssignmentDelete :: Form ()
formServiceAssignmentDelete extra = return (pure (), [whamlet|#{extra}|])


postStaffAssignmentsR :: StaffId -> Handler Html
postStaffAssignmentsR eid = do
    ((fr,fw),et) <- runFormPost $ formServiceAssignment eid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ StaffAssignmentsR eid
          
      _otherwise -> do        
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idFormAssignment <- newIdent
              $(widgetFile "data/staff/assignments/new")


formServiceAssignment :: StaffId -> Maybe (Entity Assignment) -> Form Assignment
formServiceAssignment eid assignment extra = do
    
    services <- liftHandler $ runDB $ select $ do
        x <- from $ table @Service
        orderBy [asc (x ^. ServiceName), desc (x ^. ServiceId)]
        return x
    
    (serviceR, serviceV) <- mreq (selectField (optionsPairs (options <$> services))) FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (assignmentService . entityVal <$> assignment)
    
    (roleR, roleV) <- mreq (uniqueRoleField eid serviceR) FieldSettings
        { fsLabel = SomeMessage MsgRole
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (assignmentRole . entityVal <$> assignment)
    
    (startR, startV) <- mreq md3datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAssignmentDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("step","1")]
        } (utcToLocalTime utc . assignmentTime . entityVal <$> assignment)
    
    (intervalR, intervalV) <- mreq (intField @_ @Integer) FieldSettings
        { fsLabel = SomeMessage MsgSchedulingInterval
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (truncate . (/ 60) . nominalDiffTimeToSeconds . assignmentSlotInterval . entityVal <$> assignment)
    
    (priorityR, priorityV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgPriority
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (assignmentPriority . entityVal <$> assignment)

    let r = Assignment eid <$> serviceR <*> roleR <*> (localTimeToUTC utc <$> startR)
            <*> ((* 60) . secondsToNominalDiffTime . fromIntegral <$> intervalR)
            <*> priorityR

    let w = [whamlet|
                    #{extra}
                    ^{md3widgetSelect serviceV}
                    ^{md3widget roleV}
                    ^{md3widget startV}

                    <div.field.label.border.round :isJust (fvErrors intervalV):.invalid>
                      ^{fvInput intervalV}
                      <label for=#{fvId intervalV}>
                        #{fvLabel intervalV}
                        $if fvRequired intervalV
                          <sup>*
                      $maybe err <- fvErrors intervalV
                        <span.error>#{err}
                      $nothing
                        <span.helper>_{MsgUnitMinutes}                     
                      
                                            
                    ^{md3widget priorityV}
                    |]

    return (r, w)
        
  where
      options e = (serviceName . entityVal $ e, entityKey e)
      
      uniqueRoleField :: StaffId -> FormResult ServiceId -> Field Handler Text
      uniqueRoleField eid' sid = checkM (uniqueRole eid' sid) textField

      uniqueRole :: StaffId -> FormResult ServiceId -> Text -> Handler (Either AppMessage Text)
      uniqueRole eid' sid role = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Assignment
              where_ $ x ^. AssignmentStaff ==. val eid'
              case sid of
                FormSuccess sid' -> where_ $ x ^. AssignmentService ==. val sid'
                _otherwise -> where_ $ val True
              where_ $ x ^. AssignmentRole ==. val role
              return x
          return $ case x of
            Nothing -> Right role
            Just (Entity aid _) -> case assignment of
              Nothing -> Left MsgAlreadyExists
              Just (Entity aid' _) | aid == aid' -> Right role
                                   | otherwise -> Left MsgAlreadyExists


getStaffAssignmentsR :: StaffId -> Handler Html
getStaffAssignmentsR eid = do
    
    assignments <- runDB $ select $ do
        x :& s :& w :& b <- from $ table @Assignment
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
        where_ $ x ^. AssignmentStaff ==. val eid
        orderBy [desc (x ^. AssignmentId)]
        return (x,(s,(w,b)))

    attribution <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @StaffPhoto
        where_ $ x ^. StaffPhotoStaff ==. val eid
        return (x ^. StaffPhotoAttribution) )
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignments
        classHeadline <- newIdent
        classSupportingText <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/rows")
        $(widgetFile "data/staff/assignments/assignments")


postEmployeeDeleR :: StaffId -> Handler Html
postEmployeeDeleR eid = do
    ((fr,_),_) <- runFormPost formEmployeeDelete
    case fr of
      FormSuccess () -> do
          void $ runDB $ P.delete eid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR StaffR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ EmployeeR eid


postEmployeeR :: StaffId -> Handler Html
postEmployeeR eid = do
    
    employee <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x

    ((fr,fw),et) <- runFormPost $ formEmployee employee

    case fr of
      FormSuccess (r,(Just fi,attrib)) -> do
          void $ runDB $ P.replace eid r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (StaffPhoto eid (fileContentType fi) bs attrib)
              [ StaffPhotoMime P.=. fileContentType fi
              , StaffPhotoPhoto P.=. bs
              , StaffPhotoAttribution P.=. attrib
              ]
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ EmployeeR eid
      FormSuccess (r,(Nothing,attrib)) -> do
          runDB $ P.replace eid r
          runDB $ update $ \x -> do
                set x [ StaffPhotoAttribution =. val attrib ]
                where_ $ x ^. StaffPhotoStaff ==. val eid
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ EmployeeR eid
      _otherwise -> do    
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgEmployee
              idFormStaff <- newIdent
              $(widgetFile "data/staff/edit")


getEmployeeEditR :: StaffId -> Handler Html
getEmployeeEditR eid = do
    
    employee <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x

    (fw,et) <- generateFormPost $ formEmployee employee
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployee
        idFormStaff <- newIdent
        $(widgetFile "data/staff/edit")


postStaffR :: Handler Html
postStaffR = do

    ((fr,fw),et) <- runFormPost $ formEmployee Nothing

    case fr of
      FormSuccess (r,(Just fi,attrib)) -> do
          eid <- runDB $ insert r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (StaffPhoto eid (fileContentType fi) bs attrib)
                    [ StaffPhotoMime P.=. fileContentType fi
                    , StaffPhotoPhoto P.=. bs
                    , StaffPhotoAttribution P.=. attrib
                    ]
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR StaffR
          
      FormSuccess (r,_) -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR StaffR
          
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              idFormStaff <- newIdent
              $(widgetFile "data/staff/new")


getEmployeeNewR :: Handler Html
getEmployeeNewR = do

    (fw,et) <- generateFormPost $ formEmployee Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        idFormStaff <- newIdent
        $(widgetFile "data/staff/new")


formEmployee :: Maybe (Entity Staff) -> Form (Staff,(Maybe FileInfo,Maybe Html))
formEmployee staff extra = do
    
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (staffName . entityVal <$> staff)

    users <- liftHandler $ runDB $ select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserName), desc (x ^. UserId)]
        return x
        
    (accountR, accountV) <- mopt (selectField (optionsPairs (options <$> users))) FieldSettings
        { fsLabel = SomeMessage MsgAccount
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (staffAccount . entityVal <$> staff)
    
    (mobileR,mobileV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgMobile
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (staffMobile . entityVal <$> staff)
    
    (phoneR,phoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (staffPhone . entityVal <$> staff)

    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    attrib <- (unValue =<<) <$> case staff of
      Just (Entity eid _) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @StaffPhoto
          where_ $ x ^. StaffPhotoStaff ==. val eid
          return $ x ^. StaffPhotoAttribution
      Nothing -> return Nothing
    
    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just attrib)

    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    idButtonUploadPhoto <- newIdent
    idButtonTakePhoto <- newIdent

    idOverlay <- newIdent 
    idDialogSnapshot <- newIdent
    idVideo <- newIdent
    idButtonCloseDialogSnapshot <- newIdent
    idButtonCapture <- newIdent

    return ( (,) <$> (Staff <$> nameR <*> accountR <*> mobileR <*> phoneR) <*> ((,) <$> photoR <*> attribR)
           , $(widgetFile "data/staff/form")
           )
        
  where
      options e = (fromMaybe (userEmail . entityVal $ e) (userName . entityVal $ e), entityKey e)


getEmployeeR :: StaffId -> Handler Html
getEmployeeR eid = do

    employee <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x

    attribution <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @StaffPhoto
        where_ $ x ^. StaffPhotoStaff ==. val eid
        return (x ^. StaffPhotoAttribution) )

    (fw0,et0) <- generateFormPost formEmployeeDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployee 
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idFormDelete <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/staff/employee")


formEmployeeDelete :: Form ()
formEmployeeDelete extra = return (pure (), [whamlet|#{extra}|])


getStaffR :: Handler Html
getStaffR = do

    staff <- (bimap (second (join . unValue)) unValue <$>) <$> runDB ( select $ do
        x :& f <- from $ table @Staff
            `leftJoin` table @StaffPhoto `on` (\(x :& f) -> just (x ^. StaffId) ==. f ?. StaffPhotoStaff)

        let assignments :: SqlExpr (Value Int)
            assignments = subSelectCount $ do
                y <- from $ table @Assignment
                where_ $ y ^. AssignmentStaff ==. x ^. StaffId
            
        orderBy [desc (x ^. StaffId)]
        return ((x, f ?. StaffPhotoAttribution), assignments) )
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        classHeadline <- newIdent
        classSupportingText <- newIdent
        classAttribution <- newIdent
        
        $(widgetFile "common/css/main")
        $(widgetFile "data/staff/staff")


getEmployeePhotoR :: StaffId -> Handler TypedContent
getEmployeePhotoR eid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @StaffPhoto
        where_ $ x ^. StaffPhotoStaff ==. val eid
        return x
    case photo of
      Just (Entity _ (StaffPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg
