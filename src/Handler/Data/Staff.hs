{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Data.Staff
  ( getStaffR
  , getEmployeeR
  , getEmployeeNewR
  , postStaffR
  , getEmployeeEditR
  , postEmployeeDeleR
  , postEmployeeR
  , getStaffAssignmentsR
  , postStaffAssignmentsR
  , getStaffAssignmentR
  , postStaffAssignmentR
  , getStaffAssignmentNewR
  , getStaffAssignmentEditR
  , postStaffAssignmentDeleR
  ) where

import Control.Monad (void)

import Data.Maybe (fromMaybe)
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, asc, selectOne, where_, val
    , (^.), (==.), (:&)((:&))
    , innerJoin, on
    )
import Database.Persist
    ( Entity (Entity), entityVal, entityKey, insert_)
import qualified Database.Persist as P (delete, PersistStoreWrite (replace))
    
import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR
      ( EmployeeR, EmployeeNewR, StaffR, EmployeeEditR, EmployeeDeleR
      , StaffAssignmentsR, StaffAssignmentR, StaffAssignmentNewR
      , StaffAssignmentEditR, StaffAssignmentDeleR
      )
    , AppMessage
      ( MsgStaff, MsgAdd, MsgYouMightWantToAddAFew, MsgThereAreNoDataYet
      , MsgCancel, MsgSave, MsgBack, MsgFullName, MsgAccount, MsgMobile
      , MsgPhone, MsgRecordAdded, MsgEmployee, MsgServiceAssignments
      , MsgDetails, MsgDeleteAreYouSure, MsgEdit, MsgDele, MsgConfirmPlease
      , MsgInvalidFormData, MsgRecordDeleted, MsgRecordEdited, MsgWorkspace, MsgService, MsgAssignmentDate, MsgServiceAssignment, MsgTheStart, MsgBusiness
      )
    )
    
import Material3 (md3mreq, md3telField, md3textField, md3mopt, md3selectField, md3datetimeLocalField)

import Model
    ( statusError, statusSuccess
    , StaffId, Staff (Staff, staffName, staffMobile, staffPhone, staffAccount)
    , User (User, userName, userEmail)
    , AssignmentId, Assignment (Assignment, assignmentService, assignmentStart)
    , Service (Service, serviceName)
    , Workspace (Workspace)
    , Business (Business)
    , EntityField
      ( StaffId, UserName, UserId, AssignmentService, ServiceId
      , ServiceWorkspace, WorkspaceId, WorkspaceBusiness, BusinessId
      , AssignmentStaff, AssignmentId, ServiceName
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetAccount, widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages, whamlet, addMessageI, redirect
    , MonadHandler (liftHandler), SomeMessage (SomeMessage)
    )
import Yesod.Core.Handler (getMessageRender)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( FieldSettings
      ( FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs
      )
    , FormResult (FormSuccess), FieldView (fvInput), optionsPairs, runFormPost
    )
import Yesod.Form.Functions (generateFormPost)
import Yesod.Persist (YesodPersist(runDB))
import Data.Time.LocalTime (utc, utcToLocalTime, localTimeToUTC)


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

    (fw2,et2) <- generateFormPost formServiceAssignmentDelete
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment 
        $(widgetFile "data/staff/assignments/assignment")


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
     
    msgr <- getMessageRender
    
    services <- liftHandler $ runDB $ select $ do
        x <- from $ table @Service
        orderBy [asc (x ^. ServiceName), desc (x ^. ServiceId)]
        return x
    
    (serviceR, serviceV) <- md3mreq (md3selectField (optionsPairs (options <$> services))) FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgService)]
        } (assignmentService . entityVal <$> assignment)
    
    (startR, startV) <- md3mreq md3datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAssignmentDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAssignmentDate),("step","1")]
        } (utcToLocalTime utc . assignmentStart . entityVal <$> assignment)

    return ( Assignment eid <$> serviceR <*> (localTimeToUTC utc <$> startR)
           , [whamlet|#{extra} ^{fvInput serviceV} ^{fvInput startV}|]
           )
        
  where
      options e = (serviceName . entityVal $ e, entityKey e)


getStaffAssignmentsR :: StaffId -> Handler Html
getStaffAssignmentsR eid = do
    
    assignments <- runDB $ select $ do
        x :& s :& w :& b :& e <- from $ table @Assignment
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @Staff `on` (\(x :& _ :& _ :& _ :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        where_ $ x ^. AssignmentStaff ==. val eid
        orderBy [desc (x ^. AssignmentId)]
        return (x,(e,(s,(w,b))))
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignments
        idTabAssignments <- newIdent
        idPanelAssignments <- newIdent
        idFabAdd <- newIdent 
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
      FormSuccess r -> do
          void $ runDB $ P.replace eid r
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
      FormSuccess r -> do
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


formEmployee :: Maybe (Entity Staff) -> Form Staff
formEmployee staff extra = do

    msgr <- getMessageRender
    
    (nameR,nameV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgFullName)]
        } (staffName . entityVal <$> staff)

    users <- liftHandler $ runDB $ select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserName), desc (x ^. UserId)]
        return x
        
    (accountR, accountV) <- md3mopt (md3selectField (optionsPairs (options <$> users))) FieldSettings
        { fsLabel = SomeMessage MsgAccount
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAccount)]
        } (staffAccount . entityVal <$> staff)
    
    (mobileR,mobileV) <- md3mopt md3telField FieldSettings
        { fsLabel = SomeMessage MsgMobile
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgMobile)]
        } (staffMobile . entityVal <$> staff)
    
    (phoneR,phoneV) <- md3mopt md3telField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgPhone)]
        } (staffPhone . entityVal <$> staff)

    return ( Staff <$> nameR <*> accountR <*> mobileR <*> phoneR
           , [whamlet|#{extra} ^{fvInput nameV} ^{fvInput accountV} ^{fvInput mobileV} ^{fvInput phoneV}|]
           )
        
  where
      options e = (fromMaybe (userEmail . entityVal $ e) (userName . entityVal $ e), entityKey e)


getEmployeeR :: StaffId -> Handler Html
getEmployeeR eid = do

    employee <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x

    (fw2,et2) <- generateFormPost formEmployeeDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployee 
        idPanelDetails <- newIdent
        idTabDetails <- newIdent
        $(widgetFile "data/staff/employee")


formEmployeeDelete :: Form ()
formEmployeeDelete extra = return (pure (), [whamlet|#{extra}|])


getStaffR :: Handler Html
getStaffR = do

    staff <- runDB $ select $ do
        x <- from $ table @Staff
        orderBy [desc (x ^. StaffId)]
        return x
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff 
        idFabAdd <- newIdent
        $(widgetFile "data/staff/staff")

