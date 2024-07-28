{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Data.Services
  ( getServicesR
  , getServiceR
  , getServiceNewR
  , postServicesR
  , getServiceEditR
  , postServiceDeleR
  , postServiceR
  , getServiceAssignmentsR
  , getServiceAssignmentNewR
  , postServiceAssignmentsR
  , getServiceAssignmentR
  , getServiceAssignmentEditR
  , postServiceAssignmentDeleR
  , postServiceAssignmentR
  ) where


import Control.Monad (void)

import Data.Text (Text)
import Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (utcToLocalTime, localTimeToUTC, utc)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, asc, selectOne, where_, val
    , (^.), (==.), (:&)((:&))
    , innerJoin, on
    )
import Database.Persist
    (Entity (Entity), entityVal, entityKey, insert_)
import qualified Database.Persist as P (delete, PersistStoreWrite (replace))

import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR
      ( ServicesR, ServiceR, ServiceNewR, ServiceEditR, ServiceDeleR
      , ServiceAssignmentsR, ServiceAssignmentNewR, ServiceAssignmentR
      , ServiceAssignmentEditR, ServiceAssignmentDeleR
      )
    , AppMessage
      ( MsgServices, MsgAdd, MsgYouMightWantToAddAFew, MsgThereAreNoDataYet
      , MsgService, MsgSave, MsgCancel, MsgBack, MsgTheName, MsgWorkspace
      , MsgDescription, MsgRecordAdded, MsgServiceAssignments, MsgDetails
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgEdit, MsgRecordEdited
      , MsgDele, MsgRecordDeleted, MsgInvalidFormData, MsgServiceAssignment
      , MsgEmployee, MsgAssignmentDate, MsgTheStart, MsgBusiness, MsgPrice
      , MsgSchedulingInterval, MsgUnitMinutes, MsgPriority, MsgAlreadyExists
      , MsgRole, MsgAvailable, MsgDuration
      )
    )
    
import Material3
    ( md3mreq, md3textField, md3textareaField, md3mopt, md3selectField
    , md3datetimeLocalField, md3intField, md3switchField
    )

import Model
    ( statusSuccess, statusError
    , ServiceId
    , Service
      ( Service, serviceName, serviceWorkspace, serviceDescr, servicePrice, serviceAvailable, serviceDuration
      )
    , WorkspaceId, Workspace (workspaceName, Workspace)
    , AssignmentId
    , Assignment
      ( Assignment, assignmentStaff, assignmentTime, assignmentSlotInterval
      , assignmentPriority, assignmentRole
      )
    , StaffId, Staff (staffName, Staff)
    , Business (Business)
    , EntityField
      ( ServiceId, WorkspaceName, WorkspaceId, AssignmentId, WorkspaceBusiness
      , StaffName, StaffId, AssignmentService, ServiceWorkspace, BusinessId
      , AssignmentStaff, ServiceName, AssignmentRole
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetAccount, widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, newIdent, getMessageRender
    , redirect, whamlet, addMessageI
    , SomeMessage (SomeMessage), MonadHandler (liftHandler)
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , optionsPairs, FieldView (fvInput, fvId, fvLabel), FormResult (FormSuccess), Field
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Persist (YesodPersist(runDB))


postServiceAssignmentR :: ServiceId -> AssignmentId -> Handler Html
postServiceAssignmentR sid aid = do

    assignment <- runDB $ selectOne $ do
        x <- from $ table @Assignment
        where_ $ x ^. AssignmentId ==. val aid
        return x
        
    ((fr,fw),et) <- runFormPost $ formServiceAssignment sid assignment

    case fr of
      FormSuccess r -> do
          runDB $ P.replace aid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ ServiceAssignmentR sid aid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idFormAssignment <- newIdent
              $(widgetFile "data/services/assignments/edit")


postServiceAssignmentDeleR :: ServiceId -> AssignmentId -> Handler Html
postServiceAssignmentDeleR sid aid = do

    ((fr,_),_) <- runFormPost formServiceAssignmentDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete aid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ ServiceAssignmentsR sid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ ServiceAssignmentR sid aid


getServiceAssignmentEditR :: ServiceId -> AssignmentId -> Handler Html
getServiceAssignmentEditR sid aid = do

    assignment <- runDB $ selectOne $ do
        x <- from $ table @Assignment
        where_ $ x ^. AssignmentId ==. val aid
        return x
        
    (fw,et) <- generateFormPost $ formServiceAssignment sid assignment
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idFormAssignment <- newIdent
        $(widgetFile "data/services/assignments/edit")


getServiceAssignmentR :: ServiceId -> AssignmentId -> Handler Html
getServiceAssignmentR sid aid = do

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
        $(widgetFile "data/services/assignments/assignment")
  where
      toMinutes interval = truncate @_ @Integer (nominalDiffTimeToSeconds interval / 60)


formServiceAssignmentDelete :: Form ()
formServiceAssignmentDelete extra = return (pure (), [whamlet|#{extra}|])


postServiceAssignmentsR :: ServiceId -> Handler Html
postServiceAssignmentsR sid = do
    ((fr,fw),et) <- runFormPost $ formServiceAssignment sid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ ServiceAssignmentsR sid
      _otherwise -> do        
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idFormAssignment <- newIdent
              $(widgetFile "data/services/assignments/new")


getServiceAssignmentNewR :: ServiceId -> Handler Html
getServiceAssignmentNewR sid = do
    (fw,et) <- generateFormPost $ formServiceAssignment sid Nothing
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idFormAssignment <- newIdent
        $(widgetFile "data/services/assignments/new")


formServiceAssignment :: ServiceId -> Maybe (Entity Assignment) -> Form Assignment
formServiceAssignment sid assignment extra = do
     
    msgr <- getMessageRender
    
    staff <- liftHandler $ runDB $ select $ do
        x <- from $ table @Staff
        orderBy [asc (x ^. StaffName), desc (x ^. StaffId)]
        return x
    
    (employeeR, employeeV) <- md3mreq (md3selectField (optionsPairs (options <$> staff))) FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgEmployee)]
        } (assignmentStaff . entityVal <$> assignment)
    
    (roleR, roleV) <- md3mreq (uniqueRoleField sid employeeR) FieldSettings
        { fsLabel = SomeMessage MsgRole
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgRole)]
        } (assignmentRole . entityVal <$> assignment)
    
    (startR, startV) <- md3mreq md3datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAssignmentDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAssignmentDate),("step","1")]
        } (utcToLocalTime utc . assignmentTime . entityVal <$> assignment)
    
    (intervalR, intervalV) <- md3mreq md3intField FieldSettings
        { fsLabel = SomeMessage MsgSchedulingInterval
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgSchedulingInterval),("supporting-text", msgr MsgUnitMinutes)]
        } (truncate . (/ 60) . nominalDiffTimeToSeconds . assignmentSlotInterval . entityVal <$> assignment)
    
    (priorityR, priorityV) <- md3mreq md3intField FieldSettings
        { fsLabel = SomeMessage MsgPriority
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgPriority)]
        } (assignmentPriority . entityVal <$> assignment)

    let r = Assignment <$> employeeR <*> pure sid <*> roleR <*> (localTimeToUTC utc <$> startR)
            <*> ((* 60) . secondsToNominalDiffTime . fromIntegral <$> intervalR)
            <*> priorityR

    let w = [whamlet|
                    #{extra}
                    ^{fvInput employeeV}
                    ^{fvInput roleV}
                    ^{fvInput startV}
                    ^{fvInput intervalV}
                    ^{fvInput priorityV}
                    |]
    
    return (r, w)
        
  where
      options e = (staffName . entityVal $ e, entityKey e)
      
      uniqueRoleField :: ServiceId -> FormResult StaffId -> Field Handler Text
      uniqueRoleField sid' eid = checkM (uniqueRole sid' eid) md3textField

      uniqueRole :: ServiceId -> FormResult StaffId -> Text -> Handler (Either AppMessage Text)
      uniqueRole sid' eid name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Assignment
              where_ $ x ^. AssignmentService ==. val sid'
              case eid of
                FormSuccess eid' -> where_ $ x ^. AssignmentStaff ==. val eid'
                _otherwise -> where_ $ val True
              where_ $ x ^. AssignmentRole ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity aid _) -> case assignment of
              Nothing -> Left MsgAlreadyExists
              Just (Entity aid' _) | aid == aid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getServiceAssignmentsR :: ServiceId -> Handler Html
getServiceAssignmentsR sid = do
    
    assignments <- runDB $ select $ do
        x :& s :& w :& b :& e <- from $ table @Assignment
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @Staff `on` (\(x :& _ :& _ :& _ :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        where_ $ x ^. AssignmentService ==. val sid
        orderBy [desc (x ^. AssignmentId)]
        return (x,(e,(s,(w,b))))
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignments
        idTabAssignments <- newIdent
        idPanelAssignments <- newIdent
        idFabAdd <- newIdent 
        $(widgetFile "data/services/assignments/assignments")


postServiceR :: ServiceId -> Handler Html
postServiceR sid = do
    
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x
        
    ((fr,fw),et) <- runFormPost $ formService service
    case fr of
      FormSuccess r -> do
          runDB $ P.replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ ServiceR sid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormStaff <- newIdent
              $(widgetFile "data/services/edit")


postServiceDeleR :: ServiceId -> Handler Html
postServiceDeleR sid = do
    ((fr,_),_) <- runFormPost formServiceDelete
    case fr of
      FormSuccess () -> do
          void $ runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR ServicesR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ ServiceR sid          
    

getServiceEditR :: ServiceId -> Handler Html
getServiceEditR sid = do
    
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x
        
    (fw,et) <- generateFormPost $ formService service
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormStaff <- newIdent
        $(widgetFile "data/services/edit")


postServicesR :: Handler Html
postServicesR = do
    ((fr,fw),et) <- runFormPost $ formService Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR ServicesR
      _otherwise -> do        
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormStaff <- newIdent
              $(widgetFile "data/services/new")


getServiceNewR :: Handler Html
getServiceNewR = do
    (fw,et) <- generateFormPost $ formService Nothing
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormStaff <- newIdent
        $(widgetFile "data/services/new")


formService :: Maybe (Entity Service) -> Form Service
formService service extra = do

    msgr <- getMessageRender
    
    workspaces <- liftHandler $ runDB $ select $ do
        x <- from $ table @Workspace
        orderBy [asc (x ^. WorkspaceName), desc (x ^. WorkspaceId)]
        return x
    
    (workspaceR, workspaceV) <- md3mreq (md3selectField (optionsPairs (options <$> workspaces))) FieldSettings
        { fsLabel = SomeMessage MsgWorkspace
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgWorkspace)]
        } (serviceWorkspace . entityVal <$> service)
    
    (nameR,nameV) <- md3mreq (uniqueNameField workspaceR) FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (serviceName . entityVal <$> service)
    
    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgDescription)]
        } (serviceDescr . entityVal <$> service)
    
    (priceR,priceV) <- md3mreq md3intField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgPrice)]
        } (servicePrice . entityVal <$> service)
    
    (availableR,availableV) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage MsgAvailable
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAvailable)]
        } (serviceAvailable . entityVal <$> service)
    
    (durationR,durationV) <- md3mreq md3intField FieldSettings
        { fsLabel = SomeMessage MsgDuration
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgDuration),("supporting-text", msgr MsgUnitMinutes)]
        } (truncate . (/ 60) . nominalDiffTimeToSeconds . serviceDuration . entityVal <$> service)

    let r = Service <$> workspaceR <*> nameR <*> descrR <*> priceR <*> availableR
             <*> ((* 60) . secondsToNominalDiffTime . fromIntegral <$> durationR)
    let w = [whamlet|
                    #{extra}
                    ^{fvInput workspaceV}
                    ^{fvInput nameV}
                    ^{fvInput descrV}
                    ^{fvInput priceV}

                    <div style="display:flex;align-items:center;gap:1rem">
                      ^{fvInput availableV}
                      <label.body-large for=#{fvId availableV}>
                        #{fvLabel availableV}
                        
                    ^{fvInput durationV}
                    |]
    return (r,w)
        
  where
      options e = (workspaceName . entityVal $ e, entityKey e)
      
      uniqueNameField :: FormResult WorkspaceId -> Field Handler Text
      uniqueNameField wid = checkM (uniqueName wid) md3textField

      uniqueName :: FormResult WorkspaceId -> Text -> Handler (Either AppMessage Text)
      uniqueName wid name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Service
              case wid of
                FormSuccess wid' -> where_ $ x ^. ServiceWorkspace ==. val wid'
                _otherwise -> where_ $ val True
              where_ $ x ^. ServiceName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity sid _) -> case service of
              Nothing -> Left MsgAlreadyExists
              Just (Entity sid' _) | sid == sid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists
    


getServiceR :: ServiceId -> Handler Html
getServiceR sid = do
    service <- runDB $ selectOne $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. ServiceId ==. val sid
        return (x,w)

    (fw2,et2) <- generateFormPost formServiceDelete
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idTabDetails <- newIdent
        idPanelDetails <- newIdent
        $(widgetFile "data/services/service")
        

formServiceDelete :: Form ()
formServiceDelete extra = return (pure (), [whamlet|#{extra}|])


getServicesR :: Handler Html
getServicesR = do
    
    services <- runDB $ select $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        orderBy [desc (x ^. ServiceId)]
        return (x,w)
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFabAdd <- newIdent
        $(widgetFile "data/services/services")
