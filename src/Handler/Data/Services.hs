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
  ) where

import Control.Monad (void)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, asc, selectOne, where_, val
    , (^.), (==.)
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
      )
    , AppMessage
      ( MsgServices, MsgAdd, MsgYouMightWantToAddAFew, MsgThereAreNoDataYet
      , MsgService, MsgSave, MsgCancel, MsgBack, MsgTheName, MsgWorkspace
      , MsgDescription, MsgRecordAdded, MsgServiceAssignments, MsgDetails
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgEdit, MsgRecordEdited
      , MsgDele, MsgRecordDeleted, MsgInvalidFormData, MsgServiceAssignment
      , MsgEmployee, MsgAssignmentDate
      )
    )
    
import Material3
    ( md3mreq, md3textField, md3textareaField, md3mopt, md3selectField
    , md3datetimeLocalField
    )

import Model
    ( statusSuccess, statusError
    , ServiceId, Service(Service, serviceName, serviceWorkspace, serviceDescr)
    , Workspace (workspaceName)
    , AssignmentId, Assignment (Assignment, assignmentStaff, assignmentTime)
    , Staff (Staff, staffName)
    , EntityField
      ( ServiceId, WorkspaceName, WorkspaceId, AssignmentTime, AssignmentId
      , StaffName, StaffId
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
    , optionsPairs, FieldView (fvInput), FormResult (FormSuccess)
    )
import Yesod.Form.Functions (generateFormPost, runFormPost)
import Yesod.Persist (YesodPersist(runDB))


getServiceAssignmentR :: ServiceId -> AssignmentId -> Handler Html
getServiceAssignmentR sid aid = undefined


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
    
    (timeR, timeV) <- md3mreq md3datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAssignmentDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAssignmentDate)]
        } (assignmentTime . entityVal <$> assignment)

    return ( Assignment <$> employeeR <*> pure sid <*> timeR
           , [whamlet|#{extra} ^{fvInput employeeV} ^{fvInput timeV}|]
           )
        
  where
      options e = (staffName . entityVal $ e, entityKey e)


getServiceAssignmentsR :: ServiceId -> Handler Html
getServiceAssignmentsR sid = do
    
    assignments <- runDB $ select $ do
        x <- from $ table @Assignment
        orderBy [desc (x ^. AssignmentId)]
        return x
        
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
        
    ((fr,fw),et) <- runFormPost $ formServices service
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
        
    (fw,et) <- generateFormPost $ formServices service
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormStaff <- newIdent
        $(widgetFile "data/services/edit")


postServicesR :: Handler Html
postServicesR = do
    ((fr,fw),et) <- runFormPost $ formServices Nothing
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
    (fw,et) <- generateFormPost $ formServices Nothing
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormStaff <- newIdent
        $(widgetFile "data/services/new")


formServices :: Maybe (Entity Service) -> Form Service
formServices service extra = do

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
    
    (nameR,nameV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (serviceName . entityVal <$> service)
    
    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgDescription)]
        } (serviceDescr . entityVal <$> service)

    return ( Service <$> workspaceR <*> nameR <*> descrR
           , [whamlet|#{extra} ^{fvInput workspaceV} ^{fvInput nameV} ^{fvInput descrV}|]
           )
        
  where
      options e = (workspaceName . entityVal $ e, entityKey e)
    


getServiceR :: ServiceId -> Handler Html
getServiceR sid = do
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x

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
        x <- from $ table @Service
        orderBy [desc (x ^. ServiceId)]
        return x
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFabAdd <- newIdent
        $(widgetFile "data/services/services")
