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
  , getServicePhotoR, postServicePhotoR
  , getServicePhotosR, postServicePhotosR
  , getServicePhotoDefaultR
  , getServicePhotoEditR
  , getServicePhotoNewR
  , postServicePhotoDeleR
  ) where


import Control.Monad (void, unless, when, join)

import qualified Data.Aeson as A (toJSON)
import Data.Bifunctor (Bifunctor(bimap, second))
import qualified Data.Map as M (fromListWith, findWithDefault)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (utcToLocalTime, localTimeToUTC, utc)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, asc, selectOne, where_, val
    , (^.), (?.), (==.), (:&)((:&)), (=.)
    , innerJoin, on, isNothing_, valList, justList, in_, Value (unValue)
    , update, set, just, leftJoin
    )
import Database.Persist
    (Entity (Entity), entityVal, entityKey, insert_)
import qualified Database.Persist as P (delete, PersistStoreWrite (replace))
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( Handler, Form, Widget
    , Route (DataR, StaticR)
    , DataR
      ( ServicesR, ServiceR, ServiceNewR, ServiceEditR, ServiceDeleR
      , ServiceAssignmentsR, ServiceAssignmentNewR, ServiceAssignmentR
      , ServiceAssignmentEditR, ServiceAssignmentDeleR, SectorsR
      , DataBusinessesR, ServicePhotoR, ServicePhotosR, ServicePhotoEditR
      , ServicePhotoNewR, ServicePhotoDeleR, ServicePhotoDefaultR, EmployeePhotoR
      )
    , AppMessage
      ( MsgServices, MsgAdd, MsgYouMightWantToAddAFew, MsgThereAreNoDataYet
      , MsgService, MsgSave, MsgCancel, MsgBack, MsgTheName, MsgWorkspace
      , MsgDescription, MsgRecordAdded, MsgServiceAssignments, MsgDetails
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgEdit, MsgRecordEdited
      , MsgDele, MsgRecordDeleted, MsgInvalidFormData, MsgServiceAssignment
      , MsgEmployee, MsgAssignmentDate, MsgTheStart, MsgBusiness, MsgPrice
      , MsgSchedulingInterval, MsgUnitMinutes, MsgPriority, MsgAlreadyExists
      , MsgRole, MsgAvailable, MsgDuration, MsgType, MsgSectors, MsgBusinesses
      , MsgWorkspaces, MsgPhoto, MsgAttribution, MsgPhotos
      )
    )

import Material3
    ( md3mreq, md3textField, md3textareaField, md3mopt, md3selectField
    , md3datetimeLocalField, md3intField, md3switchField, md3htmlField
    )

import Model
    ( statusSuccess, statusError
    , ServiceId
    , Service
      ( Service, serviceName, serviceWorkspace, serviceDescr, servicePrice
      , serviceAvailable, serviceDuration, serviceType
      )
    , WorkspaceId, Workspace (workspaceName, Workspace)
    , AssignmentId
    , Assignment
      ( Assignment, assignmentStaff, assignmentTime, assignmentSlotInterval
      , assignmentPriority, assignmentRole
      )
    , StaffId, Staff (staffName, Staff)
    , Business (Business)
    , Sector (sectorName, Sector)
    , Sectors (Sectors)
    , ServicePhotoId
    , ServicePhoto
      ( ServicePhoto, servicePhotoAttribution, servicePhotoService, servicePhotoMime
      , servicePhotoPhoto
      )
    , BusinessLogo (BusinessLogo), StaffPhoto
    , EntityField
      ( ServiceId, WorkspaceName, WorkspaceId, AssignmentId, WorkspaceBusiness
      , StaffName, StaffId, AssignmentService, ServiceWorkspace, BusinessId
      , AssignmentStaff, ServiceName, AssignmentRole, SectorName, SectorParent
      , ServiceType, BusinessName, ServicePhotoService, ServicePhotoId
      , ServicePhotoMime, ServicePhotoPhoto, ServicePhotoAttribution
      , BusinessLogoBusiness, StaffPhotoStaff, StaffPhotoAttribution
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    , img_add_photo_alternate_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Widgets (widgetMenu, widgetAccount, widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, newIdent, getMessageRender
    , redirect, whamlet, addMessageI, YesodRequest (reqGetParams)
    , SomeMessage (SomeMessage), MonadHandler (liftHandler)
    , getRequest, TypedContent (TypedContent), ToContent (toContent)
    , lookupGetParams, FileInfo (fileContentType), fileSourceByteString
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , optionsPairs, FieldView (fvInput, fvId, fvLabel, fvErrors)
    , FormResult (FormSuccess), Field, textField, fileField, mreq, mopt
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Persist (YesodPersist(runDB))


postServiceAssignmentR :: ServiceId -> AssignmentId -> Handler Html
postServiceAssignmentR sid aid = do
    stati <- reqGetParams <$> getRequest

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
    stati <- reqGetParams <$> getRequest
    ((fr,_),_) <- runFormPost formServiceAssignmentDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete aid
          addMessageI statusSuccess MsgRecordDeleted
          redirect (DataR $ ServiceAssignmentsR sid,stati)
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (DataR $ ServiceAssignmentR sid aid,stati)


getServiceAssignmentEditR :: ServiceId -> AssignmentId -> Handler Html
getServiceAssignmentEditR sid aid = do
    stati <- reqGetParams <$> getRequest

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
    stati <- reqGetParams <$> getRequest

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
    stati <- reqGetParams <$> getRequest

    assignments <- (second (second (second (second(second (join . unValue))))) <$>) <$> runDB ( select $ do
        x :& s :& w :& b :& e :& f <- from $ table @Assignment
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @Staff `on` (\(x :& _ :& _ :& _ :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
            `leftJoin` table @StaffPhoto `on` (\(_ :& _ :& _ :& _ :& e :& f) -> just (e ^. StaffId) ==. f ?. StaffPhotoStaff)
        where_ $ x ^. AssignmentService ==. val sid
        orderBy [desc (x ^. AssignmentId)]
        return (x,(e,(s,(w,(b, f ?. StaffPhotoAttribution))))) )

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignments
        idTabAssignments <- newIdent
        idPanelAssignments <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/services/assignments/assignments")


postServiceR :: ServiceId -> Handler Html
postServiceR sid = do
    stati <- reqGetParams <$> getRequest

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
              idFormService <- newIdent
              $(widgetFile "data/services/edit")


postServiceDeleR :: ServiceId -> Handler Html
postServiceDeleR sid = do
    stati <- reqGetParams <$> getRequest
    ((fr,_),_) <- runFormPost formServiceDelete
    case fr of
      FormSuccess () -> do
          void $ runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect (DataR ServicesR,stati)
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (DataR $ ServiceR sid,stati)


getServiceEditR :: ServiceId -> Handler Html
getServiceEditR sid = do
    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formService service

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormService <- newIdent
        $(widgetFile "data/services/edit")


postServicesR :: Handler Html
postServicesR = do
    stati <- reqGetParams <$> getRequest
    ((fr,fw),et) <- runFormPost $ formService Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect (DataR ServicesR, stati)
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormService <- newIdent
              $(widgetFile "data/services/new")


getServiceNewR :: Handler Html
getServiceNewR = do
    stati <- reqGetParams <$> getRequest
    
    (fw,et) <- generateFormPost $ formService Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormService <- newIdent
        $(widgetFile "data/services/new")


formService :: Maybe (Entity Service) -> Form Service
formService service extra = do

    msgr <- getMessageRender

    workspaces <- liftHandler $ runDB $ select $ do
        x <- from $ table @Workspace
        orderBy [asc (x ^. WorkspaceName), desc (x ^. WorkspaceId)]
        return x

    (workspaceR, workspaceV) <- md3mreq (md3selectField (optionsPairs (optionsWorkspace <$> workspaces))) FieldSettings
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

    sectors <- liftHandler $ runDB $ select $ do
        x <- from $ table @Sector
        orderBy [asc (x ^. SectorName)]
        return x

    (typeR, typeV) <- md3mopt (md3selectField (optionsPairs (optionsType <$> sectors))) FieldSettings
        { fsLabel = SomeMessage MsgType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgType)]
        } (serviceType . entityVal <$> service)

    let r = Service <$> workspaceR <*> nameR <*> descrR <*> priceR <*> availableR
             <*> ((* 60) . secondsToNominalDiffTime . fromIntegral <$> durationR)
             <*> typeR

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
                    ^{fvInput typeV}
                    |]
    return (r,w)

  where
      optionsType e = (sectorName . entityVal $ e, entityKey e)

      optionsWorkspace e = (workspaceName . entityVal $ e, entityKey e)

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

    stati <- reqGetParams <$> getRequest

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

    stati <- reqGetParams <$> getRequest
    
    selectedSectors <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramSector
    selectedBusinesses <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramBusiness
    selectedWorkspaces <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramWorkspace

    services <- runDB $ select $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        unless (null selectedSectors) $ where_ $ x ^. ServiceType `in_` justList (valList selectedSectors)
        unless (null selectedBusinesses) $ where_ $ w ^. WorkspaceBusiness `in_` valList selectedBusinesses
        unless (null selectedWorkspaces) $ where_ $ w ^. WorkspaceId `in_` valList selectedWorkspaces
        orderBy [desc (x ^. ServiceId)]
        return (x,w)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFabAdd <- newIdent
        $(widgetFile "data/services/services")


postServicePhotoDeleR :: ServiceId -> ServicePhotoId -> Handler Html
postServicePhotoDeleR sid fid = do
    ((fr2,_),_) <- runFormPost formPhotoDelete
    case fr2 of
      FormSuccess () -> do
          void $ runDB $ P.delete fid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ ServicePhotosR sid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ ServicePhotoEditR sid fid


formPhotoDelete :: Form ()
formPhotoDelete extra = return (pure (),[whamlet|#{extra}|])


postServicePhotosR :: ServiceId -> Handler Html
postServicePhotosR sid = do

    ((fr,fw),et) <- runFormPost $ formPhotoNew sid Nothing

    case fr of
      FormSuccess (fi,attribution) -> do
          bs <- fileSourceByteString fi
          runDB $ insert_ ServicePhoto { servicePhotoService = sid
                                       , servicePhotoMime = fileContentType fi
                                       , servicePhotoPhoto = bs
                                       , servicePhotoAttribution = attribution
                                       }
          redirect $ DataR $ ServicePhotosR sid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormNew <- newIdent
              $(widgetFile "data/services/photos/new")


getServicePhotoNewR :: ServiceId -> Handler Html
getServicePhotoNewR sid = do

    (fw,et) <- generateFormPost $ formPhotoNew sid Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idFormNew <- newIdent
        $(widgetFile "data/services/photos/new")


formPhotoNew :: ServiceId -> Maybe (Entity ServicePhoto) -> Form (FileInfo,Maybe Html)
formPhotoNew sid photo extra = do

    msgr <- getMessageRender
    
    (photoR,photoV) <- mreq fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing
    
    (attribR,attribV) <- md3mopt md3htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAttribution)]
        } (servicePhotoAttribution . entityVal <$> photo)

    let r = (,) <$> photoR <*> attribR

    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    
    let w = $(widgetFile "data/services/photos/form")
    return (r,w)


postServicePhotoR :: ServiceId -> ServicePhotoId -> Handler Html
postServicePhotoR sid fid = do
    
    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        return x

    ((fr,fw),et) <- runFormPost $ formPhoto sid photo

    case fr of
      FormSuccess (Just fi,attribution) -> do
          bs <- fileSourceByteString fi
          runDB $ update $ \x -> do
              set x [ ServicePhotoMime =. val (fileContentType fi)
                    , ServicePhotoPhoto =. val bs
                    , ServicePhotoAttribution =. val attribution
                    ]
              where_ $ x ^. ServicePhotoId ==. val fid
          redirect $ DataR $ ServicePhotosR sid
          
      FormSuccess (Nothing,attribution) -> do
          runDB $ update $ \x -> do
              set x [ ServicePhotoAttribution =. val attribution ]
              where_ $ x ^. ServicePhotoId ==. val fid
          redirect $ DataR $ ServicePhotosR sid
              
      _otherwise -> do
          (fw2,et2) <- generateFormPost formPhotoDelete
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormEdit <- newIdent
              $(widgetFile "data/services/photos/edit")


getServicePhotoEditR :: ServiceId -> ServicePhotoId -> Handler Html
getServicePhotoEditR sid fid = do

    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        return x
        
    (fw2,et2) <- generateFormPost formPhotoDelete
    
    (fw,et) <- generateFormPost $ formPhoto sid photo
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idFormEdit <- newIdent
        $(widgetFile "data/services/photos/edit")
    


formPhoto :: ServiceId -> Maybe (Entity ServicePhoto) -> Form (Maybe FileInfo,Maybe Html)
formPhoto sid photo extra = do

    msgr <- getMessageRender
    
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing
    
    (attribR,attribV) <- md3mopt md3htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAttribution)]
        } (servicePhotoAttribution . entityVal <$> photo)

    let r = (,) <$> photoR <*> attribR

    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    
    let w = $(widgetFile "data/services/photos/form")
    return (r,w)


getServicePhotosR :: ServiceId -> Handler Html
getServicePhotosR sid = do

    stati <- reqGetParams <$> getRequest

    photos <- runDB $ select $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoService ==. val sid
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idTabPhotos <- newIdent
        idPanelPhotos <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/services/photos/photos")


getServicePhotoDefaultR :: ServiceId -> Handler TypedContent
getServicePhotoDefaultR sid = do
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


getServicePhotoR :: ServiceId -> ServicePhotoId -> Handler TypedContent
getServicePhotoR sid fid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        where_ $ x ^. ServicePhotoService ==. val sid
        return x
    case photo of
      Just (Entity _ (ServicePhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg


widgetChips :: Widget
widgetChips = do
    stati <- reqGetParams <$> getRequest
    
    let inputSectors = filter (\(x,_) -> x == paramSector) stati
        selectedSectors = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputSectors

    sectors <- liftHandler $ runDB $ select $ do
        x <- from $ table @Sector
        where_ $ isNothing_ $ x ^. SectorParent
        orderBy [asc (x ^. SectorName)]
        return x

    let inputBusinesses = filter (\(x,_) -> x == paramBusiness) stati
        selectedBusinesses = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputBusinesses

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

    let toMap = M.fromListWith (<>) . (second (:[]) <$>) . (bimap unValue unValue <$>)
    
    businessWorkspaces <- liftHandler $ toMap <$> runDB ( select $ do
        x :& b <- from $ table @Workspace
            `innerJoin` table @Business `on` (\(x :& b) -> x ^. WorkspaceBusiness ==. b ^. BusinessId)
        unless (null selectedBusinesses) $ where_ $ x ^. WorkspaceBusiness `in_` valList selectedBusinesses
        when (null selectedBusinesses) $ where_ $ val False
        return (b ^. BusinessId,x ^. WorkspaceId) )

    let paramsWokspaces bid = (\x -> (paramWorkspace,pack $ show $ fromSqlKey x))
            <$> filter (\x -> x `notElem` M.findWithDefault [] bid businessWorkspaces) selectedWorkspaces

    
    scrollX1 <- fromMaybe 0 . (readMaybe @Double . unpack =<<) <$> runInputGet (iopt textField paramX1)
    scrollX2 <- fromMaybe 0 . (readMaybe @Double . unpack =<<) <$> runInputGet (iopt textField paramX2)
    scrollX3 <- fromMaybe 0 . (readMaybe @Double . unpack =<<) <$> runInputGet (iopt textField paramX3)
    
    idFilterChips <- newIdent
    idDetailsSectors <- newIdent
    idChipSetSectors <- newIdent
    idChipSetBusinesses <- newIdent
    idChipSetWorkspaces <- newIdent
    
    $(widgetFile "data/services/widgets/chips")



paramSector :: Text
paramSector = "t"

paramBusiness :: Text
paramBusiness = "b"

paramWorkspace :: Text
paramWorkspace = "w"


paramX3 :: Text
paramX3 = "x3"

paramX2 :: Text
paramX2 = "x2"

paramX1 :: Text
paramX1 = "x1"
