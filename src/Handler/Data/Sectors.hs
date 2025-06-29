{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Data.Sectors
  ( getSectorsR, postSectorsR
  , getSectorR, postSectorR
  , getSectorNewR
  , getSectorEditR
  , postSectorDeleR
  , getSectorServicesR, postSectorServicesR
  , getSectorServiceR, postSectorServiceR
  , getSectorServiceNewR
  , getSectorServiceEditR
  , postSectorServiceDeleR
  , getSectorServicePhotosR, postSectorServicePhotosR
  , postSectorServicePhotoR
  , getSectorServicePhotoNewR
  , getSectorServicePhotoEditR
  , postSectorServicePhotoDeleR
  , getSectorServiceAssignmentsR, postSectorServiceAssignmentsR
  , getSectorServiceAssignmentR, postSectorServiceAssignmentR
  , getSectorServiceAssignmentNewR
  , getSectorServiceAssignmentEditR
  , postSectorServiceAssignmentDeleR
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void, join)

import Data.Bifunctor (Bifunctor(bimap, second))
import qualified Data.List.Safe as LS (last)
import Data.Maybe (isJust) 
import Data.Text (Text)
import Data.Time (utcToLocalTime, utc, localTimeToUTC)
import Data.Time.Clock (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, val, selectOne, where_, on, desc
    , (^.), (?.), (==.), (:&) ((:&)), (=.)
    , Value (Value, unValue), leftJoin, just, isNothing_, leftJoin, innerJoin
    , update, set
    )
import Database.Persist
    ( Entity (Entity), entityKey, entityVal, insert_)
import qualified Database.Persist as P (delete, replace)

import Foundation
    ( Handler, Form, widgetMainMenu, widgetAccount, widgetSnackbar, widgetEmpty
    , Route (DataR, StaticR)
    , DataR
      ( SectorsR, SectorR, SectorNewR, SectorEditR, SectorDeleR, SectorServicesR
      , ServicePhotoDefaultR, SectorServiceNewR, SectorServiceR
      , SectorServiceEditR, SectorServiceDeleR
      , SectorServicePhotosR, SectorServicePhotoR
      , ServicePhotoR, SectorServicePhotoNewR, SectorServicePhotoEditR
      , SectorServicePhotoDeleR, EmployeePhotoR
      , SectorServiceAssignmentsR, SectorServiceAssignmentR
      , SectorServiceAssignmentNewR, SectorServiceAssignmentEditR
      , SectorServiceAssignmentDeleR
      )
    , AppMessage
      ( MsgSectors, MsgAdd
      , MsgSector, MsgBack, MsgSave, MsgCancel, MsgAlreadyExists, MsgPhotos
      , MsgTheName, MsgDescription, MsgRecordAdded, MsgRecordDeleted
      , MsgInvalidFormData, MsgEdit, MsgDetails, MsgDeleteAreYouSure
      , MsgDele, MsgConfirmPlease, MsgSubsectors, MsgRecordEdited
      , MsgGroup, MsgServices, MsgWorkspace, MsgPrice, MsgAvailable, MsgDuration
      , MsgUnitMinutes, MsgService, MsgServiceAssignments, MsgServiceAssignment
      , MsgPhoto, MsgAttribution, MsgTheStart, MsgSchedulingInterval, MsgRole
      , MsgPriority, MsgEmployee, MsgBusiness, MsgAssignmentDate, MsgUploadPhoto
      )
    )
    
import Material3
    ( md3datetimeLocalField, md3widgetSelect, md3widgetTextarea, md3widgetSwitch
    , md3widget
    )
    
import Model
    ( statusSuccess, statusError, Sectors (Sectors)
    , SectorId, Sector(Sector, sectorName, sectorDescr)
    , Sector(Sector, sectorParent)
    , ServiceId
    , Service
      ( Service, serviceWorkspace, serviceName, serviceDescr, servicePrice
      , serviceAvailable, serviceDuration
      )
    , WorkspaceId, Workspace (Workspace, workspaceName)
    , ServicePhotoId, ServicePhoto (ServicePhoto, servicePhotoAttribution)
    , AssignmentId
    , Assignment
      ( Assignment, assignmentStaff, assignmentRole, assignmentSlotInterval
      , assignmentPriority, assignmentTime
      )
    , Business (Business)
    , StaffId, Staff (Staff, staffName), StaffPhoto
    , EntityField
      ( SectorName, ServiceWorkspace, WorkspaceId, ServiceType, SectorId
      , SectorName, SectorParent, WorkspaceName, ServiceName, ServiceId
      , ServicePhotoService, ServicePhotoId, ServicePhotoMime
      , ServicePhotoPhoto, ServicePhotoAttribution, AssignmentService
      , WorkspaceBusiness, BusinessId, AssignmentStaff, StaffPhotoStaff
      , StaffId, AssignmentId, StaffPhotoAttribution, StaffName, AssignmentRole
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_add_photo_alternate_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, getMessages, newIdent, whamlet
    , SomeMessage (SomeMessage), redirect, addMessageI, getRequest
    , MonadHandler (liftHandler), YesodRequest (reqGetParams)
    , FileInfo (fileContentType), fileSourceByteString
    )
import Yesod.Form
    ( Field, FormResult (FormSuccess)
    , FieldView (fvInput, fvLabel, fvId, fvErrors)
    , FieldSettings
      ( FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs
      )
    )
import Yesod.Form.Fields
    ( optionsPairs, selectField, textField, textareaField, intField, checkBoxField
    , fileField, htmlField
    )
import Yesod.Form.Functions
    ( mopt, mreq, generateFormPost, checkM, runFormPost
    )
import Yesod.Form.Types (fvRequired)
import Yesod.Persist (YesodPersist(runDB))


postSectorServiceAssignmentDeleR :: SectorId -> ServiceId -> AssignmentId -> Sectors -> Handler Html
postSectorServiceAssignmentDeleR gid sid aid ps = do
    stati <- reqGetParams <$> getRequest
    ((fr,_),_) <- runFormPost formServiceAssignmentDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete aid
          addMessageI statusSuccess MsgRecordDeleted
          redirect (DataR $ SectorServiceAssignmentsR gid sid ps,stati)
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (DataR $ SectorServiceAssignmentR gid sid aid ps,stati)


getSectorServiceAssignmentEditR :: SectorId -> ServiceId -> AssignmentId -> Sectors -> Handler Html
getSectorServiceAssignmentEditR gid sid aid ps = do
    stati <- reqGetParams <$> getRequest

    assignment <- runDB $ selectOne $ do
        x <- from $ table @Assignment
        where_ $ x ^. AssignmentId ==. val aid
        return x

    (fw,et) <- generateFormPost $ formServiceAssignment sid assignment

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idHeader <- newIdent
        idMain <- newIdent
        idFormAssignment <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/sectors/services/assignments/edit")


getSectorServiceAssignmentNewR :: SectorId -> ServiceId -> Sectors -> Handler Html
getSectorServiceAssignmentNewR gid sid ps = do
    (fw,et) <- generateFormPost $ formServiceAssignment sid Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idHeader <- newIdent
        idMain <- newIdent
        idFormAssignment <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/sectors/services/assignments/new")


formServiceAssignment :: ServiceId -> Maybe (Entity Assignment) -> Form Assignment
formServiceAssignment sid assignment extra = do

    staff <- liftHandler $ runDB $ select $ do
        x <- from $ table @Staff
        orderBy [asc (x ^. StaffName), desc (x ^. StaffId)]
        return x

    (employeeR, employeeV) <- mreq (selectField (optionsPairs (options <$> staff))) FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (assignmentStaff . entityVal <$> assignment)

    (roleR, roleV) <- mreq (uniqueRoleField sid employeeR) FieldSettings
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

    let r = Assignment <$> employeeR <*> pure sid <*> roleR <*> (localTimeToUTC utc <$> startR)
            <*> ((* 60) . secondsToNominalDiffTime . fromIntegral <$> intervalR)
            <*> priorityR

    let w = [whamlet|
                    #{extra}
                    ^{md3widgetSelect employeeV}
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
      options e = (staffName . entityVal $ e, entityKey e)

      uniqueRoleField :: ServiceId -> FormResult StaffId -> Field Handler Text
      uniqueRoleField sid' eid = checkM (uniqueRole sid' eid) textField

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


postSectorServiceAssignmentR :: SectorId -> ServiceId -> AssignmentId -> Sectors -> Handler Html
postSectorServiceAssignmentR gid sid aid ps = do
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
          redirect $ DataR $ SectorServiceAssignmentR gid sid aid ps
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idHeader <- newIdent
              idMain <- newIdent
              idFormAssignment <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/sectors/services/assignments/edit")


getSectorServiceAssignmentR :: SectorId -> ServiceId -> AssignmentId -> Sectors -> Handler Html
getSectorServiceAssignmentR gid sid aid ps = do
    stati <- reqGetParams <$> getRequest

    assignment <- runDB $ selectOne $ do
        x :& s :& w :& b :& e <- from $ table @Assignment
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @Staff `on` (\(x :& _ :& _ :& _ :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        where_ $ x ^. AssignmentId ==. val aid
        return (x,(e,(s,(w,b))))

    (fw0,et0) <- generateFormPost formServiceAssignmentDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idHeader <- newIdent
        idMain <- newIdent
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idFormDelete <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/sectors/services/assignments/assignment")
  where
      toMinutes interval = truncate @_ @Integer (nominalDiffTimeToSeconds interval / 60)


formServiceAssignmentDelete :: Form ()
formServiceAssignmentDelete extra = return (pure (), [whamlet|#{extra}|])


postSectorServiceAssignmentsR :: SectorId -> ServiceId -> Sectors -> Handler Html
postSectorServiceAssignmentsR gid sid ps = do
    ((fr,fw),et) <- runFormPost $ formServiceAssignment sid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ SectorServiceAssignmentsR gid sid ps
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idHeader <- newIdent
              idMain <- newIdent
              idFormAssignment <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/sectors/services/assignments/new")


getSectorServiceAssignmentsR :: SectorId -> ServiceId -> Sectors -> Handler Html
getSectorServiceAssignmentsR gid sid ps = do
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
        idHeader <- newIdent
        idMain <- newIdent
        classHeadline <- newIdent
        classSupportingText <- newIdent
        classAttribution <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "common/css/rows")
        $(widgetFile "common/css/attribution")        
        $(widgetFile "data/sectors/services/assignments/assignments")


postSectorServicePhotoDeleR :: SectorId -> ServiceId -> ServicePhotoId -> Sectors -> Handler Html
postSectorServicePhotoDeleR gid sid fid ps = do
    ((fr2,_),_) <- runFormPost formPhotoDelete
    case fr2 of
      FormSuccess () -> do
          void $ runDB $ P.delete fid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ SectorServicePhotosR gid sid ps
          
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ SectorServicePhotoEditR gid sid fid ps


postSectorServicePhotoR :: SectorId -> ServiceId -> ServicePhotoId -> Sectors -> Handler Html
postSectorServicePhotoR gid sid fid ps = do
    
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

          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ SectorServicePhotosR gid sid ps
          
      FormSuccess (Nothing,attribution) -> do
          runDB $ update $ \x -> do
              set x [ ServicePhotoAttribution =. val attribution ]
              where_ $ x ^. ServicePhotoId ==. val fid
              
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ SectorServicePhotosR gid sid ps
              
      _otherwise -> do
          (fw0,et0) <- generateFormPost formPhotoDelete
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idHeader <- newIdent
              idMain <- newIdent
              idFormEdit <- newIdent
              idOverlay <- newIdent
              idDialogDelete <- newIdent
              idFormDelete <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/sectors/services/photos/edit")


getSectorServicePhotoEditR :: SectorId -> ServiceId -> ServicePhotoId -> Sectors -> Handler Html
getSectorServicePhotoEditR gid sid fid ps = do

    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        return x
        
    (fw0,et0) <- generateFormPost formPhotoDelete
    
    (fw,et) <- generateFormPost $ formPhoto sid photo
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idHeader <- newIdent
        idMain <- newIdent
        idFormEdit <- newIdent 
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idFormDelete <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/sectors/services/photos/edit")


formPhotoDelete :: Form ()
formPhotoDelete extra = return (pure (),[whamlet|#{extra}|])    


formPhoto :: ServiceId -> Maybe (Entity ServicePhoto) -> Form (Maybe FileInfo,Maybe Html)
formPhoto sid photo extra = do
    
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing
    
    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (servicePhotoAttribution . entityVal <$> photo)

    let r = (,) <$> photoR <*> attribR

    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    
    let w = $(widgetFile "data/sectors/services/photos/form")
    return (r,w)


getSectorServicePhotoNewR :: SectorId -> ServiceId -> Sectors -> Handler Html
getSectorServicePhotoNewR gid sid ps = do

    (fw,et) <- generateFormPost $ formPhotoNew sid Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idHeader <- newIdent
        idMain <- newIdent
        idFormNew <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/sectors/services/photos/new")


formPhotoNew :: ServiceId -> Maybe (Entity ServicePhoto) -> Form (FileInfo,Maybe Html)
formPhotoNew sid photo extra = do
    
    (photoR,photoV) <- mreq fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing
    
    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (servicePhotoAttribution . entityVal <$> photo)

    let r = (,) <$> photoR <*> attribR

    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    
    let w = $(widgetFile "data/sectors/services/photos/form")
    return (r,w)


postSectorServicePhotosR :: SectorId -> ServiceId -> Sectors -> Handler Html
postSectorServicePhotosR gid sid ps = do

    ((fr,fw),et) <- runFormPost $ formPhotoNew sid Nothing

    case fr of
      FormSuccess (fi,attribution) -> do
          bs <- fileSourceByteString fi
          runDB $ insert_ $ ServicePhoto sid (fileContentType fi) bs attribution

          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ SectorServicePhotosR gid sid ps
          
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idHeader <- newIdent
              idMain <- newIdent
              idFormNew <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/sectors/services/photos/new")


getSectorServicePhotosR :: SectorId -> ServiceId -> Sectors -> Handler Html
getSectorServicePhotosR gid sid ps = do

    stati <- reqGetParams <$> getRequest

    photos <- runDB $ select $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoService ==. val sid
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idHeader <- newIdent
        idMain <- newIdent
        classAttribution <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "data/sectors/services/photos/photos")


postSectorServiceDeleR :: SectorId -> ServiceId -> Sectors -> Handler Html
postSectorServiceDeleR gid sid ps = do
    stati <- reqGetParams <$> getRequest
    ((fr,_),_) <- runFormPost formServiceDelete
    case fr of
      FormSuccess () -> do
          void $ runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect (DataR $ SectorServicesR gid ps,stati)
          
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (DataR $ SectorServiceR gid sid ps,stati)


getSectorServiceEditR :: SectorId -> ServiceId -> Sectors -> Handler Html
getSectorServiceEditR gid sid ps = do
    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formService gid service

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idHeader <- newIdent
        idMain <- newIdent
        idFormService <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/sectors/services/edit")


postSectorServiceR :: SectorId -> ServiceId -> Sectors -> Handler Html
postSectorServiceR gid sid ps = do
    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formService gid service
    case fr of
      FormSuccess r -> do
          runDB $ P.replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ SectorServiceR gid sid ps
          
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idHeader <- newIdent
              idMain <- newIdent
              idFormService <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/sectors/services/edit")


getSectorServiceR :: SectorId -> ServiceId -> Sectors -> Handler Html
getSectorServiceR gid sid ps = do

    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. ServiceId ==. val sid
        return (x,w)

    (fw0,et0) <- generateFormPost formServiceDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idHeader <- newIdent
        idMain <- newIdent
        classCurrency <- newIdent
        classDuration <- newIdent
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idFormDelete <- newIdent
        $(widgetFile "common/js/seconds2duration")
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "data/sectors/services/service")


formServiceDelete :: Form ()
formServiceDelete extra = return (pure (), [whamlet|#{extra}|])


getSectorServiceNewR :: SectorId -> Sectors -> Handler Html
getSectorServiceNewR gid ps = do
    stati <- reqGetParams <$> getRequest
    
    (fw,et) <- generateFormPost $ formService gid Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormService <- newIdent
        $(widgetFile "data/sectors/services/new")


formService :: SectorId -> Maybe (Entity Service) -> Form Service
formService gid service extra = do

    workspaces <- liftHandler $ runDB $ select $ do
        x <- from $ table @Workspace
        orderBy [asc (x ^. WorkspaceName), desc (x ^. WorkspaceId)]
        return x

    (workspaceR, workspaceV) <- mreq (selectField (optionsPairs (optionsWorkspace <$> workspaces))) FieldSettings
        { fsLabel = SomeMessage MsgWorkspace
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (serviceWorkspace . entityVal <$> service)

    (nameR,nameV) <- mreq (uniqueNameField workspaceR) FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (serviceName . entityVal <$> service)

    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (serviceDescr . entityVal <$> service)

    (priceR,priceV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (servicePrice . entityVal <$> service)

    (availableR,availableV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAvailable
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (serviceAvailable . entityVal <$> service)

    (durationR,durationV) <- mreq (intField @_ @Integer) FieldSettings
        { fsLabel = SomeMessage MsgDuration
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (truncate . (/ 60) . nominalDiffTimeToSeconds . serviceDuration . entityVal <$> service)

    let r = Service <$> workspaceR <*> nameR <*> descrR <*> priceR <*> availableR
             <*> ((* 60) . secondsToNominalDiffTime . fromIntegral <$> durationR)
             <*> pure (Just gid)

    let w = [whamlet|
                    #{extra}
                    ^{md3widgetSelect workspaceV}
                    ^{md3widget nameV}
                    ^{md3widgetTextarea descrV}
                    ^{md3widget priceV}
                    ^{md3widgetSwitch availableV}
                    
                    <div.field.label.border.round :isJust (fvErrors durationV):.invalid>
                      ^{fvInput durationV}
                      <label for=#{fvId durationV}>
                          #{fvLabel durationV}
                          $if fvRequired durationV
                            <sup>*
                      $maybe err <- fvErrors durationV
                        <span.error>#{err}
                      $nothing
                        <span.helper>_{MsgUnitMinutes}
                      
                    |]
    return (r,w)

  where

      optionsWorkspace e = (workspaceName . entityVal $ e, entityKey e)

      uniqueNameField :: FormResult WorkspaceId -> Field Handler Text
      uniqueNameField wid = checkM (uniqueName wid) textField

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


postSectorServicesR :: SectorId -> Sectors -> Handler Html
postSectorServicesR gid ps = do
    stati <- reqGetParams <$> getRequest
    ((fr,fw),et) <- runFormPost $ formService gid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect (DataR $ SectorServicesR gid ps, stati)
          
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormService <- newIdent
              $(widgetFile "data/sectors/services/new")


getSectorServicesR :: SectorId -> Sectors -> Handler Html
getSectorServicesR gid ps@(Sectors gids) = do

    stati <- reqGetParams <$> getRequest

    services <- runDB $ select $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. ServiceType ==. just (val gid)
        return (x,w)
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idHeader <- newIdent
        idMain <- newIdent
        classHeadline <- newIdent
        classCurrency <- newIdent
        classSupportingText <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "common/css/rows")
        $(widgetFile "data/sectors/services/services")
        

postSectorDeleR :: SectorId -> Sectors -> Handler Html
postSectorDeleR gid ps = do
    ((fr,_),_) <- runFormPost formSectorDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete gid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ SectorsR ps
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ SectorR gid ps


postSectorR :: SectorId -> Sectors -> Handler Html
postSectorR gid ps = do

    sector <- runDB $ selectOne $ do
        x <- from $ table @Sector
        where_ $ x ^. SectorId ==. val gid
        return x

    ((fr,fw),et) <- runFormPost $ formSector Nothing sector
    case fr of
      FormSuccess r -> do
          runDB $ P.replace gid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ SectorR gid ps
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgSector
          idFormSector <- newIdent
          $(widgetFile "data/sectors/edit")


getSectorEditR :: SectorId -> Sectors -> Handler Html
getSectorEditR gid ps = do

    sector <- runDB $ selectOne $ do
        x <- from $ table @Sector
        where_ $ x ^. SectorId ==. val gid
        return x

    (fw,et) <- generateFormPost $ formSector Nothing sector
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSector
        idFormSector <- newIdent
        $(widgetFile "data/sectors/edit")


postSectorsR :: Sectors -> Handler Html
postSectorsR ps = do
    ((fr,fw),et) <- runFormPost $ formSector Nothing Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ SectorsR ps
          
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgSector
          idHeader <- newIdent
          idMain <- newIdent
          idFormSector <- newIdent
          $(widgetFile "data/sectors/new")


getSectorNewR :: Sectors -> Handler Html
getSectorNewR ps@(Sectors gids) = do

    (fw,et) <- generateFormPost $ formSector (LS.last gids) Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSector
        idHeader <- newIdent
        idMain <- newIdent
        idFormSector <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/sectors/new")


formSector :: Maybe SectorId -> Maybe (Entity Sector) -> Form Sector
formSector pid sector extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (sectorName . entityVal <$> sector)

    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (sectorDescr . entityVal <$> sector)

    sectors <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Sector
        orderBy [asc (x ^. SectorName)]
        return (x ^. SectorName, x ^. SectorId) )

    (parentR,parentV) <- mopt (selectField (optionsPairs sectors)) FieldSettings
        { fsLabel = SomeMessage MsgGroup
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } ((sectorParent . entityVal <$> sector) <|> pure pid)

    let r = Sector <$> nameR <*> descrR <*> parentR
    let w = [whamlet|
                    #{extra}
                    ^{md3widget nameV}
                    ^{md3widgetTextarea descrV}
                    ^{md3widgetSelect parentV}
                    |]
    return (r,w)
    
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Sector
              where_ $ x ^. SectorName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity gid _) -> case sector of
              Nothing -> Left MsgAlreadyExists
              Just (Entity gid' _) | gid == gid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getSectorR :: SectorId -> Sectors -> Handler Html
getSectorR gid ps@(Sectors gids) = do

    sector <- runDB $ selectOne $ do
        x :& p <- from $ table @Sector
            `leftJoin` table @Sector `on` (\(x :& p) -> x ^. SectorParent ==. p ?. SectorId)
        where_ $ x ^. SectorId ==. val gid
        return (x,p)

    (fw0,et0) <- generateFormPost formSectorDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSector
        idHeader <- newIdent
        idMain <- newIdent
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idFormDelete <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "data/sectors/sector")


formSectorDelete :: Form ()
formSectorDelete extra = return (FormSuccess (), [whamlet|#{extra}|])


getSectorsR :: Sectors -> Handler Html
getSectorsR ps@(Sectors []) = do

    sectors <- runDB $ select $ do
        x <- from $ table @Sector
        where_ $ isNothing_ $ x ^. SectorParent
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSectors
        idHeader <- newIdent
        idMain <- newIdent
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        classHeadline <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "data/sectors/sectors")
             
getSectorsR ps@(Sectors gids) = do

    sectors <- runDB $ select $ do
        x <- from $ table @Sector
        where_ $ x ^. SectorParent ==. val (LS.last gids)

        let level = val (length gids)
        return (level,x)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSectors
        idHeader <- newIdent
        idMain <- newIdent
        classHeadline <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "data/sectors/subsectors")


keyScrollTop :: Text
keyScrollTop = "scrollTop"
