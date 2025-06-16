{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Data.Business
  ( getDataBusinessesR, postDataBusinessesR
  , getDataBusinessNewR  
  , getDataBusinessR, postDataBusinessR
  , getDataBusinessEditR
  , postDataBusinessDeleR
  , getDataBusinessLogoR
  , getDataWorkspacesR, postDataWorkspacesR
  , getDataWorkspaceNewR
  , getDataWorkspaceR, postDataWorkspaceR
  , getDataWorkspaceEditR  
  , postDataWorkspaceDeleR
  , getDataWorkingHoursR
  , getDataWorkingSlotsR, postDataWorkingSlotsR
  , getDataWorkingSlotNewR
  , getDataWorkingSlotR, postDataWorkingSlotR
  , getDataWorkingSlotEditR
  , postDataWorkingSlotDeleR
  , getBusinessServicesR, postBusinessServicesR
  , getBusinessServiceR, postBusinessServiceR
  , getBusinessServiceNewR
  , getBusinessServiceEditR
  , postBusinessServiceDeleR
  , getBusinessServicePhotosR, postBusinessServicePhotosR
  , getBusinessServicePhotoNewR
  , getBusinessServicePhotoEditR
  , postBusinessServicePhotoR
  , postBusinessServicePhotoDeleR
  , getBusinessServiceAssignmentsR, postBusinessServiceAssignmentsR
  , getBusinessServiceAssignmentR, postBusinessServiceAssignmentR
  , getBusinessServiceAssignmentNewR
  , getBusinessServiceAssignmentEditR
  , postBusinessServiceAssignmentDeleR
  , getPayOptionsR, postPayOptionsR
  , getPayOptionR, postPayOptionR
  , getPayOptionNewR
  , getPayOptionEditR
  , postPayOptionDeleR
  , getWorkspaceServicesR, postWorkspaceServicesR
  , getWorkspaceServiceR, postWorkspaceServiceR
  , getWorkspaceServiceNewR
  , getWorkspaceServiceEditR
  , postWorkspaceServiceDeleR
  , getWorkspaceServicePhotosR, postWorkspaceServicePhotosR
  , getWorkspaceServicePhotoEditR
  , getWorkspaceServicePhotoNewR
  , getWorkspaceServicePhotoR, postWorkspaceServicePhotoR
  , postWorkspaceServicePhotoDeleR
  , getWorkspaceServiceAssignmentsR, postWorkspaceServiceAssignmentsR
  , getWorkspaceServiceAssignmentNewR
  , getWorkspaceServiceAssignmentR, postWorkspaceServiceAssignmentR
  , getWorkspaceServiceAssignmentEditR
  , postWorkspaceServiceAssignmentDeleR
  ) where

import Control.Monad (join, void)

import Data.Bifunctor (first, second)

import qualified Data.Map as M (Map, fromListWith, member, notMember, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
    ( UTCTime(utctDay), getCurrentTime, LocalTime (LocalTime)
    , DayPeriod (periodLastDay), diffLocalTime, nominalDiffTimeToSeconds
    , secondsToNominalDiffTime, utcToLocalTime, utc, localTimeToUTC
    )
import Data.Time.Calendar
    ( DayOfWeek (Monday), DayPeriod (periodFirstDay)
    , weekFirstDay, addDays, toGregorian, Day
    )
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, leftJoin, on, update, set
    , (^.), (?.), (==.), (=.), (:&)((:&))
    , Value (unValue), desc, selectOne, where_, val, between, leftJoin, just
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (delete)
    , entityVal, entityKey, insert, insert_, replace, upsert
    )
import qualified Database.Persist as P ((=.), delete, replace)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetAccount, widgetMainMenu
    , Route (DataR, StaticR)
    , DataR
      ( DataBusinessesR, DataBusinessNewR, DataBusinessR, DataBusinessEditR
      , DataBusinessDeleR, DataBusinessLogoR, DataWorkspacesR, DataWorkspaceNewR
      , DataWorkspaceR, DataWorkspaceEditR, DataWorkspaceDeleR, DataWorkingHoursR
      , DataWorkingSlotsR, DataWorkingSlotNewR, DataWorkingSlotR
      , DataWorkingSlotEditR, DataWorkingSlotDeleR, EmployeePhotoR
      , PayOptionsR, PayOptionR, PayOptionNewR, PayOptionEditR, PayOptionDeleR
      , WorkspaceServicesR, WorkspaceServiceR, WorkspaceServiceNewR, ServicePhotoDefaultR
      , WorkspaceServiceEditR, WorkspaceServiceDeleR, WorkspaceServicePhotosR
      , WorkspaceServiceAssignmentsR, ServicePhotoR, WorkspaceServicePhotoEditR
      , WorkspaceServicePhotoNewR, WorkspaceServiceAssignmentNewR, WorkspaceServicePhotoDeleR
      , WorkspaceServiceAssignmentR, WorkspaceServicePhotoR, WorkspaceServiceAssignmentEditR
      , WorkspaceServiceAssignmentDeleR
      , BusinessServicesR, BusinessServiceR, BusinessServiceNewR, BusinessServiceEditR
      , BusinessServiceDeleR, BusinessServiceAssignmentsR, BusinessServicePhotosR
      , BusinessServicePhotoNewR, BusinessServicePhotoEditR, BusinessServicePhotoR
      , BusinessServicePhotoDeleR
      , BusinessServiceAssignmentR, BusinessServiceAssignmentNewR
      , BusinessServiceAssignmentEditR, BusinessServiceAssignmentDeleR
      )
    , AppMessage
      ( MsgBusinesses, MsgThereAreNoDataYet, MsgYouMightWantToAddAFew
      , MsgAdd, MsgOwner, MsgTheName, MsgSave, MsgCancel, MsgBusiness
      , MsgBack, MsgDele, MsgEdit, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordDeleted, MsgInvalidFormData, MsgRecordEdited, MsgRecordAdded
      , MsgDetails, MsgWorkspaces, MsgAddress, MsgWorkspace, MsgAlreadyExists
      , MsgCurrency, MsgTimeZone, MsgWorkingHours, MsgStartTime, MsgEndTime
      , MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgDay, MsgSymbolHour, MsgSymbolMinute, MsgPaymentOptions, MsgPayOptions
      , MsgPaymentOption, MsgPayNow, MsgPayAtVenue, MsgType, MsgYooKassa
      , MsgStripe, MsgDescription, MsgPaymentGateway, MsgPayOption, MsgIcon
      , MsgLogotype, MsgLogo, MsgAttribution, MsgBusinessFullName, MsgServices
      , MsgService, MsgUnitMinutes, MsgDuration, MsgAvailable, MsgPrice, MsgPhoto
      , MsgServiceAssignments, MsgServiceAssignment, MsgEmployee, MsgRole
      , MsgAssignmentDate, MsgSchedulingInterval, MsgPriority, MsgTheStart
      , MsgPhotos
      )
    )

import Material3
    ( md3selectField, md3mreq, md3textField, md3textareaField, md3timeField
    , md3mopt, md3htmlField, md3intField, md3switchField, md3datetimeLocalField
    )

import Model
    ( statusError, statusSuccess
    , BusinessId, Business(Business, businessOwner, businessName, businessFullName, businessDescr)
    , User (User, userName, userEmail)
    , WorkspaceId
    , Workspace
      ( Workspace, workspaceName, workspaceAddress, workspaceCurrency, workspaceTzo
      )
    , WorkingHoursId, WorkingHours (WorkingHours, workingHoursStart, workingHoursEnd)
    , PayOptionId
    , PayOption
      ( PayOption, payOptionType, payOptionName, payOptionDescr, payOptionGateway
      , payOptionIcon
      )
    , PayMethod (PayNow, PayAtVenue), PayGateway (PayGatewayStripe, PayGatewayYookassa)
    , BusinessLogo (BusinessLogo)
    , Service
      ( Service, serviceType, serviceDuration, serviceAvailable, servicePrice
      , serviceDescr, serviceName, serviceWorkspace
      )
    , ServiceId, Sector (sectorName)
    , ServicePhotoId
    , ServicePhoto
      ( ServicePhoto, servicePhotoAttribution, servicePhotoService, servicePhotoMime
      , servicePhotoPhoto
      )
    , AssignmentId
    , Assignment
      ( Assignment, assignmentStaff, assignmentRole, assignmentTime, assignmentSlotInterval
      , assignmentPriority
      )
    , StaffId, Staff (Staff, staffName), StaffPhoto
    , EntityField
      ( UserName, UserId, BusinessId, BusinessOwner, WorkspaceId
      , WorkspaceBusiness, BusinessName, WorkspaceName, WorkingHoursWorkspace
      , WorkingHoursDay, WorkingHoursId, PayOptionWorkspace, PayOptionId, PayOptionType
      , PayOptionName, BusinessLogoBusiness, BusinessLogoAttribution, BusinessLogoPhoto
      , BusinessLogoMime, ServiceWorkspace, ServiceName, SectorName, ServicePhotoService
      , ServiceId, AssignmentService, AssignmentStaff, StaffId, StaffPhotoAttribution
      , StaffPhotoStaff, AssignmentId, ServicePhotoId, ServicePhotoMime, ServicePhotoPhoto
      , ServicePhotoAttribution, StaffName, AssignmentRole
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_broken_image_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    , img_add_photo_alternate_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    , img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)
import Text.Printf (printf)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getMessageRender
    , MonadHandler (liftHandler), redirect, whamlet, addMessageI
    , YesodRequest (reqGetParams), getRequest, MonadIO (liftIO)
    , TypedContent (TypedContent), ToContent (toContent)
    , FileInfo (fileContentType), fileSourceByteString
    )
import Yesod.Core.Handler (newIdent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( Field
    , FieldSettings (FieldSettings, fsName, fsLabel, fsTooltip, fsId, fsAttrs)
    , FieldView (fvInput, fvId, fvErrors, fvLabel), FormResult (FormSuccess)
    , Textarea (Textarea), fileField, mreq, mopt
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Fields (optionsPairs)
import Yesod.Persist.Core (runDB)


postBusinessServiceAssignmentDeleR :: BusinessId -> ServiceId -> AssignmentId -> Handler Html
postBusinessServiceAssignmentDeleR bid sid aid = do
    stati <- reqGetParams <$> getRequest
    ((fr,_),_) <- runFormPost formServiceAssignmentDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete aid
          addMessageI statusSuccess MsgRecordDeleted
          redirect (DataR $ BusinessServiceAssignmentsR bid sid,stati)
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (DataR $ BusinessServiceAssignmentR bid sid aid,stati)


getBusinessServiceAssignmentEditR :: BusinessId -> ServiceId -> AssignmentId -> Handler Html
getBusinessServiceAssignmentEditR bid sid aid = do
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
        $(widgetFile "data/business/services/assignments/edit")


getBusinessServiceAssignmentNewR :: BusinessId -> ServiceId -> Handler Html
getBusinessServiceAssignmentNewR bid sid = do
    (fw,et) <- generateFormPost $ formServiceAssignment sid Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idFormAssignment <- newIdent
        $(widgetFile "data/business/services/assignments/new")


postBusinessServiceAssignmentR :: BusinessId -> ServiceId -> AssignmentId -> Handler Html
postBusinessServiceAssignmentR bid sid aid = do
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
          redirect $ DataR $ BusinessServiceAssignmentR bid sid aid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idFormAssignment <- newIdent
              $(widgetFile "data/business/services/assignments/edit")


getBusinessServiceAssignmentR :: BusinessId -> ServiceId -> AssignmentId -> Handler Html
getBusinessServiceAssignmentR bid sid aid = do
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
        $(widgetFile "data/business/services/assignments/assignment")
  where
      toMinutes interval = truncate @_ @Integer (nominalDiffTimeToSeconds interval / 60)


postBusinessServiceAssignmentsR :: BusinessId -> ServiceId -> Handler Html
postBusinessServiceAssignmentsR bid sid = do
    ((fr,fw),et) <- runFormPost $ formServiceAssignment sid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ BusinessServiceAssignmentsR bid sid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idFormAssignment <- newIdent
              $(widgetFile "data/business/services/assignments/new")


getBusinessServiceAssignmentsR :: BusinessId -> ServiceId -> Handler Html
getBusinessServiceAssignmentsR bid sid = do
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
        $(widgetFile "data/business/services/assignments/assignments")


postBusinessServicePhotoDeleR :: BusinessId -> ServiceId -> ServicePhotoId -> Handler Html
postBusinessServicePhotoDeleR bid sid fid = do
    ((fr2,_),_) <- runFormPost formBusinessServicePhotoDelete
    case fr2 of
      FormSuccess () -> do
          void $ runDB $ P.delete fid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ BusinessServicePhotosR bid sid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ BusinessServicePhotoEditR bid sid fid


formBusinessServicePhotoDelete :: Form ()
formBusinessServicePhotoDelete extra = return (pure (),[whamlet|#{extra}|])


postBusinessServicePhotoR :: BusinessId -> ServiceId -> ServicePhotoId -> Handler Html
postBusinessServicePhotoR bid sid fid = do
    
    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        return x

    ((fr,fw),et) <- runFormPost $ formBusinessServicePhoto sid photo

    case fr of
      FormSuccess (Just fi,attribution) -> do
          bs <- fileSourceByteString fi
          runDB $ update $ \x -> do
              set x [ ServicePhotoMime =. val (fileContentType fi)
                    , ServicePhotoPhoto =. val bs
                    , ServicePhotoAttribution =. val attribution
                    ]
              where_ $ x ^. ServicePhotoId ==. val fid
          redirect $ DataR $ BusinessServicePhotosR bid sid
          
      FormSuccess (Nothing,attribution) -> do
          runDB $ update $ \x -> do
              set x [ ServicePhotoAttribution =. val attribution ]
              where_ $ x ^. ServicePhotoId ==. val fid
          redirect $ DataR $ BusinessServicePhotosR bid sid
              
      _otherwise -> do
          (fw2,et2) <- generateFormPost formBusinessServicePhotoDelete
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormEdit <- newIdent
              $(widgetFile "data/business/services/photos/edit")


getBusinessServicePhotoEditR :: BusinessId -> ServiceId -> ServicePhotoId -> Handler Html
getBusinessServicePhotoEditR bid sid fid = do

    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        return x
        
    (fw2,et2) <- generateFormPost formBusinessServicePhotoDelete
    
    (fw,et) <- generateFormPost $ formBusinessServicePhoto sid photo
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idFormEdit <- newIdent
        $(widgetFile "data/business/services/photos/edit")
    

formBusinessServicePhoto :: ServiceId -> Maybe (Entity ServicePhoto) -> Form (Maybe FileInfo,Maybe Html)
formBusinessServicePhoto sid photo extra = do

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
    
    let w = $(widgetFile "data/business/services/photos/form")
    return (r,w)


postBusinessServicePhotosR :: BusinessId -> ServiceId -> Handler Html
postBusinessServicePhotosR bid sid = do

    ((fr,fw),et) <- runFormPost $ formWorkspaceServicePhotoNew sid Nothing

    case fr of
      FormSuccess (fi,attribution) -> do
          bs <- fileSourceByteString fi
          runDB $ insert_ ServicePhoto { servicePhotoService = sid
                                       , servicePhotoMime = fileContentType fi
                                       , servicePhotoPhoto = bs
                                       , servicePhotoAttribution = attribution
                                       }
          redirect $ DataR $ BusinessServicePhotosR bid sid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormNew <- newIdent
              $(widgetFile "data/business/services/photos/new")


getBusinessServicePhotoNewR :: BusinessId -> ServiceId -> Handler Html
getBusinessServicePhotoNewR bid sid = do

    (fw,et) <- generateFormPost $ formWorkspaceServicePhotoNew sid Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idFormNew <- newIdent
        $(widgetFile "data/business/services/photos/new")


getBusinessServicePhotosR :: BusinessId -> ServiceId -> Handler Html
getBusinessServicePhotosR bid sid = do

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
        $(widgetFile "data/business/services/photos/photos")


postBusinessServiceDeleR :: BusinessId -> ServiceId -> Handler Html
postBusinessServiceDeleR bid sid = do
    stati <- reqGetParams <$> getRequest
    ((fr,_),_) <- runFormPost formBusinessServiceDelete
    case fr of
      FormSuccess () -> do
          void $ runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect (DataR $ BusinessServicesR bid,stati)
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (DataR $ BusinessServiceR bid sid,stati)


formBusinessServiceDelete :: Form ()
formBusinessServiceDelete extra = return (pure (), [whamlet|#{extra}|])


postBusinessServiceR :: BusinessId -> ServiceId -> Handler Html
postBusinessServiceR bid sid = do
    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formBusinessService bid service
    case fr of
      FormSuccess r -> do
          runDB $ P.replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ BusinessServiceR bid sid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormService <- newIdent
              $(widgetFile "data/business/services/edit")


getBusinessServiceEditR :: BusinessId -> ServiceId -> Handler Html
getBusinessServiceEditR bid sid = do
    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formBusinessService bid service

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormService <- newIdent
        $(widgetFile "data/business/services/edit")


postBusinessServicesR :: BusinessId -> Handler Html
postBusinessServicesR bid = do
    
    stati <- reqGetParams <$> getRequest
    ((fr,fw),et) <- runFormPost $ formBusinessService bid Nothing
    
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect (DataR $ BusinessServicesR bid,stati)
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormService <- newIdent
              $(widgetFile "data/business/services/new")


getBusinessServiceNewR :: BusinessId -> Handler Html
getBusinessServiceNewR bid = do
    stati <- reqGetParams <$> getRequest
    
    (fw,et) <- generateFormPost $ formBusinessService bid Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormService <- newIdent
        $(widgetFile "data/business/services/new")


formBusinessService :: BusinessId -> Maybe (Entity Service) -> Form Service
formBusinessService bid service extra = do

    msgr <- getMessageRender

    workspaces <- liftHandler $ runDB $ select $ do
        x <- from $ table @Workspace
        where_ $ x ^. WorkspaceBusiness ==. val bid
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


getBusinessServiceR :: BusinessId -> ServiceId -> Handler Html
getBusinessServiceR bid sid = do

    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. ServiceId ==. val sid
        return (x,w)

    (fw2,et2) <- generateFormPost formBusinessServiceDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService 
        idTabDetails <- newIdent
        idPanelDetails <- newIdent
        $(widgetFile "data/business/services/service")


getBusinessServicesR :: BusinessId -> Handler Html
getBusinessServicesR bid = do

    stati <- reqGetParams <$> getRequest

    attribution <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @BusinessLogo
        where_ $ x ^. BusinessLogoBusiness ==. val bid
        return (x ^. BusinessLogoAttribution) )

    services <- runDB $ select $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ w ^. WorkspaceBusiness ==. val bid
        return (x,w)
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idTabServices <- newIdent
        idPanelServices <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/business/services/services")


postWorkspaceServiceAssignmentDeleR :: BusinessId -> WorkspaceId -> ServiceId -> AssignmentId -> Handler Html
postWorkspaceServiceAssignmentDeleR bid wid sid aid = do
    stati <- reqGetParams <$> getRequest
    ((fr,_),_) <- runFormPost formServiceAssignmentDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete aid
          addMessageI statusSuccess MsgRecordDeleted
          redirect (DataR $ WorkspaceServiceAssignmentsR bid wid sid,stati)
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (DataR $ WorkspaceServiceAssignmentR bid wid sid aid,stati)


postWorkspaceServiceAssignmentR :: BusinessId -> WorkspaceId -> ServiceId -> AssignmentId -> Handler Html
postWorkspaceServiceAssignmentR bid wid sid aid = do
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
          redirect $ DataR $ WorkspaceServiceAssignmentR bid wid sid aid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idFormAssignment <- newIdent
              $(widgetFile "data/business/workspaces/services/assignments/edit")


getWorkspaceServiceAssignmentEditR :: BusinessId -> WorkspaceId -> ServiceId -> AssignmentId -> Handler Html
getWorkspaceServiceAssignmentEditR bid wid sid aid = do
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
        $(widgetFile "data/business/workspaces/services/assignments/edit")


getWorkspaceServiceAssignmentR :: BusinessId -> WorkspaceId -> ServiceId -> AssignmentId -> Handler Html
getWorkspaceServiceAssignmentR bid wid sid aid = do
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
        $(widgetFile "data/business/workspaces/services/assignments/assignment")
  where
      toMinutes interval = truncate @_ @Integer (nominalDiffTimeToSeconds interval / 60)


formServiceAssignmentDelete :: Form ()
formServiceAssignmentDelete extra = return (pure (), [whamlet|#{extra}|])


getWorkspaceServiceAssignmentNewR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
getWorkspaceServiceAssignmentNewR bid wid sid = do
    (fw,et) <- generateFormPost $ formServiceAssignment sid Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceAssignment
        idFormAssignment <- newIdent
        $(widgetFile "data/business/workspaces/services/assignments/new")


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


postWorkspaceServiceAssignmentsR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
postWorkspaceServiceAssignmentsR bid wid sid = do
    ((fr,fw),et) <- runFormPost $ formServiceAssignment sid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ WorkspaceServiceAssignmentsR bid wid sid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServiceAssignment
              idFormAssignment <- newIdent
              $(widgetFile "data/business/workspaces/services/assignments/new")


getWorkspaceServiceAssignmentsR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
getWorkspaceServiceAssignmentsR bid wid sid = do
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
        $(widgetFile "data/business/workspaces/services/assignments/assignments")


postWorkspaceServicePhotoDeleR :: BusinessId -> WorkspaceId -> ServiceId -> ServicePhotoId -> Handler Html
postWorkspaceServicePhotoDeleR bid wid sid fid = do
    ((fr2,_),_) <- runFormPost formWorkspaceServicePhotoDelete
    case fr2 of
      FormSuccess () -> do
          void $ runDB $ P.delete fid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ WorkspaceServicePhotosR bid wid sid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ WorkspaceServicePhotoEditR bid wid sid fid


postWorkspaceServicePhotoR :: BusinessId -> WorkspaceId -> ServiceId -> ServicePhotoId -> Handler Html
postWorkspaceServicePhotoR bid wid sid fid = do
    
    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        return x

    ((fr,fw),et) <- runFormPost $ formWorkspaceServicePhoto sid photo

    case fr of
      FormSuccess (Just fi,attribution) -> do
          bs <- fileSourceByteString fi
          runDB $ update $ \x -> do
              set x [ ServicePhotoMime =. val (fileContentType fi)
                    , ServicePhotoPhoto =. val bs
                    , ServicePhotoAttribution =. val attribution
                    ]
              where_ $ x ^. ServicePhotoId ==. val fid
          redirect $ DataR $ WorkspaceServicePhotosR bid wid sid
          
      FormSuccess (Nothing,attribution) -> do
          runDB $ update $ \x -> do
              set x [ ServicePhotoAttribution =. val attribution ]
              where_ $ x ^. ServicePhotoId ==. val fid
          redirect $ DataR $ WorkspaceServicePhotosR bid wid sid
              
      _otherwise -> do
          (fw2,et2) <- generateFormPost formWorkspaceServicePhotoDelete
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormEdit <- newIdent
              $(widgetFile "data/business/workspaces/services/photos/edit")


getWorkspaceServicePhotoR :: BusinessId -> WorkspaceId -> ServiceId -> ServicePhotoId -> Handler TypedContent
getWorkspaceServicePhotoR _bid _wid sid fid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        where_ $ x ^. ServicePhotoService ==. val sid
        return x
    case photo of
      Just (Entity _ (ServicePhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg


getWorkspaceServicePhotoEditR :: BusinessId -> WorkspaceId -> ServiceId -> ServicePhotoId -> Handler Html
getWorkspaceServicePhotoEditR bid wid sid fid = do

    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        return x
        
    (fw2,et2) <- generateFormPost formWorkspaceServicePhotoDelete
    
    (fw,et) <- generateFormPost $ formWorkspaceServicePhoto sid photo
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idFormEdit <- newIdent
        $(widgetFile "data/business/workspaces/services/photos/edit")
    

formWorkspaceServicePhoto :: ServiceId -> Maybe (Entity ServicePhoto) -> Form (Maybe FileInfo,Maybe Html)
formWorkspaceServicePhoto sid photo extra = do

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
    
    let w = $(widgetFile "data/business/workspaces/services/photos/form")
    return (r,w)


formWorkspaceServicePhotoDelete :: Form ()
formWorkspaceServicePhotoDelete extra = return (pure (),[whamlet|#{extra}|])


getWorkspaceServicePhotoNewR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
getWorkspaceServicePhotoNewR bid wid sid = do

    (fw,et) <- generateFormPost $ formWorkspaceServicePhotoNew sid Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idFormNew <- newIdent
        $(widgetFile "data/business/workspaces/services/photos/new")


formWorkspaceServicePhotoNew :: ServiceId -> Maybe (Entity ServicePhoto) -> Form (FileInfo,Maybe Html)
formWorkspaceServicePhotoNew sid photo extra = do

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
    
    let w = $(widgetFile "data/business/workspaces/services/photos/form")
    return (r,w)


postWorkspaceServicePhotosR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
postWorkspaceServicePhotosR bid wid sid = do

    ((fr,fw),et) <- runFormPost $ formWorkspaceServicePhotoNew sid Nothing

    case fr of
      FormSuccess (fi,attribution) -> do
          bs <- fileSourceByteString fi
          runDB $ insert_ ServicePhoto { servicePhotoService = sid
                                       , servicePhotoMime = fileContentType fi
                                       , servicePhotoPhoto = bs
                                       , servicePhotoAttribution = attribution
                                       }
          redirect $ DataR $ WorkspaceServicePhotosR bid wid sid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormNew <- newIdent
              $(widgetFile "data/business/workspaces/services/photos/new")


getWorkspaceServicePhotosR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
getWorkspaceServicePhotosR bid wid sid = do

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
        $(widgetFile "data/business/workspaces/services/photos/photos")


postWorkspaceServiceDeleR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
postWorkspaceServiceDeleR bid wid sid = do
    stati <- reqGetParams <$> getRequest
    ((fr,_),_) <- runFormPost formWorkspaceServiceDelete
    case fr of
      FormSuccess () -> do
          void $ runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect (DataR $ WorkspaceServicesR bid wid,stati)
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (DataR $ WorkspaceServiceR bid wid sid,stati)


getWorkspaceServiceNewR :: BusinessId -> WorkspaceId -> Handler Html
getWorkspaceServiceNewR bid wid = do

    stati <- reqGetParams <$> getRequest

    (fw,et) <- generateFormPost $ formServiceWorkspace wid Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idFormService <- newIdent
        $(widgetFile "data/business/workspaces/services/new")


formServiceWorkspace :: WorkspaceId -> Maybe (Entity Service) -> Form Service
formServiceWorkspace wid service extra = do

    msgr <- getMessageRender

    (nameR,nameV) <- md3mreq (uniqueNameField wid) FieldSettings
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

    let r = Service wid <$> nameR <*> descrR <*> priceR <*> availableR
             <*> ((* 60) . secondsToNominalDiffTime . fromIntegral <$> durationR)
             <*> typeR

    let w = [whamlet|
                    #{extra}
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

      uniqueNameField :: WorkspaceId -> Field Handler Text
      uniqueNameField wid' = checkM (uniqueName wid') md3textField

      uniqueName :: WorkspaceId -> Text -> Handler (Either AppMessage Text)
      uniqueName wid' name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Service
              where_ $ x ^. ServiceWorkspace ==. val wid'
              where_ $ x ^. ServiceName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity sid _) -> case service of
              Nothing -> Left MsgAlreadyExists
              Just (Entity sid' _) | sid == sid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getWorkspaceServiceEditR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
getWorkspaceServiceEditR bid wid sid = do
    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formServiceWorkspace wid service

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFormService <- newIdent
        $(widgetFile "data/business/workspaces/services/edit")


postWorkspaceServiceR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
postWorkspaceServiceR bid wid sid = do
    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formServiceWorkspace wid service
    case fr of
      FormSuccess r -> do
          runDB $ P.replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ WorkspaceServiceR bid wid sid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormService <- newIdent
              $(widgetFile "data/business/workspaces/services/edit")
    

getWorkspaceServiceR :: BusinessId -> WorkspaceId -> ServiceId -> Handler Html
getWorkspaceServiceR bid wid sid = do

    stati <- reqGetParams <$> getRequest

    service <- runDB $ selectOne $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. ServiceId ==. val sid
        where_ $ x ^. ServiceWorkspace ==. val wid
        return (x,w)

    (fw2,et2) <- generateFormPost formWorkspaceServiceDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService 
        idTabDetails <- newIdent
        idPanelDetails <- newIdent
        $(widgetFile "data/business/workspaces/services/service")


formWorkspaceServiceDelete :: Form ()
formWorkspaceServiceDelete extra = return (pure (), [whamlet|#{extra}|])


postWorkspaceServicesR :: BusinessId -> WorkspaceId -> Handler Html
postWorkspaceServicesR bid wid = do
    
    stati <- reqGetParams <$> getRequest
    ((fr,fw),et) <- runFormPost $ formServiceWorkspace wid Nothing
    
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect (DataR $ WorkspaceServicesR bid wid,stati)
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgService
              idFormService <- newIdent
              $(widgetFile "data/business/workspaces/services/new")


getWorkspaceServicesR :: BusinessId -> WorkspaceId -> Handler Html
getWorkspaceServicesR bid wid = do

    stati <- reqGetParams <$> getRequest

    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime

    services <- runDB $ select $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. ServiceWorkspace ==. val wid
        return (x,w)
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idTabServices <- newIdent
        idPanelServices <- newIdent
        idFabAdd <- newIdent 
        $(widgetFile "data/business/workspaces/services/services")
    


postPayOptionDeleR :: BusinessId -> WorkspaceId -> PayOptionId -> Handler Html
postPayOptionDeleR bid wid oid = do

    ((fr,_),_) <- runFormPost formPayOptionDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete oid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ PayOptionsR bid wid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ PayOptionR bid wid oid


formPayOptionDelete :: Form ()
formPayOptionDelete extra = return (pure (), [whamlet|#{extra}|])


getPayOptionEditR :: BusinessId -> WorkspaceId -> PayOptionId -> Handler Html
getPayOptionEditR bid wid oid = do

    option <- runDB $ selectOne $ do
        x <- from $ table @PayOption
        where_ $ x ^. PayOptionId ==. val oid
        return x

    (fw,et) <- generateFormPost $ formPayOption wid option

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentOption
        idFormPayOption <- newIdent
        $(widgetFile "data/business/workspaces/pay/edit")


getPayOptionNewR :: BusinessId -> WorkspaceId -> Handler Html
getPayOptionNewR bid wid = do
    (fw,et) <- generateFormPost $ formPayOption wid Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentOption
        idFormPayOption <- newIdent
        $(widgetFile "data/business/workspaces/pay/new")


formPayOption :: WorkspaceId -> Maybe (Entity PayOption) -> Form PayOption
formPayOption wid option extra = do

    msgr <- getMessageRender

    (typeR,typeV) <- md3mreq (md3selectField (optionsPairs types)) FieldSettings
        { fsLabel = SomeMessage MsgType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgType)]
        } (payOptionType . entityVal <$> option)

    (nameR,nameV) <- md3mreq (uniqueNameField wid typeR) FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (payOptionName . entityVal <$> option)

    (gateR,gateV) <- case typeR of
      FormSuccess PayNow -> first (Just <$>) <$> md3mreq (md3selectField (optionsPairs gates)) FieldSettings
          { fsLabel = SomeMessage MsgPaymentGateway
          , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
          , fsAttrs = [("label", msgr MsgPaymentGateway)]
          } (payOptionGateway . entityVal =<< option)

      _otherwise -> md3mopt (md3selectField (optionsPairs gates)) FieldSettings
          { fsLabel = SomeMessage MsgPaymentGateway
          , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
          , fsAttrs = [("label", msgr MsgPaymentGateway)]
          } (payOptionGateway . entityVal <$> option)

    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgDescription)]
        } (payOptionDescr . entityVal <$> option)

    (iconR,iconV) <- md3mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgIcon
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgIcon)]
        } (payOptionIcon . entityVal <$> option)

    let r = PayOption wid <$> typeR <*> nameR <*> gateR <*> descrR <*> iconR
    let w = [whamlet|
                    #{extra}
                    ^{fvInput typeV}
                    ^{fvInput nameV}
                    ^{fvInput gateV}
                    ^{fvInput descrV}
                    ^{fvInput iconV}
                    |]
    return (r,w)
  where

      types = [(MsgPayNow,PayNow),(MsgPayAtVenue,PayAtVenue)]
      gates = [(MsgStripe,PayGatewayStripe),(MsgYooKassa,PayGatewayYookassa)]

      uniqueNameField :: WorkspaceId -> FormResult PayMethod -> Field Handler Text
      uniqueNameField wid' typ = checkM (uniqueName wid' typ) md3textField

      uniqueName :: WorkspaceId -> FormResult PayMethod -> Text -> Handler (Either AppMessage Text)
      uniqueName wid' typ name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @PayOption
              where_ $ x ^. PayOptionWorkspace ==. val wid'
              case typ of
                FormSuccess typ' -> where_ $ x ^. PayOptionType ==. val typ'
                _otherwise -> where_ $ val True
              where_ $ x ^. PayOptionName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity oid _) -> case option of
              Nothing -> Left MsgAlreadyExists
              Just (Entity oid' _) | oid == oid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


postPayOptionR :: BusinessId -> WorkspaceId -> PayOptionId -> Handler Html
postPayOptionR bid wid oid = do

    option <- runDB $ selectOne $ do
        x <- from $ table @PayOption
        where_ $ x ^. PayOptionId ==. val oid
        return x

    ((fr,fw),et) <- runFormPost $ formPayOption wid option
    case fr of
      FormSuccess r -> do
          runDB $ replace oid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ PayOptionR bid wid oid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idFormPayOption <- newIdent
              $(widgetFile "data/business/workspaces/pay/edit")


getPayOptionR :: BusinessId -> WorkspaceId -> PayOptionId -> Handler Html
getPayOptionR bid wid oid = do

    option <- runDB $ selectOne $ do
        x :& w :& b <- from $ table @PayOption
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. PayOptionWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
        where_ $ x ^. PayOptionId ==. val oid
        return (x,w,b)

    (fw2,et2) <- generateFormPost formPayOptionDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentOption
        $(widgetFile "data/business/workspaces/pay/option")


postPayOptionsR :: BusinessId -> WorkspaceId -> Handler Html
postPayOptionsR bid wid = do
    ((fr,fw),et) <- runFormPost $ formPayOption wid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ PayOptionsR bid wid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idFormPayOption <- newIdent
              $(widgetFile "data/business/workspaces/pay/new")


getPayOptionsR :: BusinessId -> WorkspaceId -> Handler Html
getPayOptionsR bid wid = do

    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime

    options <- runDB $ select $ do
        x <- from $ table @PayOption
        where_ $ x ^. PayOptionWorkspace ==. val wid
        orderBy [asc (x ^. PayOptionId)]
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentOptions
        idTabPayOptions <- newIdent
        idPanelPayOptions <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/business/workspaces/pay/options")


postDataWorkingSlotDeleR :: BusinessId -> WorkspaceId -> Day -> WorkingHoursId -> Handler Html
postDataWorkingSlotDeleR bid wid day sid = do

    ((fr,_),_) <- runFormPost formWorkingSlotDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete bid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ DataWorkingSlotsR bid wid day
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ DataWorkingSlotR bid wid day sid


postDataWorkingSlotR :: BusinessId -> WorkspaceId -> Day -> WorkingHoursId -> Handler Html
postDataWorkingSlotR bid wid day sid = do

    slot <- runDB $ selectOne $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formWorkSlot wid day slot

    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DataWorkingSlotsR bid wid day
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkingHours
              idFormWorkSlot <- newIdent
              $(widgetFile "data/business/workspaces/hours/slots/edit")


getDataWorkingSlotEditR :: BusinessId -> WorkspaceId -> Day -> WorkingHoursId -> Handler Html
getDataWorkingSlotEditR bid wid day sid = do

    slot <- runDB $ selectOne $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formWorkSlot wid day slot

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idFormWorkSlot <- newIdent
        $(widgetFile "data/business/workspaces/hours/slots/edit")


postDataWorkingSlotsR :: BusinessId -> WorkspaceId -> Day -> Handler Html
postDataWorkingSlotsR bid wid day = do

    ((fr,fw),et) <- runFormPost $ formWorkSlot wid day Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ DataWorkingSlotsR bid wid day
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkingHours
              idFormWorkSlot <- newIdent
              $(widgetFile "data/business/workspaces/hours/slots/new")


getDataWorkingSlotNewR :: BusinessId -> WorkspaceId -> Day -> Handler Html
getDataWorkingSlotNewR bid wid day = do

    (fw,et) <- generateFormPost $ formWorkSlot wid day Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idFormWorkSlot <- newIdent
        $(widgetFile "data/business/workspaces/hours/slots/new")


formWorkSlot :: WorkspaceId -> Day -> Maybe (Entity WorkingHours) -> Form WorkingHours
formWorkSlot wid day slot extra = do

    msgr <- getMessageRender

    (startR,startV) <- md3mreq md3timeField FieldSettings
        { fsLabel = SomeMessage MsgStartTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgStartTime)]
        } (workingHoursStart . entityVal <$> slot)

    (endR,endV) <- md3mreq md3timeField FieldSettings
        { fsLabel = SomeMessage MsgEndTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgEndTime)]
        } (workingHoursEnd . entityVal <$> slot)

    let r = WorkingHours wid day <$> startR <*> endR
    let w = [whamlet|#{extra} ^{fvInput startV} ^{fvInput endV}|]
    return (r,w)


getDataWorkingSlotR :: BusinessId -> WorkspaceId -> Day -> WorkingHoursId -> Handler Html
getDataWorkingSlotR bid wid day sid = do

    slot <- runDB $ selectOne $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursId ==. val sid
        return x

    (fw2,et2) <- generateFormPost formWorkingSlotDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        $(widgetFile "data/business/workspaces/hours/slots/slot")


formWorkingSlotDelete :: Form ()
formWorkingSlotDelete extra = return (pure (), [whamlet|#{extra}|])


getDataWorkingSlotsR :: BusinessId -> WorkspaceId -> Day -> Handler Html
getDataWorkingSlotsR bid wid day = do

    stati <- reqGetParams <$> getRequest

    slots <- runDB $ select $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursWorkspace ==. val wid
        where_ $ x ^. WorkingHoursDay ==. val day
        return x

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idFabAdd <- newIdent
        $(widgetFile "data/business/workspaces/hours/slots/slots")


getDataWorkingHoursR :: BusinessId -> WorkspaceId -> Month -> Handler Html
getDataWorkingHoursR bid wid month = do

    stati <- reqGetParams <$> getRequest

    let mapper (Entity _ (WorkingHours _ day start end)) = (day,diffLocalTime (LocalTime day end) (LocalTime day start))

    hours <- groupByKey mapper <$> runDB ( select $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursWorkspace ==. val wid
        where_ $ x ^. WorkingHoursDay `between` (val $ periodFirstDay month, val $ periodLastDay month)
        return x ) :: Handler (M.Map Day NominalDiffTime)

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idTabWorkingHours <- newIdent
        idPanelWorkingHours <- newIdent
        idCalendarPage <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/business/workspaces/hours/hours")
  where

      groupByKey :: (Ord k, Num a) => (v -> (k,a)) -> [v] -> M.Map k a
      groupByKey key = M.fromListWith (+) . fmap key

      rest diff = mod (div (truncate @_ @Integer $ nominalDiffTimeToSeconds diff) 60) 60


postDataWorkspaceDeleR :: BusinessId -> WorkspaceId -> Handler Html
postDataWorkspaceDeleR bid wid = do

    ((fr,_),_) <- runFormPost formWorkspaceDelete

    case fr of
      FormSuccess () -> do
          runDB $ delete wid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ DataWorkspacesR bid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ DataWorkspaceR bid wid


postDataWorkspaceR :: BusinessId -> WorkspaceId -> Handler Html
postDataWorkspaceR bid wid = do

    workspace <- runDB $ selectOne $ do
        x <- from $ table @Workspace
        where_ $ x ^. WorkspaceId ==. val wid
        return x

    ((fr,fw),et) <- runFormPost $ formWorkspace bid workspace

    case fr of
      FormSuccess r -> do
          runDB $ replace wid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DataWorkspaceR bid wid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkspace
              idFormWorkspace <- newIdent
              $(widgetFile "data/business/workspaces/edit")


getDataWorkspaceEditR :: BusinessId -> WorkspaceId -> Handler Html
getDataWorkspaceEditR bid wid = do

    workspace <- runDB $ selectOne $ do
        x <- from $ table @Workspace
        where_ $ x ^. WorkspaceId ==. val wid
        return x

    (fw,et) <- generateFormPost $ formWorkspace bid workspace

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idFormWorkspace <- newIdent
        $(widgetFile "data/business/workspaces/edit")


getDataWorkspaceR :: BusinessId -> WorkspaceId -> Handler Html
getDataWorkspaceR bid wid = do

    workspace <- runDB $ selectOne $ do
        x :& b :& o <- from $ table @Workspace
            `innerJoin` table @Business `on` (\(x :& b) -> x ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @User `on` (\(_ :& b :& o) -> b ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. WorkspaceId ==. val wid
        return (x,b,o)

    (fw2,et2) <- generateFormPost formWorkspaceDelete

    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idTabDetails <- newIdent
        idPanelDetails <- newIdent
        $(widgetFile "data/business/workspaces/workspace")


formWorkspaceDelete :: Form ()
formWorkspaceDelete extra = return (pure (), [whamlet|#{extra}|])


postDataWorkspacesR :: BusinessId -> Handler Html
postDataWorkspacesR bid = do

    ((fr,fw),et) <- runFormPost $ formWorkspace bid Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DataWorkspacesR bid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkspace
              idFormWorkspace <- newIdent
              $(widgetFile "data/business/workspaces/new")


getDataWorkspaceNewR :: BusinessId -> Handler Html
getDataWorkspaceNewR bid = do

    (fw,et) <- generateFormPost $ formWorkspace bid Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idFormWorkspace <- newIdent
        $(widgetFile "data/business/workspaces/new")


formWorkspace :: BusinessId -> Maybe (Entity Workspace) -> Form Workspace
formWorkspace bid workspace extra = do

    msgr <- getMessageRender

    (nameR, nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (workspaceName . entityVal <$> workspace)

    (addrR, addrV) <- md3mreq md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAddress)]
        } (workspaceAddress . entityVal <$> workspace)

    (tzoR, tzoV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgTimeZone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTimeZone)]
        } (pack . show . workspaceTzo . entityVal <$> workspace)

    (currencyR, currencyV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgCurrency
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgCurrency)]
        } (workspaceCurrency . entityVal <$> workspace)

    return ( Workspace bid <$> nameR <*> addrR <*> (read . unpack <$> tzoR) <*> currencyR
           , [whamlet|#{extra} ^{fvInput nameV} ^{fvInput addrV} ^{fvInput tzoV} ^{fvInput currencyV}|]
           )
  where

      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Workspace
              where_ $ x ^. WorkspaceBusiness ==. val bid
              where_ $ x ^. WorkspaceName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity wid _) -> case workspace of
              Nothing -> Left MsgAlreadyExists
              Just (Entity wid' _) | wid == wid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getDataWorkspacesR :: BusinessId -> Handler Html
getDataWorkspacesR bid = do

    workspaces <- runDB $ select $ do
        x <- from $ table @Workspace
        where_ $ x ^. WorkspaceBusiness ==. val bid
        orderBy [desc (x ^. WorkspaceId)]
        return x

    attribution <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @BusinessLogo
        where_ $ x ^. BusinessLogoBusiness ==. val bid
        return (x ^. BusinessLogoAttribution) )

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspaces
        idTabWorkspaces <- newIdent
        idPanelWorkspaces <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/business/workspaces/workspaces")


postDataBusinessDeleR :: BusinessId -> Handler Html
postDataBusinessDeleR bid = do
    ((fr,_),_) <- runFormPost formBusinessDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete bid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR DataBusinessesR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ DataBusinessR bid


postDataBusinessR :: BusinessId -> Handler Html
postDataBusinessR bid = do

    business <- runDB $ selectOne $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessId ==. val bid
        return x

    ((fr,fw),et) <- runFormPost $ formBusiness business

    case fr of
      FormSuccess (r,Just fi,attrib) -> do
          runDB $ replace bid r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (BusinessLogo bid (fileContentType fi) bs attrib)
              [ BusinessLogoMime P.=. fileContentType fi
              , BusinessLogoPhoto P.=. bs
              , BusinessLogoAttribution P.=. attrib
              ]
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DataBusinessR bid
      FormSuccess (r,Nothing,attrib) -> do
          runDB $ replace bid r
          runDB $ update $ \x -> do
                set x [ BusinessLogoAttribution =. val attrib ]
                where_ $ x ^. BusinessLogoBusiness ==. val bid
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DataBusinessR bid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idFormBusiness <- newIdent
              $(widgetFile "data/business/edit")


getDataBusinessEditR :: BusinessId -> Handler Html
getDataBusinessEditR bid = do

    business <- runDB $ selectOne $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessId ==. val bid
        return x

    (fw,et) <- generateFormPost $ formBusiness business

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idFormBusiness <- newIdent
        $(widgetFile "data/business/edit")


getDataBusinessR :: BusinessId -> Handler Html
getDataBusinessR bid = do

    business <- runDB $ selectOne $ do
        x :& o <- from $ table @Business
            `innerJoin` table @User `on` (\(x :& o) -> x ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. BusinessId ==. val bid
        return (x,o)

    attribution <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @BusinessLogo
        where_ $ x ^. BusinessLogoBusiness ==. val bid
        return (x ^. BusinessLogoAttribution) )

    (fw2,et2) <- generateFormPost formBusinessDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idTabDetails <- newIdent
        idPanelDetails <- newIdent
        $(widgetFile "data/business/business")


formBusinessDelete :: Form ()
formBusinessDelete extra = return (pure (), [whamlet|#{extra}|])


postDataBusinessesR :: Handler Html
postDataBusinessesR = do
    ((fr,fw),et) <- runFormPost $ formBusiness Nothing
    case fr of
      FormSuccess (r,Just fi,attrib) -> do
          bid <- runDB $ insert r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (BusinessLogo bid (fileContentType fi) bs attrib)
                    [ BusinessLogoMime P.=. fileContentType fi
                    , BusinessLogoPhoto P.=. bs
                    , BusinessLogoAttribution P.=. attrib
                    ]
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR DataBusinessesR
      FormSuccess (r,_,_) -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR DataBusinessesR
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idFormBusiness <- newIdent
              $(widgetFile "data/business/new")


getDataBusinessNewR :: Handler Html
getDataBusinessNewR = do
    (fw,et) <- generateFormPost $ formBusiness Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idFormBusiness <- newIdent
        $(widgetFile "data/business/new")


formBusiness :: Maybe (Entity Business) -> Form (Business,Maybe FileInfo,Maybe Html)
formBusiness business extra = do

    owners <- liftHandler $ runDB ( select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserName)]
        return x )

    msgr <- getMessageRender

    (ownerR, ownerV) <- md3mreq (md3selectField (optionsPairs (options <$> owners))) FieldSettings
        { fsLabel = SomeMessage MsgOwner
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgOwner)]
        } (businessOwner . entityVal <$> business)

    (nameR, nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (businessName . entityVal <$> business)

    (fullNameR, fullNameV) <- md3mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgBusinessFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgBusinessFullName)]
        } (businessFullName . entityVal <$> business)

    (descrR, descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgDescription)]
        } (businessDescr . entityVal <$> business)

    (logoR,logoV) <- md3mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgLogo
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    attrib <- (unValue =<<) <$> case business of
      Just (Entity bid _) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @BusinessLogo
          where_ $ x ^. BusinessLogoBusiness ==. val bid
          return $ x ^. BusinessLogoAttribution
      Nothing -> return Nothing

    (attribR,attribV) <- md3mopt md3htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAttribution)]
        } (Just attrib)

    idLabelLogo <- newIdent
    idFigureLogo <- newIdent
    idImgLogo <- newIdent

    return ( (,,) <$> (Business <$> ownerR <*> nameR <*> fullNameR <*> descrR) <*> logoR <*> attribR
           , $(widgetFile "data/business/form")
           )
  where
      options e = (fromMaybe (userEmail . entityVal $ e) (userName . entityVal $ e), entityKey e)

      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Business
              where_ $ x ^. BusinessName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity bid _) -> case business of
              Nothing -> Left MsgAlreadyExists
              Just (Entity bid' _) | bid == bid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getDataBusinessesR :: Handler Html
getDataBusinessesR = do

    businesses <- (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& l <- from $ table @Business
            `leftJoin` table @BusinessLogo `on` (\(x :& l) -> just (x ^. BusinessId) ==. l ?. BusinessLogoBusiness)
        orderBy [desc (x ^. BusinessId)]
        return (x, l ?. BusinessLogoAttribution) )

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusinesses
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/business/businesses")


getDataBusinessLogoR :: BusinessId -> Handler TypedContent
getDataBusinessLogoR bid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @BusinessLogo
        where_ $ x ^. BusinessLogoBusiness ==. val bid
        return x
    case photo of
      Just (Entity _ (BusinessLogo _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_broken_image_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
