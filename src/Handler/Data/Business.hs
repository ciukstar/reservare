{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Data.Business
  ( getDataBusinessesR
  , getDataBusinessNewR
  , postDataBusinessesR
  , getDataBusinessR
  , getDataBusinessEditR
  , postDataBusinessDeleR
  , postDataBusinessR
  , getDataWorkspacesR
  , getDataWorkspaceNewR
  , postDataWorkspacesR
  , getDataWorkspaceR
  , getDataWorkspaceEditR
  , postDataWorkspaceR
  , postDataWorkspaceDeleR
  , getDataWorkingHoursR
  , getDataWorkingSlotsR
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, pack)
import Data.Time.Calendar
    ( DayOfWeek (Monday), DayPeriod (periodFirstDay)
    , weekFirstDay, addDays, toGregorian, Day
    )
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.LocalTime (LocalTime(localDay))

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , desc, selectOne, where_, val
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (delete)
    , entityVal, entityKey, insert_, replace
    )

import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR
      ( DataBusinessesR, DataBusinessNewR, DataBusinessR, DataBusinessEditR
      , DataBusinessDeleR, DataWorkspacesR, DataWorkspaceNewR, DataWorkspaceR
      , DataWorkspaceEditR, DataWorkspaceDeleR, DataWorkingHoursR
      , DataWorkingSlotsR
      )
    , AppMessage
      ( MsgBusinesses, MsgThereAreNoDataYet, MsgYouMightWantToAddAFew
      , MsgAdd, MsgOwner, MsgTheName, MsgSave, MsgCancel, MsgBusiness
      , MsgBack, MsgDele, MsgEdit, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordDeleted, MsgInvalidFormData, MsgRecordEdited, MsgRecordAdded
      , MsgDetails, MsgWorkspaces, MsgAddress, MsgWorkspace, MsgAlreadyExists
      , MsgCurrency, MsgTimeZone, MsgWorkingHours
      , MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      )
    )

import Material3 (md3selectField, md3mreq, md3textField, md3textareaField)

import Model
    ( statusError, statusSuccess
    , BusinessId, Business(Business, businessOwner, businessName)
    , User (User, userName, userEmail)
    , WorkspaceId
    , Workspace
      ( Workspace, workspaceName, workspaceAddress, workspaceCurrency, workspaceTzo
      )
    , EntityField
      ( UserName, UserId, BusinessId, BusinessOwner, WorkspaceId
      , WorkspaceBusiness, BusinessName, WorkspaceName, WorkingHoursWorkspace, WorkingHoursDay
      ), WorkingHours (WorkingHours)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Widgets (widgetBanner, widgetSnackbar, widgetAccount, widgetMenu)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getMessageRender
    , MonadHandler (liftHandler), redirect, whamlet, addMessageI
    , YesodRequest (reqGetParams), getRequest, MonadIO (liftIO)
    )
import Yesod.Core.Handler (newIdent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( Field, FieldSettings (FieldSettings, fsName, fsLabel, fsTooltip, fsId, fsAttrs)
    , FieldView (fvInput), FormResult (FormSuccess)
    )
import Yesod.Form.Input (runInputGet, iopt) 
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Fields (optionsPairs, datetimeLocalField)
import Yesod.Persist.Core (runDB)
import Data.Time (UTCTime(utctDay), getCurrentTime)


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
    tid <- runInputGet ( iopt datetimeLocalField "tid" )
    
    hours <- runDB $ select $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursWorkspace ==. val wid
        return x

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idTabWorkingHours <- newIdent
        idPanelWorkingHours <- newIdent
        idCalendarPage <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/business/workspaces/hours/hours")


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
      FormSuccess r -> do
          runDB $ replace bid r
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
      FormSuccess r -> do
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


formBusiness :: Maybe (Entity Business) -> Form Business
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

    return ( Business <$> ownerR <*> nameR
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
    
    businesses <- runDB $ select $ do
        x <- from $ table @Business
        orderBy [desc (x ^. BusinessId)]
        return x
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusinesses
        idFabAdd <- newIdent
        $(widgetFile "data/business/businesses")
