{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Business
  ( getBusinessesR
  , getBusinessNewR
  , postBusinessesR
  , getBusinessR
  , getBusinessEditR
  , postBusinessDeleR
  , postBusinessR
  , getWorkspacesR
  , getWorkspaceNewR
  , postWorkspacesR
  , getWorkspaceR
  , getWorkspaceEditR
  , postWorkspaceR
  , postWorkspaceDeleR
  ) where

import Data.Text (Text, unpack, pack)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , desc, selectOne, where_, val
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (delete)
    , entityVal, insert_, replace
    )

import Foundation
    ( Handler, Form
    , Route
      ( BusinessesR, BusinessNewR, BusinessR, BusinessEditR, BusinessDeleR
      , WorkspacesR, WorkspaceNewR, WorkspaceR, WorkspaceEditR, WorkspaceDeleR
      )
    , AppMessage
      ( MsgBusinesses, MsgThereAreNoDataYet, MsgYouMightWantToAddAFew
      , MsgAdd, MsgOwner, MsgTheName, MsgSave, MsgCancel, MsgBusiness
      , MsgBack, MsgDele, MsgEdit, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordDeleted, MsgInvalidFormData, MsgRecordEdited, MsgRecordAdded
      , MsgDetails, MsgWorkspaces, MsgAddress, MsgWorkspace, MsgAlreadyExists, MsgTimeZone, MsgCurrency
      )
    )

import Material3 (md3mreq, md3textField, md3textareaField)

import Model
    ( statusError, statusSuccess
    , BusinessId, Business(Business, businessName)
    , UserId, User (User)
    , WorkspaceId, Workspace (Workspace, workspaceName, workspaceAddress, workspaceTzo, workspaceCurrency)
    , EntityField
      ( UserId, BusinessId, BusinessOwner, WorkspaceId
      , WorkspaceBusiness, BusinessName, WorkspaceName
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Widgets (widgetBanner, widgetSnackbar, widgetAccount, widgetMenu)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getMessageRender
    , redirect, whamlet, addMessageI
    )
import Yesod.Core.Handler (newIdent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form
    ( Field, FieldSettings (FieldSettings, fsName, fsLabel, fsTooltip, fsId, fsAttrs)
    , FieldView (fvInput), FormResult (FormSuccess)
    )
import Yesod.Persist.Core (runDB)


postWorkspaceDeleR :: UserId -> BusinessId -> WorkspaceId -> Handler Html
postWorkspaceDeleR uid bid wid = do
    
    ((fr,_),_) <- runFormPost formWorkspaceDelete
    
    case fr of
      FormSuccess () -> do
          runDB $ delete wid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ WorkspacesR uid bid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ WorkspaceR uid bid wid


postWorkspaceR :: UserId -> BusinessId -> WorkspaceId -> Handler Html
postWorkspaceR uid bid wid = do

    workspace <- runDB $ selectOne $ do
        x <- from $ table @Workspace
        where_ $ x ^. WorkspaceId ==. val wid
        return x    

    ((fr,fw),et) <- runFormPost $ formWorkspace bid workspace

    case fr of
      FormSuccess r -> do
          runDB $ replace wid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ WorkspaceR uid bid wid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkspace
              idFormWorkspace <- newIdent
              $(widgetFile "business/workspaces/edit")


getWorkspaceEditR :: UserId -> BusinessId -> WorkspaceId -> Handler Html
getWorkspaceEditR uid bid wid = do

    workspace <- runDB $ selectOne $ do
        x <- from $ table @Workspace
        where_ $ x ^. WorkspaceId ==. val wid
        return x

    (fw,et) <- generateFormPost $ formWorkspace bid workspace
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idFormWorkspace <- newIdent
        $(widgetFile "business/workspaces/edit")


getWorkspaceR :: UserId -> BusinessId -> WorkspaceId -> Handler Html
getWorkspaceR uid bid wid = do

    workspace <- runDB $ selectOne $ do
        x :& b :& o <- from $ table @Workspace
            `innerJoin` table @Business `on` (\(x :& b) -> x ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @User `on` (\(_ :& b :& o) -> b ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. WorkspaceId ==. val wid
        return (x,b,o)

    (fw2,et2) <- generateFormPost formWorkspaceDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        $(widgetFile "business/workspaces/workspace")
        

formWorkspaceDelete :: Form ()
formWorkspaceDelete extra = return (pure (), [whamlet|#{extra}|])


postWorkspacesR :: UserId -> BusinessId -> Handler Html
postWorkspacesR uid bid = do

    ((fr,fw),et) <- runFormPost $ formWorkspace bid Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ WorkspacesR uid bid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkspace
              idFormWorkspace <- newIdent
              $(widgetFile "business/workspaces/new")


getWorkspaceNewR :: UserId -> BusinessId -> Handler Html
getWorkspaceNewR uid bid = do

    (fw,et) <- generateFormPost $ formWorkspace bid Nothing   
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idFormWorkspace <- newIdent
        $(widgetFile "business/workspaces/new")


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


getWorkspacesR :: UserId -> BusinessId -> Handler Html
getWorkspacesR uid bid = do

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
        $(widgetFile "business/workspaces/workspaces")


postBusinessDeleR :: UserId -> BusinessId -> Handler Html
postBusinessDeleR uid bid = do
    ((fr,_),_) <- runFormPost formBusinessDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete bid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ BusinessesR uid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ BusinessR uid bid


postBusinessR :: UserId -> BusinessId -> Handler Html
postBusinessR uid bid = do
    
    business <- runDB $ selectOne $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessId ==. val bid
        return x

    ((fr,fw),et) <- runFormPost $ formBusiness uid business

    case fr of
      FormSuccess r -> do
          runDB $ replace bid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ BusinessR uid bid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idFormBusiness <- newIdent
              $(widgetFile "business/edit")


getBusinessEditR :: UserId -> BusinessId -> Handler Html
getBusinessEditR uid bid = do
    
    business <- runDB $ selectOne $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessId ==. val bid
        return x

    (fw,et) <- generateFormPost $ formBusiness uid business
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idFormBusiness <- newIdent
        $(widgetFile "business/edit")


getBusinessR :: UserId -> BusinessId -> Handler Html
getBusinessR uid bid = do
    
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
        $(widgetFile "business/business")


formBusinessDelete :: Form ()
formBusinessDelete extra = return (pure (), [whamlet|#{extra}|])


postBusinessesR :: UserId -> Handler Html
postBusinessesR uid = do
    ((fr,fw),et) <- runFormPost $ formBusiness uid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ BusinessesR uid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idFormBusiness <- newIdent
              $(widgetFile "business/new")


getBusinessNewR :: UserId -> Handler Html
getBusinessNewR uid = do
    (fw,et) <- generateFormPost $ formBusiness uid Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idFormBusiness <- newIdent
        $(widgetFile "business/new")


formBusiness :: UserId -> Maybe (Entity Business) -> Form Business
formBusiness uid business extra = do
    
    msgr <- getMessageRender
        
    (nameR, nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (businessName . entityVal <$> business)

    return ( Business uid <$> nameR
           , $(widgetFile "business/form")
           )
  where
      
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


getBusinessesR :: UserId -> Handler Html
getBusinessesR uid = do
    
    businesses <- runDB $ select $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessOwner ==. val uid
        orderBy [desc (x ^. BusinessId)]
        return x
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusinesses
        idFabAdd <- newIdent
        $(widgetFile "business/businesses")
