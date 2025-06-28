{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Business
  ( getBusinessesR, postBusinessesR
  , getBusinessNewR
  , getBusinessR, postBusinessR
  , getBusinessEditR
  , postBusinessDeleR
  , getWorkspacesR, postWorkspacesR
  , getWorkspaceNewR
  , getWorkspaceR, postWorkspaceR
  , getWorkspaceEditR
  , postWorkspaceDeleR
  , getBusinessLogoR
  ) where

import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( Value (unValue), select, from, table, orderBy, innerJoin, on
    , (^.), (==.), (:&)((:&)), (=.)
    , desc, selectOne, where_, val, update, set
    )
import Database.Persist
    ( Entity (Entity), entityVal
    , insert, insert_, replace, delete, upsert
    )
import qualified Database.Persist as P ((=.)) 

import Foundation
    ( Handler, Form, widgetSnackbar, widgetAccount, widgetMainMenu
    , Route
      ( BusinessesR, BusinessNewR, BusinessR, BusinessEditR, BusinessDeleR
      , WorkspacesR, WorkspaceNewR, WorkspaceR, WorkspaceEditR, WorkspaceDeleR
      , StaticR, BusinessLogoR
      )
    , AppMessage
      ( MsgBusinesses, MsgThereAreNoDataYet, MsgYouMightWantToAddAFew
      , MsgAdd, MsgOwner, MsgTheName, MsgSave, MsgCancel, MsgBusiness
      , MsgBack, MsgDele, MsgEdit, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordDeleted, MsgInvalidFormData, MsgRecordEdited, MsgRecordAdded
      , MsgDetails, MsgWorkspaces, MsgAddress, MsgWorkspace, MsgAlreadyExists
      , MsgTimeZone, MsgCurrency, MsgBusinessFullName, MsgDescription, MsgLogo
      , MsgAttribution, MsgLogotype
      )
    )

import Material3 (md3widget, md3widgetTextarea)

import Model
    ( statusError, statusSuccess
    , BusinessId, Business(Business, businessName, businessFullName, businessDescr)
    , BusinessLogo (BusinessLogo)
    , UserId, User (User)
    , WorkspaceId
    , Workspace
      ( Workspace, workspaceName, workspaceAddress, workspaceTzo
      , workspaceCurrency
      )
    , EntityField
      ( UserId, BusinessId, BusinessOwner, WorkspaceId, WorkspaceBusiness
      , WorkspaceName, BusinessLogoBusiness, BusinessLogoAttribution, BusinessName, BusinessLogoMime, BusinessLogoPhoto
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles (img_broken_image_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, redirect, whamlet, addMessageI
    , FileInfo (fileContentType), MonadHandler (liftHandler)
    , TypedContent (TypedContent), ToContent (toContent), fileSourceByteString
    )
import Yesod.Core.Handler (newIdent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( Field, FormResult (FormSuccess)
    , FieldSettings ( FieldSettings, fsName, fsLabel, fsTooltip, fsId, fsAttrs)
    , FieldView (fvId, fvInput, fvErrors)
    )
import Yesod.Form.Fields
    ( textareaField, textField, fileField, htmlField
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM, mreq, mopt)
import Yesod.Persist.Core (runDB)
import Control.Monad (void)


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
              idHeader <- newIdent
              idMain <- newIdent
              idFormWorkspace <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
        idHeader <- newIdent
        idMain <- newIdent
        idFormWorkspace <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "business/workspaces/edit")


getWorkspaceR :: UserId -> BusinessId -> WorkspaceId -> Handler Html
getWorkspaceR uid bid wid = do

    workspace <- runDB $ selectOne $ do
        x :& b :& o <- from $ table @Workspace
            `innerJoin` table @Business `on` (\(x :& b) -> x ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @User `on` (\(_ :& b :& o) -> b ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. WorkspaceId ==. val wid
        return (x,b,o)

    (fw0,et0) <- generateFormPost formWorkspaceDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idHeader <- newIdent
        idMain <- newIdent
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idFormDelete <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
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
              idHeader <- newIdent
              idMain <- newIdent
              idFormWorkspace <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "business/workspaces/new")


getWorkspaceNewR :: UserId -> BusinessId -> Handler Html
getWorkspaceNewR uid bid = do

    (fw,et) <- generateFormPost $ formWorkspace bid Nothing   
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idHeader <- newIdent
        idMain <- newIdent
        idFormWorkspace <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "business/workspaces/new")


formWorkspace :: BusinessId -> Maybe (Entity Workspace) -> Form Workspace
formWorkspace bid workspace extra = do
    
    (nameR, nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (workspaceName . entityVal <$> workspace)
        
    (addrR, addrV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (workspaceAddress . entityVal <$> workspace)
        
    (tzoR, tzoV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTimeZone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (pack . show . workspaceTzo . entityVal <$> workspace)
        
    (currencyR, currencyV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgCurrency
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (workspaceCurrency . entityVal <$> workspace)

    return ( Workspace bid <$> nameR <*> addrR <*> (read . unpack <$> tzoR) <*> currencyR
           , [whamlet|
                     #{extra}
                     ^{md3widget nameV}
                     ^{md3widgetTextarea addrV}
                     ^{md3widget tzoV}
                     ^{md3widget currencyV}
                     |]
           )
  where
      
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

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
        idHeader <- newIdent
        idMain <- newIdent
        classHeadline <- newIdent
        classSupportingText <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "common/css/rows")
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
      FormSuccess (r,Just fi,attrib) -> do
          runDB $ replace bid r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (BusinessLogo bid (fileContentType fi) bs attrib)
              [ BusinessLogoMime P.=. fileContentType fi
              , BusinessLogoPhoto P.=. bs
              , BusinessLogoAttribution P.=. attrib
              ]
          addMessageI statusSuccess MsgRecordEdited
          redirect $ BusinessR uid bid
          
      FormSuccess (r,Nothing,attrib) -> do
          runDB $ replace bid r
          runDB $ update $ \x -> do
                set x [ BusinessLogoAttribution =. val attrib ]
                where_ $ x ^. BusinessLogoBusiness ==. val bid
          addMessageI statusSuccess MsgRecordEdited
          redirect $ BusinessR uid bid
          
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idHeader <- newIdent
              idMain <- newIdent
              idFormBusiness <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
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
        idHeader <- newIdent
        idMain <- newIdent
        idFormBusiness <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "business/edit")


getBusinessR :: UserId -> BusinessId -> Handler Html
getBusinessR uid bid = do
    
    business <- runDB $ selectOne $ do
        x :& o <- from $ table @Business
            `innerJoin` table @User `on` (\(x :& o) -> x ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. BusinessId ==. val bid
        return (x,o)

    attribution <- ((unValue =<<) <$>) $ runDB $ selectOne $ do
        x <- from $ table @BusinessLogo
        where_ $ x ^. BusinessLogoBusiness ==. val bid
        return (x ^. BusinessLogoAttribution)

    (fw0,et0) <- generateFormPost formBusinessDelete
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idHeader <- newIdent
        idMain <- newIdent
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idFormDelete <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "business/business")


formBusinessDelete :: Form ()
formBusinessDelete extra = return (pure (), [whamlet|#{extra}|])


postBusinessesR :: UserId -> Handler Html
postBusinessesR uid = do
    ((fr,fw),et) <- runFormPost $ formBusiness uid Nothing
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
          redirect $ BusinessesR uid
          
      FormSuccess (r,_,_) -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ BusinessesR uid
          
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idHeader <- newIdent
              idMain <- newIdent
              idFormBusiness <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "common/css/main")
              $(widgetFile "business/new")


getBusinessNewR :: UserId -> Handler Html
getBusinessNewR uid = do
    (fw,et) <- generateFormPost $ formBusiness uid Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idHeader <- newIdent
        idMain <- newIdent
        idFormBusiness <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "business/new")


formBusiness :: UserId -> Maybe (Entity Business) -> Form (Business,Maybe FileInfo,Maybe Html)
formBusiness uid business extra = do

    (nameR, nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (businessName . entityVal <$> business)

    (fullNameR, fullNameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgBusinessFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (businessFullName . entityVal <$> business)

    (descrR, descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (businessDescr . entityVal <$> business)

    (logoR,logoV) <- mopt fileField FieldSettings
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

    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just attrib)

    idLabelLogo <- newIdent
    idFigureLogo <- newIdent
    idImgLogo <- newIdent

    return ( (,,) <$> (Business uid <$> nameR <*> fullNameR <*> descrR) <*> logoR <*> attribR
           , $(widgetFile "business/form")
           )
  where

      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

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
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        idHeader <- newIdent
        idMain <- newIdent
        classHeadline <- newIdent
        classSupportingText <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "common/css/rows")
        $(widgetFile "business/businesses")


getBusinessLogoR :: UserId -> BusinessId -> Handler TypedContent
getBusinessLogoR _ bid = do
    logo <- runDB $ selectOne $ do
        x <- from $ table @BusinessLogo
        where_ $ x ^. BusinessLogoBusiness ==. val bid
        return x
        
    case logo of
      Just (Entity _ (BusinessLogo _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_broken_image_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
