{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Catalog
  ( getCatalogR
  , getCatalogServicePhotoR
  ) where

import Control.Monad (unless, forM)

import Data.Maybe (mapMaybe)
import Data.Text (Text, unpack)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, in_
    , (^.), (==.), (:&) ((:&))
    , justList, valList, orderBy, desc
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (toSqlKey)

import Foundation
    ( Handler
    , Route (CatalogR, HomeR, StaticR, CatalogServicePhotoR)
    , AppMessage
      ( MsgServiceCatalog, MsgNoServicesWereFoundForSearchTerms, MsgBack
      )
    )
    
import Model
    ( ServiceId, Service (Service)
    , Workspace (Workspace)
    , ServicePhotoId, ServicePhoto (ServicePhoto)
    , EntityField
      ( ServiceAvailable, ServicePhotoService, ServiceWorkspace, WorkspaceId
      , ServiceType, WorkspaceBusiness, ServiceId, ServicePhotoId
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Widgets (widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, lookupGetParams, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect
    )
import Yesod.Persist (YesodPersist(runDB))
import Data.Text.Encoding (encodeUtf8)
import Settings.StaticFiles (img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg)


getCatalogR :: Handler Html
getCatalogR = do
    
    selectedSectors <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramSector
    selectedBusinesses <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramBusiness
    selectedWorkspaces <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramWorkspace

    services <- do
        xs <- runDB $ select $ do
            x :& w <- from $ table @Service
                `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            where_ $ x ^. ServiceAvailable ==. val True
            unless (null selectedSectors) $ where_ $ x ^. ServiceType `in_` justList (valList selectedSectors)
            unless (null selectedBusinesses) $ where_ $ w ^. WorkspaceBusiness `in_` valList selectedBusinesses
            unless (null selectedWorkspaces) $ where_ $ w ^. WorkspaceId `in_` valList selectedWorkspaces
            orderBy [desc (x ^. ServiceId)]
            return (x,w)
        forM xs $ \(s@(Entity sid _),w) -> do
            f <- runDB $ selectOne $ do
                x <- from $ table @ServicePhoto
                where_ $ x ^. ServicePhotoService ==. val sid
                return x
            return (s,w,f)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceCatalog 
        $(widgetFile "catalog/services")


getCatalogServicePhotoR :: ServiceId -> ServicePhotoId -> Handler TypedContent
getCatalogServicePhotoR sid fid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @ServicePhoto
        where_ $ x ^. ServicePhotoId ==. val fid
        where_ $ x ^. ServicePhotoService ==. val sid
        return x
    case photo of
      Just (Entity _ (ServicePhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg



paramSector :: Text
paramSector = "t"

paramBusiness :: Text
paramBusiness = "b"

paramWorkspace :: Text
paramWorkspace = "w"
