{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Catalog
  ( getCatalogR
  , getCatalogServiceR
  , getCatalogServiceBusinessR
  , getCatalogServicePhotoR
  , getCatalogServiceAssignmentsR
  ) where

import Control.Monad (unless, forM)

import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, in_
    , (^.), (?.), (==.), (:&) ((:&))
    , justList, valList, orderBy, desc, leftJoin
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (toSqlKey)

import Foundation
    ( Handler
    , Route
      ( CatalogR, HomeR, StaticR, CatalogServicePhotoR, CatalogServiceR
      , CatalogServiceBusinessR, CatalogServiceAssignmentsR
      , AccountPhotoR
      )
    , AppMessage
      ( MsgCatalogue, MsgServiceCatalog, MsgNoServicesWereFoundForSearchTerms
      , MsgBack, MsgService, MsgDetails, MsgDescription, MsgTheName, MsgPrice
      , MsgPhoto, MsgServiceAssignments, MsgDuration, MsgBookNow, MsgBusiness
      , MsgAddress, MsgTimeZone, MsgCurrency, MsgOwner, MsgEmail
      , MsgRegistrant, MsgNoStaffAssignedYet
      )
    )

import Handler.Booking
    ( widgetFilterChips, paramSector, paramBusiness, paramWorkspace )

import Model
    ( ServiceId, Service (Service)
    , Workspace (Workspace)
    , ServicePhotoId, ServicePhoto (ServicePhoto)
    , Business (Business)
    , User (User)
    , Assignment (Assignment), Staff (Staff)
    , EntityField
      ( ServiceAvailable, ServicePhotoService, ServiceWorkspace, WorkspaceId
      , ServiceType, WorkspaceBusiness, ServiceId, ServicePhotoId, BusinessId
      , BusinessOwner, UserId, AssignmentStaff, StaffId, AssignmentService
      , StaffAccount
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg )

import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Widgets (widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, lookupGetParams, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect, newIdent
    )
import Yesod.Persist (YesodPersist(runDB))


getCatalogServiceAssignmentsR :: ServiceId -> Handler Html
getCatalogServiceAssignmentsR sid = do

    assignments <- runDB $ select $ do
        x :& e :& u <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
            `leftJoin` table @User `on` (\(_ :& e :& u) -> e ^. StaffAccount ==. u ?. UserId)
        where_ $ x ^. AssignmentService ==. val sid
        return ((x,e),u)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idTabAssignments <- newIdent
        idPanelAssignments <- newIdent 
        $(widgetFile "catalog/assignments")


getCatalogServiceBusinessR :: ServiceId -> Handler Html
getCatalogServiceBusinessR sid = do

    workspace <- runDB $ selectOne $ do
        x :& w :& b :& o <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @User `on` (\(_ :& _ :& b :& o) -> b ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. ServiceId ==. val sid
        return ((w,b),o)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idTabBusiness <- newIdent
        idPanelBusiness <- newIdent 
        $(widgetFile "catalog/business")
        


getCatalogServiceR :: ServiceId -> Handler Html
getCatalogServiceR sid = do

    service <- do
        service <- runDB $ selectOne $ do
            x :& w <- from $ table @Service
                `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            where_ $ x ^. ServiceId ==. val sid
            where_ $ x ^. ServiceAvailable ==. val True
            orderBy [desc (x ^. ServiceId)]
            return (x,w)
        photos <- runDB $ select $ do
            x <- from $ table @ServicePhoto
            where_ $ x ^. ServicePhotoService ==. val sid
            return x
        return (service,photos)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idTabDetails <- newIdent
        idPanelDetails <- newIdent 
        $(widgetFile "catalog/service")


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
