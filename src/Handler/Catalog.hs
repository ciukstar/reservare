{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Catalog
  ( getCatalogR
  , getCatalogServiceR
  , getCatalogServiceBusinessR
  , getCatalogServicePhotoDefaultR
  , getStaffPhotoR
  , getCatalogServicePhotoR
  , getCatalogServiceAssignmentsR
  ) where

import Control.Monad (unless)

import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (unpack, pack)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, in_
    , (^.), (?.), (==.), (:&) ((:&))
    , justList, valList, orderBy, desc, leftJoin
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Foundation
    ( Handler
    , Route
      ( CatalogR, HomeR, StaticR, CatalogServicePhotoR, CatalogServiceR
      , CatalogServiceBusinessR, CatalogServiceAssignmentsR, AccountPhotoR
      , CatalogServicePhotoDefaultR
      )
    , AppMessage
      ( MsgCatalogue, MsgServiceCatalog, MsgNoServicesWereFoundForSearchTerms
      , MsgBack, MsgService, MsgDetails, MsgDescription, MsgTheName, MsgPrice
      , MsgPhoto, MsgServiceAssignments, MsgDuration, MsgBookNow, MsgBusiness
      , MsgAddress, MsgTimeZone, MsgCurrency, MsgOwner, MsgEmail, MsgRegistrant
      , MsgNoStaffAssignedYet
      )
    )

import Handler.Booking
    ( widgetFilterChips, paramSector, paramBusiness, paramWorkspace, paramScrollY )

import Model
    ( ServiceId, Service (Service)
    , Workspace (Workspace)
    , ServicePhotoId, ServicePhoto (ServicePhoto)
    , Business (Business)
    , User (User)
    , Assignment (Assignment)
    , StaffId, Staff (Staff)
    , BusinessLogo (BusinessLogo)
    , EntityField
      ( ServiceAvailable, ServicePhotoService, ServiceWorkspace, WorkspaceId
      , ServiceType, WorkspaceBusiness, ServiceId, ServicePhotoId, BusinessId
      , BusinessOwner, UserId, AssignmentStaff, StaffId, AssignmentService
      , StaffAccount, BusinessLogoBusiness, StaffPhotoStaff
      ), StaffPhoto (StaffPhoto)
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg, img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg )

import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Widgets (widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, lookupGetParams, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect, newIdent, lookupGetParam, YesodRequest (reqGetParams), getRequest
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

    stati <- reqGetParams <$> getRequest

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
    scrollY <- lookupGetParam paramScrollY
    paramSid <- lookupGetParam "sid"
    
    selectedSectors <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramSector
    selectedBusinesses <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramBusiness
    selectedWorkspaces <- mapMaybe ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams paramWorkspace

    services <- runDB $ select $ do
        x :& w <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. ServiceAvailable ==. val True
        unless (null selectedSectors) $ where_ $ x ^. ServiceType `in_` justList (valList selectedSectors)
        unless (null selectedBusinesses) $ where_ $ w ^. WorkspaceBusiness `in_` valList selectedBusinesses
        unless (null selectedWorkspaces) $ where_ $ w ^. WorkspaceId `in_` valList selectedWorkspaces
        orderBy [desc (x ^. ServiceId)]
        return (x,w)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServiceCatalog
        $(widgetFile "catalog/services")


getStaffPhotoR :: StaffId -> Handler TypedContent
getStaffPhotoR eid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @StaffPhoto
        where_ $ x ^. StaffPhotoStaff ==. val eid
        return x
    case photo of
      Just (Entity _ (StaffPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg


getCatalogServicePhotoDefaultR :: ServiceId -> Handler TypedContent
getCatalogServicePhotoDefaultR sid = do
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
