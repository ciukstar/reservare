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
  , getCatalogBusinessLogoR
  ) where

import Control.Monad (unless, join)

import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (unpack, pack)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, in_
    , (^.), (?.), (==.), (:&) ((:&))
    , justList, valList, orderBy, desc, leftJoin, just, Value (unValue)
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Foundation
    ( Handler
    , Route
      ( CatalogR, HomeR, StaticR, CatalogServicePhotoR, CatalogServiceR
      , CatalogServiceBusinessR, CatalogServiceAssignmentsR
      , CatalogServicePhotoDefaultR, CatalogBusinessLogoR
      , StaffPhotoR
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
    , BusinessId, Business (Business)
    , User (User)
    , Assignment (Assignment)
    , StaffId, Staff (Staff)
    , BusinessLogo (BusinessLogo)
    , StaffPhoto (StaffPhoto)
    , EntityField
      ( ServiceAvailable, ServicePhotoService, ServiceWorkspace, WorkspaceId
      , ServiceType, WorkspaceBusiness, ServiceId, ServicePhotoId, BusinessId
      , BusinessOwner, UserId, AssignmentStaff, StaffId, AssignmentService
      , BusinessLogoBusiness, StaffPhotoStaff, BusinessLogoAttribution
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_rule_settings_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    , img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg
    , img_broken_image_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Widgets (widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, lookupGetParams, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect
    , newIdent, lookupGetParam, YesodRequest (reqGetParams), getRequest
    )
import Yesod.Persist (YesodPersist(runDB))
import Data.Bifunctor (Bifunctor(second, first))


getCatalogServiceAssignmentsR :: ServiceId -> Handler Html
getCatalogServiceAssignmentsR sid = do

    assignments <- runDB $ select $ do
        x :& e <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        where_ $ x ^. AssignmentService ==. val sid
        return (x,e)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgService
        idTabAssignments <- newIdent
        idPanelAssignments <- newIdent 
        $(widgetFile "catalog/assignments")


getCatalogServiceBusinessR :: ServiceId -> Handler Html
getCatalogServiceBusinessR sid = do

    workspace <- (first (second (second (join . unValue))) <$>) <$> runDB ( selectOne $ do
        x :& w :& b :& l :& o <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
            `leftJoin` table @BusinessLogo `on` (\(_ :& _ :& b :& l) -> just (b ^. BusinessId) ==. l ?. BusinessLogoBusiness)
            `innerJoin` table @User `on` (\(_ :& _ :& b :& _ :& o) -> b ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. ServiceId ==. val sid
        return ((w,(b,l ?. BusinessLogoAttribution)),o) )
        
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


getCatalogBusinessLogoR :: BusinessId -> Handler TypedContent
getCatalogBusinessLogoR bid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @BusinessLogo
        where_ $ x ^. BusinessLogoBusiness ==. val bid
        return x
    case photo of
      Just (Entity _ (BusinessLogo _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_broken_image_24dp_00696D_FILL0_wght400_GRAD0_opsz24_svg
