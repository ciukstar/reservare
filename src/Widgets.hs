{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Widgets
  ( widgetMenu, widgetAccount, widgetBanner, widgetSnackbar, widgetFilterChips
  , paramSector, paramBusiness, paramWorkspace
  ) where

import Control.Monad (unless, when)

import qualified Data.Aeson as A (toJSON)
import Data.Bifunctor (second, bimap)
import qualified Data.Map as M (fromListWith, findWithDefault)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, unpack)

import Database.Esqueleto.Experimental
    ( select, from, table, where_, isNothing_, orderBy, asc, innerJoin
    , (^.), (==.), (:&) ((:&))
    , Value (unValue), val, valList, in_, on
    )
import Database.Persist (Entity (Entity), entityKey)
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Foundation
    ( Widget, App
    , Route (HomeR, DataR, AuthR, AccountR, AccountPhotoR, DocsR)
    , DataR (UsersR, TokensR, DataBusinessesR, StaffR, SectorsR, ServicesR)
    , AppMessage
      ( MsgMainMenu, MsgUsers, MsgWelcome, MsgData, MsgSignIn, MsgSignOut
      , MsgUserAccount, MsgPhoto, MsgTokens, MsgBusinesses, MsgSectors
      , MsgStaff, MsgServices, MsgResources, MsgDocumentation, MsgSourceCode
      , MsgWorkspaces
      )
    )

import Model
    ( statusError, statusSuccess
    , Sectors (Sectors)
    , Workspace (Workspace)
    , Sector (Sector)
    , Business (Business)
    , EntityField
      ( SectorParent, SectorName, BusinessName, WorkspaceBusiness, WorkspaceName
      , BusinessId, WorkspaceId
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core (MonadHandler(liftHandler))
import Yesod.Core.Handler
    ( getCurrentRoute, newIdent, getRequest, YesodRequest (reqGetParams)
    )
import Yesod.Form.Fields (textField)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Persist (YesodPersist(runDB))


widgetFilterChips :: Route App -> Widget
widgetFilterChips route = do

    stati <- reqGetParams <$> getRequest
    let inputSectors = filter (\(x,_) -> x == paramSector) stati
    let selectedSectors = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputSectors

    sectors <- liftHandler $ runDB $ select $ do
        x <- from $ table @Sector
        where_ $ isNothing_ $ x ^. SectorParent
        orderBy [asc (x ^. SectorName)]
        return x

    let inputBusinesses = filter (\(x,_) -> x == paramBusiness) stati
    let selectedBusinesses = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputBusinesses

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
    let selectedWorkspaces = let wids = (entityKey <$> workspaces) in
          filter (`elem` wids) $ mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) inputWorkspaces


    let toMap = M.fromListWith (<>) . (second (:[]) <$>)

    businessWorkspaces <- liftHandler $ toMap . (bimap unValue unValue <$>) <$> runDB ( select $ do
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

    $(widgetFile "widgets/chips")


widgetBanner :: [(Text, Html)] -> Widget
widgetBanner msgs = do
    $(widgetFile "widgets/banner")


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


widgetSnackbar :: [(Text, Html)] -> Widget
widgetSnackbar msgs = do
    $(widgetFile "widgets/snackbar")


widgetAccount :: Widget
widgetAccount = do

    user <- maybeAuth

    idButtonUserAccount <- newIdent
    idMenuUserAccount <- newIdent
    $(widgetFile "widgets/account")


widgetMenu :: Widget
widgetMenu = do
    curr <- getCurrentRoute

    idDetailsMenu <- newIdent
    $(widgetFile "widgets/menu")
