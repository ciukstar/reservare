{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Data.Sectors
  ( getSectorsR, postSectorsR
  , getSectorR, postSectorR
  , getSectorNewR
  , getSectorEditR
  , postSectorDeleR
  ) where

import Control.Applicative ((<|>)) 
import Control.Monad (when, unless)

import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.List.Safe as LS (last)
import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, val, selectOne, where_
    , (^.), (?.), (==.), (:&) ((:&)), (+.)
    , Value (Value, unValue), leftJoin, withRecursive, just
    , isNothing_, unionAll_, leftJoin, innerJoin, on
    )
import Database.Persist
    ( Entity (Entity), entityVal, insert_)
import qualified Database.Persist as P (delete, replace)

import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR
      ( SectorsR, SectorR, SectorNewR, SectorEditR, SectorDeleR
      )
    , AppMessage
      ( MsgSectors, MsgAdd, MsgYouMightWantToAddAFew, MsgThereAreNoDataYet
      , MsgSector, MsgBack, MsgSave, MsgCancel, MsgAlreadyExists
      , MsgTheName, MsgDescription, MsgRecordAdded, MsgRecordDeleted
      , MsgInvalidFormData, MsgEdit, MsgDetails, MsgDeleteAreYouSure
      , MsgDele, MsgConfirmPlease, MsgSubsectors, MsgTabs, MsgRecordEdited, MsgGroup
      )
    )
    
import Material3 (md3mreq, md3textField, md3mopt, md3textareaField, md3selectField)
    
import Model
    ( statusSuccess, statusError, Sectors (Sectors)
    , SectorId, Sector(Sector, sectorName, sectorDescr)
    , Sector(Sector, sectorParent), EntityField (SectorName)
    , EntityField (SectorId, SectorName, SectorParent)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetAccount, widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, getMessages, newIdent, whamlet
    , SomeMessage (SomeMessage), getMessageRender, redirect, addMessageI
    , MonadHandler (liftHandler)
    )
import Yesod.Form
    ( Field, FormResult (FormSuccess), checkM, runFormPost
    , FieldView (fvInput)
    , FieldSettings
      ( FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs
      ), optionsPairs
    )
import Yesod.Form.Functions (generateFormPost)
import Yesod.Persist (YesodPersist(runDB))


postSectorDeleR :: SectorId -> Sectors -> Handler Html
postSectorDeleR sid ps = do
    ((fr,_),_) <- runFormPost formSectorDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ SectorsR ps
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ SectorR sid ps


postSectorR :: SectorId -> Sectors -> Handler Html
postSectorR sid ps = do

    sector <- runDB $ selectOne $ do
        x <- from $ table @Sector
        where_ $ x ^. SectorId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formSector Nothing sector
    case fr of
      FormSuccess r -> do
          runDB $ P.replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ SectorR sid ps
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgSector
          idFormSector <- newIdent
          $(widgetFile "data/sectors/edit")


getSectorEditR :: SectorId -> Sectors -> Handler Html
getSectorEditR sid ps = do

    sector <- runDB $ selectOne $ do
        x <- from $ table @Sector
        where_ $ x ^. SectorId ==. val sid
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
          idFormSector <- newIdent
          $(widgetFile "data/sectors/new")


getSectorNewR :: Sectors -> Handler Html
getSectorNewR ps@(Sectors sids) = do

    (fw,et) <- generateFormPost $ formSector (LS.last sids) Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSector
        idFormSector <- newIdent
        $(widgetFile "data/sectors/new")


formSector :: Maybe SectorId -> Maybe (Entity Sector) -> Form Sector
formSector pid sector extra = do

    rndr <- getMessageRender

    (nameR,nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgTheName)]
        } (sectorName . entityVal <$> sector)

    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgDescription)]
        } (sectorDescr . entityVal <$> sector)

    sectors <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Sector
        orderBy [asc (x ^. SectorName)]
        return (x ^. SectorName, x ^. SectorId) )

    (parentR,parentV) <- md3mopt (md3selectField (optionsPairs sectors)) FieldSettings
        { fsLabel = SomeMessage MsgGroup
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgGroup)]
        } ((sectorParent . entityVal <$> sector) <|> pure pid)

    let r = Sector <$> nameR <*> descrR <*> parentR
    let w = [whamlet|#{extra} ^{fvInput nameV} ^{fvInput descrV} ^{fvInput parentV}|]
    return (r,w)
    
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Sector
              where_ $ x ^. SectorName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity sid _) -> case sector of
              Nothing -> Left MsgAlreadyExists
              Just (Entity sid' _) | sid == sid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getSectorR :: SectorId -> Sectors -> Handler Html
getSectorR sid ps@(Sectors sids) = do

    sector <- runDB $ selectOne $ do
        x :& p <- from $ table @Sector
            `leftJoin` table @Sector `on` (\(x :& p) -> x ^. SectorParent ==. p ?. SectorId)
        where_ $ x ^. SectorId ==. val sid
        return (x,p)

    (fw,et) <- generateFormPost formSectorDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSector
        idPanelDetails <- newIdent
        $(widgetFile "data/sectors/sector")


formSectorDelete :: Form ()
formSectorDelete extra = return (FormSuccess (), [whamlet|#{extra}|])


getSectorsR :: Sectors -> Handler Html
getSectorsR ps@(Sectors sids) = do

    sectors <- runDB $ select $ do
        cte <- withRecursive
            (do
                  x <- from $ table @Sector

                  unless (null sids) $ where_ $ x ^. SectorParent ==. val (LS.last sids)
                  when (null sids) $ where_ $ isNothing_ $ x ^. SectorParent

                  let level = val (length sids)
                  return (level,x)
            )
            unionAll_
            (\parent -> do
                  (l,_) :& x <- from $ parent
                      `innerJoin` table @Sector `on` (\((_, p) :& x) -> just (p ^. SectorId) ==. x ^. SectorParent)
                  let level = l +. val 1
                  -- orderBy [desc level]
                  return (level,x)
            )
        from cte

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSectors
        idTabs <- newIdent
        idPanelSubsectors <- newIdent
        idFabAdd <- newIdent
        when (null sids) $ do
            $(widgetFile "data/sectors/sectors")
        unless (null sids) $ do
            $(widgetFile "data/sectors/subsectors")
        
