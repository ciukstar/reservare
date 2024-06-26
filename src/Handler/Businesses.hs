{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Businesses
  ( getBusinessesR
  , getBusinessNewR
  , postBusinessesR
  , getBusinessR
  , getBusinessEditR
  , postBusinessDeleR
  , postBusinessR
  ) where

import Data.Maybe (fromMaybe)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , desc, selectOne, where_, val
    )
import Database.Persist
    ( Entity (Entity), entityVal, entityKey, insert_, replace, PersistStoreWrite (delete)
    )

import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR (BusinessesR, BusinessNewR, BusinessR, BusinessEditR, BusinessDeleR)
    , AppMessage
      ( MsgBusinesses, MsgThereAreNoDataYet, MsgYouMightWantToAddAFew
      , MsgAdd, MsgOwner, MsgTheName, MsgSave, MsgCancel, MsgBusiness
      , MsgBack, MsgDele, MsgEdit, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordDeleted, MsgInvalidFormData, MsgRecordEdited, MsgRecordAdded
      )
    )

import Material3 (md3selectField, md3mreq, md3textField)

import Model
    ( statusError, statusSuccess
    , BusinessId, Business(Business, businessOwner, businessName)
    , User (User, userName, userEmail)
    , EntityField (UserName, UserId, BusinessId, BusinessOwner)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Widgets (widgetBanner, widgetSnackbar, widgetAccount, widgetMenu)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getMessageRender
    , MonadHandler (liftHandler), redirect, whamlet, addMessageI
    )
import Yesod.Core.Handler (newIdent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions (generateFormPost, runFormPost)
import Yesod.Form.Fields (optionsPairs)
import Yesod.Form
    ( FieldSettings (FieldSettings, fsName, fsLabel, fsTooltip, fsId, fsAttrs)
    , FieldView (fvInput), FormResult (FormSuccess)
    )
import Yesod.Persist.Core (runDB)


postBusinessDeleR :: BusinessId -> Handler Html
postBusinessDeleR bid = do
    ((fr,_),_) <- runFormPost formBusinessDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete bid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR BusinessesR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ BusinessR bid


postBusinessR :: BusinessId -> Handler Html
postBusinessR bid = do
    
    business <- runDB $ selectOne $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessId ==. val bid
        return x

    ((fr,fw),et) <- runFormPost $ formBusiness business

    case fr of
      FormSuccess r -> do
          runDB $ replace bid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ BusinessR bid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idFormBusiness <- newIdent
              $(widgetFile "data/businesses/edit")


getBusinessEditR :: BusinessId -> Handler Html
getBusinessEditR bid = do
    
    business <- runDB $ selectOne $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessId ==. val bid
        return x

    (fw,et) <- generateFormPost $ formBusiness business
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idFormBusiness <- newIdent
        $(widgetFile "data/businesses/edit")


getBusinessR :: BusinessId -> Handler Html
getBusinessR bid = do
    
    business <- runDB $ selectOne $ do
        x :& o <- from $ table @Business
            `innerJoin` table @User `on` (\(x :& o) -> x ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. BusinessId ==. val bid
        return (x,o)

    (fw2,et2) <- generateFormPost formBusinessDelete
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        $(widgetFile "data/businesses/business")


formBusinessDelete :: Form ()
formBusinessDelete extra = return (pure (), [whamlet|#{extra}|])


postBusinessesR :: Handler Html
postBusinessesR = do
    ((fr,fw),et) <- runFormPost $ formBusiness Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR BusinessesR
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idFormBusiness <- newIdent
              $(widgetFile "data/businesses/new")


getBusinessNewR :: Handler Html
getBusinessNewR = do
    (fw,et) <- generateFormPost $ formBusiness Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idFormBusiness <- newIdent
        $(widgetFile "data/businesses/new")


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
        
    (nameR, nameV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (businessName . entityVal <$> business)

    return ( Business <$> ownerR <*> nameR
           , $(widgetFile "data/businesses/form")
           )
  where
      options e = (fromMaybe (userEmail . entityVal $ e) (userName . entityVal $ e), entityKey e)


getBusinessesR :: Handler Html
getBusinessesR = do
    
    businesses <- runDB $ select $ do
        x <- from $ table @Business
        orderBy [desc (x ^. BusinessId)]
        return x
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusinesses
        idFabAdd <- newIdent
        $(widgetFile "data/businesses/businesses")
