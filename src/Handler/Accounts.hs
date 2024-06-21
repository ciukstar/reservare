{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Accounts
  ( getAccountR
  , getAccountPhotoR
  ) where

import Database.Esqueleto.Experimental
    ( selectOne, from, table, val, where_
    , (^.), (==.)
    )
import Database.Persist (Entity(Entity))

import Foundation
    ( Handler
    , AppMessage (MsgUserAccount), Route (StaticR)
    )

import Model (UserId, UserPhoto (UserPhoto), EntityField (UserPhotoUser))

import Settings (widgetFile)
import Settings.StaticFiles (img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Handler (redirect)
import Yesod.Core.Widget (setTitleI)
import Yesod.Core.Content (TypedContent (TypedContent), ToContent (toContent))
import Yesod (YesodPersist(runDB))
import Data.Text.Encoding (encodeUtf8)

getAccountR :: UserId -> Handler Html
getAccountR uid = do
    defaultLayout $ do
        setTitleI MsgUserAccount
        $(widgetFile "accounts/account")


getAccountPhotoR :: UserId -> Handler TypedContent
getAccountPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg
