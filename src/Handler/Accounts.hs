{-# LANGUAGE TemplateHaskell #-}

module Handler.Accounts
  ( getAccountR
  , getAccountPhotoR
  ) where

import Foundation
    ( Handler
    , AppMessage (MsgUserAccount), Route (StaticR)
    )

import Model (UserId)

import Settings (widgetFile)
import Settings.StaticFiles (img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Handler (redirect)
import Yesod.Core.Widget (setTitleI)
import Yesod.Core.Content (TypedContent)

getAccountR :: UserId -> Handler Html
getAccountR uid = do
    defaultLayout $ do
        setTitleI MsgUserAccount
        $(widgetFile "accounts/account")


getAccountPhotoR :: UserId -> Handler TypedContent
getAccountPhotoR uid = redirect $ StaticR img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg
