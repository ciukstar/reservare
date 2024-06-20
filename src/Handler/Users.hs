{-# LANGUAGE TemplateHaskell #-}

module Handler.Users (getUsersR) where

import Foundation
    ( Handler
    , AppMessage (MsgUsers)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Widget (setTitleI)


getUsersR :: Handler Html
getUsersR = do
    defaultLayout $ do
        setTitleI MsgUsers
        $(widgetFile "data/users/users")
