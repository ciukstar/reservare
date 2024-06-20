{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Foundation
    ( Handler
    , AppMessage (MsgWelcome, MsgAppName)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetAccount)

import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Widget (setTitleI)


getHomeR :: Handler Html
getHomeR = do
    
    defaultLayout $ do
        setTitleI MsgWelcome
        $(widgetFile "homepage")
