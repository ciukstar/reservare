{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Foundation
    ( Handler
    , Route (BookServicesR)
    , AppMessage
      ( MsgWelcome, MsgAppName, MsgWelcomeTo, MsgMakeAnAppointment
      , MsgServicesToBook
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetAccount, widgetBanner, widgetSnackbar)

import Yesod.Core (Yesod(defaultLayout), getMessages)
import Yesod.Core.Widget (setTitleI)


getHomeR :: Handler Html
getHomeR = do

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWelcome
        $(widgetFile "homepage")
