{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Foundation
    ( Handler, widgetMainMenu
    , Route (CatalogR, AppointmentStaffR, BookServicesR, StaticR)
    , AppMessage
      ( MsgWelcome, MsgAppName, MsgWelcomeTo, MsgMakeAnAppointment
      , MsgBookAService, MsgServiceCatalog
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    (img_book_online_120dp_00696D_FILL0_wght400_GRAD0_opsz48_svg)

import Text.Hamlet (Html)

import Widgets (widgetAccount, widgetBanner, widgetSnackbar)

import Yesod.Core (Yesod(defaultLayout), getMessages, newIdent)
import Yesod.Core.Widget (setTitleI)


getHomeR :: Handler Html
getHomeR = do

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWelcome
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "homepage")
