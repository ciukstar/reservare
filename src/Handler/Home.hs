{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Foundation
    ( Handler, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (CatalogR, AppointmentStaffR, BookServicesR, StaticR)
    , AppMessage
      ( MsgWelcome, MsgAppName, MsgWelcomeTo, MsgMakeAnAppointment
      , MsgBookAService, MsgServiceCatalog, MsgLogotype
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_book_online_64dp_6750A4_FILL0_wght400_GRAD0_opsz48_svg
    )

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), getMessages, newIdent)
import Yesod.Core.Widget (setTitleI)


getHomeR :: Handler Html
getHomeR = do
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWelcome
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        idHeader <- newIdent
        idMain <- newIdent
        idNavButtons <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "homepage")
