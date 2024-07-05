{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Foundation
    ( Handler
    , Route (StaticR, HomeR, AuthR, DataR)
    , DataR ()
    , AppMessage
      ( MsgAppDocumentation, MsgAppDescription, MsgErDiagram, MsgDocumentation
      , MsgAppName, MsgIssueTracking, MsgOverview, MsgSourceCode
      , MsgDoc001, MsgDoc002, MsgDoc003
      )
    )

import Settings (widgetFile)

import Settings.StaticFiles (img_Reservare_ERD_svg)

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetAccount, widgetBanner, widgetSnackbar)

import Yesod.Auth (Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages, getUrlRender
    , getMessageRender
    )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    msgs <- getMessages
    r <- getUrlRender
    m <- getMessageRender
    let t = preEscapedToHtml . m

    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgAppDocumentation
        $(widgetFile "resources/docs")
