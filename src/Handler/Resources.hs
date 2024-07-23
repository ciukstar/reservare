{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Foundation
    ( Handler
    , Route (StaticR, HomeR)
    , AppMessage
      ( MsgAppDocumentation, MsgAppDescription, MsgErDiagram, MsgDocumentation
      , MsgAppName, MsgIssueTracking, MsgOverview, MsgStateMachineDiagram
      , MsgSourceCode, MsgBookAService, MsgUseCaseDiagram, MsgMakeAnAppointment
      , MsgDoc001, MsgDoc002, MsgDoc003
      )
    )

import Settings (widgetFile)

import Settings.StaticFiles
    ( img_Reservare_UCD_svg, img_Reservare_Book_Service_SMD_svg
    , img_Reservare_Make_Appointment_SMD_svg, img_Reservare_ERD_svg
    )

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetAccount, widgetBanner, widgetSnackbar)

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
