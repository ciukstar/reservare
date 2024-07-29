{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Foundation
    ( Handler
    , Route (StaticR, HomeR, AuthR)
    , AppMessage
      ( MsgAppDocumentation, MsgAppDescription, MsgErDiagram, MsgDocumentation
      , MsgAppName, MsgIssueTracking, MsgOverview, MsgStateMachineDiagram
      , MsgSourceCode, MsgBookAService, MsgUseCaseDiagram, MsgMakeAnAppointment
      , MsgSuperuser, MsgUsername, MsgPassword, MsgClientId, MsgClientSecret
      , MsgEmail, MsgPaymentGateway, MsgPublicKey, MsgSecretKey, MsgBasicEntities
      , MsgUser, MsgBusiness, MsgWorkspace, MsgService, MsgStaff, MsgSchedule
      , MsgBooking, MsgServiceAssignment, MsgSearchEngineOptimization
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc005, MsgDoc006
      , MsgDoc007, MsgDoc008, MsgDoc009
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
