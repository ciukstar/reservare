{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Foundation
    ( Handler, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (StaticR, HomeR, AuthR)
    , AppMessage
      ( MsgAppDocumentation, MsgAppDescription, MsgErDiagram, MsgDocumentation
      , MsgAppName, MsgIssueTracking, MsgOverview, MsgStateMachineDiagram
      , MsgSourceCode, MsgBookAService, MsgUseCaseDiagram, MsgMakeAnAppointment
      , MsgSuperuser, MsgUsername, MsgPassword, MsgClientId, MsgClientSecret
      , MsgEmail, MsgPaymentGateway, MsgPublicKey, MsgSecretKey, MsgBasicEntities
      , MsgUser, MsgBusiness, MsgWorkspace, MsgService, MsgStaff, MsgSchedule
      , MsgBooking, MsgServiceAssignment, MsgSearchEngineOptimization
      , MsgPaymentOption, MsgYooKassa, MsgShopId, MsgSector
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc005, MsgDoc006
      , MsgDoc007, MsgDoc008, MsgDoc009, MsgDoc010, MsgDoc011, MsgDoc012
      , MsgDoc013, MsgDoc014, MsgDoc015
      )
    )

import Settings (widgetFile)

import Settings.StaticFiles
    ( img_Reservare_UCD_svg, img_Reservare_Book_Service_SMD_svg
    , img_Reservare_Make_Appointment_SMD_svg, img_Reservare_ERD_svg
    )

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages, getUrlRender
    , getMessageRender, newIdent
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
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "resources/docs")
