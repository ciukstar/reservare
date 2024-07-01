{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Booking
  ( getBookServicesR
  , getBookStaffR
  ) where

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, on
    , (^.), (==.), (:&)((:&))
    )
import Database.Persist (Entity (Entity))
    
import Foundation
    ( Handler
    , Route (HomeR, BookServicesR, BookStaffR)
    , AppMessage
      ( MsgServices, MsgNext, MsgServices, MsgThereAreNoDataYet
      , MsgBack, MsgStaff
      )
    )
    
import Model
    ( ServiceId, Service(Service)
    , Workspace (Workspace)
    , Business (Business)
    , EntityField
      ( ServiceId, ServiceWorkspace, WorkspaceId, WorkspaceBusiness
      , BusinessId, ServiceName, AssignmentStaff, StaffId, StaffName
      ), Assignment (Assignment), Staff (Staff)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetSnackbar, widgetBanner)

import Yesod.Core.Handler (getMessages, newIdent)
import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))


getBookStaffR :: Handler Html
getBookStaffR = do

    staff <- runDB $ select $ do
        x :& e <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        orderBy [asc (e ^. StaffName)]
        return (x,e)
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        idFabNext <- newIdent
        $(widgetFile "book/staff/staff")


getBookServicesR :: Handler Html
getBookServicesR = do

    services <- runDB $ select $ do
        x :& w :& b <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
        orderBy [asc (x ^. ServiceName)]
        return (x,(w,b))
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFabNext <- newIdent
        $(widgetFile "book/services")
