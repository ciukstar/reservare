{-# LANGUAGE TemplateHaskell #-}

module Widgets (widgetMenu, widgetAccount) where

import Foundation
    ( Widget
    , Route (HomeR, DataR, AuthR, AccountR, AccountPhotoR)
    , DataR (UsersR)
    , AppMessage
      ( MsgMainMenu, MsgUsers, MsgWelcome, MsgData, MsgSignIn, MsgSignOut
      , MsgUserAccount, MsgPhoto
      )
    )

import Database.Persist (Entity (Entity))
    
import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core.Handler (getCurrentRoute, newIdent)


widgetAccount :: Widget
widgetAccount = do

    user <- maybeAuth
    curr <- getCurrentRoute
    
    idButtonUserAccount <- newIdent
    idMenuUserAccount <- newIdent
    idDetailsMenu <- newIdent
    $(widgetFile "widgets/account")


widgetMenu :: Widget
widgetMenu = do

    user <- maybeAuth
    curr <- getCurrentRoute
    
    idDetailsMenu <- newIdent
    $(widgetFile "widgets/menu")



