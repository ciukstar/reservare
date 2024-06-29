{-# LANGUAGE TemplateHaskell #-}

module Widgets
  ( widgetMenu, widgetAccount, widgetBanner, widgetSnackbar
  ) where

import Foundation
    ( Widget
    , Route (BusinessesR, HomeR, DataR, AuthR, AccountR, AccountPhotoR)
    , DataR (UsersR, TokensR, DataBusinessesR, StaffR, ServicesR)
    , AppMessage
      ( MsgMainMenu, MsgUsers, MsgWelcome, MsgData, MsgSignIn, MsgSignOut
      , MsgUserAccount, MsgPhoto, MsgTokens, MsgBusinesses, MsgBusiness
      , MsgStaff, MsgServices
      )
    )

import Data.Text (Text)

import Database.Persist (Entity (Entity))

import Model (statusError, statusSuccess)
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core.Handler (getCurrentRoute, newIdent)


widgetBanner :: [(Text, Html)] -> Widget
widgetBanner msgs = do
    $(widgetFile "widgets/banner")


widgetSnackbar :: [(Text, Html)] -> Widget
widgetSnackbar msgs = do
    $(widgetFile "widgets/snackbar")


widgetAccount :: Widget
widgetAccount = do

    user <- maybeAuth
    
    idButtonUserAccount <- newIdent
    idMenuUserAccount <- newIdent
    $(widgetFile "widgets/account")


widgetMenu :: Widget
widgetMenu = do

    user <- maybeAuth
    curr <- getCurrentRoute
    
    idDetailsMenu <- newIdent
    $(widgetFile "widgets/menu")



