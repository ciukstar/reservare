{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module AtVenue where

import AtVenue.Data
    ( YesodAtVenue (getHomeR, getBookDetailsR), AtVenue, resourcesAtVenue
    , Route (CheckoutR)
    , AtVenueMessage
      ( MsgPaymentStatus, MsgViewBookingDetails, MsgReturnToHomePage
      , MsgFinish, MsgBack, MsgYourBookingHasBeenCreatedSuccessfully
      )
    )
import Database.Persist.Sql (SqlBackend)

import Model
    ( statusSuccess, statusError
    , BookId, PayOptionId
    )

import Settings (widgetFile)

import Yesod.Core
    ( YesodSubDispatch (yesodSubDispatch), Application
    , mkYesodSubDispatch, Html, Yesod (defaultLayout), SubHandlerFor
    , MonadHandler (liftHandler), setTitleI, getMessages, addMessageI
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Persist.Core (YesodPersist(YesodPersistBackend))



getCheckoutR :: (YesodAtVenue m, YesodPersist m, YesodPersistBackend m ~ SqlBackend)
             => BookId -> PayOptionId -> SubHandlerFor AtVenue m Html
getCheckoutR bid oid = do

    homeR <- liftHandler getHomeR
    bookDetailsR <- liftHandler $ getBookDetailsR bid

    addMessageI statusSuccess MsgYourBookingHasBeenCreatedSuccessfully
    msgs <- getMessages
    liftHandler $ defaultLayout $ do
        setTitleI MsgPaymentStatus 
        $(widgetFile "gateways/atvenue/completion")


        
instance (YesodAtVenue m, YesodPersist m, YesodPersistBackend m ~ SqlBackend) => YesodSubDispatch AtVenue m where
    yesodSubDispatch :: YesodSubRunnerEnv AtVenue m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAtVenue)
