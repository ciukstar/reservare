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
    ( YesodAtVenue (getHomeR, getBookDetailsR, getMaybeAuthId)
    , AtVenue, resourcesAtVenue
    , Route (CheckoutR)
    , AtVenueMessage
      ( MsgPaymentStatus, MsgViewBookingDetails, MsgReturnToHomePage
      , MsgFinish, MsgClose, MsgYourBookingHasBeenCreatedSuccessfully
      , MsgAuthenticationRequired, MsgAnotherAccountAccessProhibited
      )
    )
import Database.Persist.Sql (SqlBackend)

import Model
    ( statusSuccess, statusError
    , BookId, PayOptionId, UserId
    )

import Settings (widgetFile)

import Yesod.Core
    ( YesodSubDispatch (yesodSubDispatch), Application
    , mkYesodSubDispatch, Html, Yesod (defaultLayout), SubHandlerFor
    , MonadHandler (liftHandler), setTitleI, getMessages, addMessageI
    , newIdent, permissionDeniedI
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Persist.Core (YesodPersist(YesodPersistBackend))



getCheckoutR :: (YesodAtVenue m, YesodPersist m, YesodPersistBackend m ~ SqlBackend)
             => UserId -> BookId -> PayOptionId -> SubHandlerFor AtVenue m Html
getCheckoutR uid bid _oid = do

    checkAuthorized uid

    homeR <- liftHandler getHomeR
    bookDetailsR <- liftHandler $ getBookDetailsR bid

    addMessageI statusSuccess MsgYourBookingHasBeenCreatedSuccessfully
    msgs <- getMessages
    liftHandler $ defaultLayout $ do
        setTitleI MsgPaymentStatus
        idHeader <- newIdent
        idMain <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "gateways/atvenue/completion")


checkAuthorized :: YesodAtVenue m => UserId -> SubHandlerFor AtVenue m ()
checkAuthorized uid = do
    muid <- liftHandler getMaybeAuthId

    liftHandler $ case muid of
      Nothing -> permissionDeniedI MsgAuthenticationRequired

      Just uid' | uid' /= uid -> permissionDeniedI MsgAnotherAccountAccessProhibited
                | otherwise -> return ()
                
        
instance (YesodAtVenue m, YesodPersist m, YesodPersistBackend m ~ SqlBackend) => YesodSubDispatch AtVenue m where
    yesodSubDispatch :: YesodSubRunnerEnv AtVenue m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAtVenue)
