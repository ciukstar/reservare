{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.DemoFr (fillDemoFr) where

import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.FileEmbed (embedFile)

import Database.Persist(PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)
import Import.NoFoundation (ReaderT)

import Model
    ( apiInfoGoogle, AuthenticationType (UserAuthTypeEmail)
    , User
      ( User, userEmail, userAuthType, userPassword, userVerkey, userVerified
      , userName, userSuperuser, userAdmin
      )
    , UserPhoto
      ( UserPhoto, userPhotoMime, userPhotoPhoto, userPhotoUser
      , userPhotoAttribution
      )
    , Token (Token, tokenApi, tokenStore)
    , Store (Store, storeToken, storeKey, storeVal)
    , StoreType (StoreTypeDatabase, StoreTypeGoogleSecretManager)
    )

import Settings (AppSettings)

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoFr :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoFr appSettings = do
    
    pass1 <- liftIO $ saltPass "bernardj"
    let user1 = User { userEmail = "bernardj@mail.fr"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Bernard Jade"
                     , userSuperuser = False
                     , userAdmin = True
                     }

    usr1 <- insert user1

    insert_ UserPhoto { userPhotoUser = usr1
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/2148728586.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                      }

    pass2 <- liftIO $ saltPass "thomasgr"
    let user2 = User { userEmail = "thomasgr@mail.fr"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Thomas Gabriel Raphaël"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr2 <- insert user2

    pass3 <- liftIO $ saltPass "richardal"
    let user3 = User { userEmail = "richardal@mail.fr"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Richard Arthur Louis"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr3 <- insert user3

    pass4 <- liftIO $ saltPass "duboisaa"
    let user4 = User { userEmail = "duboisaa@mail.fr"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Dubois Alice Ambre"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr4 <- insert user4

    insert_ UserPhoto { userPhotoUser = usr4
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/2148728638.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]

                      }
    return ()
