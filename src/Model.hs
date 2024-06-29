{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import qualified Data.Proxy as P (Proxy)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utcToLocalTime, utc)
import Database.Persist.Sql (PersistFieldSql (sqlType))


data StoreType = StoreTypeDatabase | StoreTypeSession | StoreTypeGoogleSecretManager
    deriving (Show, Read, Eq, Ord)
derivePersistField "StoreType"


data AuthenticationType = UserAuthTypePassword
                        | UserAuthTypeEmail
                        | UserAuthTypeGoogle
    deriving (Show, Read, Eq, Ord)
derivePersistField "AuthenticationType"

instance PersistField LocalTime where
    toPersistValue :: LocalTime -> PersistValue
    toPersistValue x = toPersistValue (localTimeToUTC utc x)

    fromPersistValue :: PersistValue -> Either Text LocalTime
    fromPersistValue (PersistUTCTime x) = Right (utcToLocalTime utc x)
    fromPersistValue _ = Left "Invalid LocalTime"

instance PersistFieldSql LocalTime where
    sqlType :: P.Proxy LocalTime -> SqlType
    sqlType _ = SqlDayTime


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


ultDestKey :: Text
ultDestKey = "_ULT"

gmailAccessToken :: Text
gmailAccessToken = "gmail_access_token"

gmailAccessTokenExpiresIn :: Text
gmailAccessTokenExpiresIn = "gmail_access_token_expires_in"


apiInfoGoogle :: Text
apiInfoGoogle = "GOOGLE_API"


gmailRefreshToken :: Text
gmailRefreshToken = "gmail_refresh_token"


gmailSender :: Text
gmailSender = "gmail_sender"

secretVolumeGmail :: String
secretVolumeGmail = "/grt/gmail_refresh_token"


statusSuccess :: Text
statusSuccess = "success"

statusError :: Text
statusError = "error"
