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
{-# LANGUAGE TypeApplications #-}

module Model where

import ClassyPrelude.Yesod


import qualified Data.Proxy as DP (Proxy (Proxy))
import Data.Time.LocalTime (TimeZone (timeZoneMinutes), minutesToTimeZone) 

import Database.Persist.Quasi ( lowerCaseSettings )

import Text.Printf (printf, PrintfType)
import Database.Persist.Sql (PersistFieldSql (sqlType))


data PayMethod = PayAtVenue | PayNow
    deriving (Show, Read, Eq, Ord)
derivePersistField "PayMethod"

data StoreType = StoreTypeDatabase | StoreTypeSession | StoreTypeGoogleSecretManager
    deriving (Show, Read, Eq, Ord)
derivePersistField "StoreType"


data AuthenticationType = UserAuthTypePassword
                        | UserAuthTypeEmail
                        | UserAuthTypeGoogle
    deriving (Show, Read, Eq, Ord)
derivePersistField "AuthenticationType"


instance PersistField TimeZone where
  toPersistValue :: TimeZone -> PersistValue
  toPersistValue x = toPersistValue $ timeZoneMinutes x

  fromPersistValue :: PersistValue -> Either Text TimeZone
  fromPersistValue (PersistInt64 x) = Right $ minutesToTimeZone $ fromIntegral x
  fromPersistValue _ = Left "Invalid TimeZone"

instance PersistFieldSql TimeZone where
    sqlType :: DP.Proxy TimeZone -> SqlType
    sqlType _ = SqlInt64

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


endpointStripePaymentIntentCancel :: PrintfType r => r
endpointStripePaymentIntentCancel = printf $ unpack (endpointStripePaymentIntents <> "/%s/cancel")

endpointStripePaymentIntents :: Text
endpointStripePaymentIntents = endpointStripe <> "/payment_intents"

endpointStripe :: Text
endpointStripe = "https://api.stripe.com/v1"

scriptRemoteStripe :: Text
scriptRemoteStripe = "https://js.stripe.com/v3"


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
