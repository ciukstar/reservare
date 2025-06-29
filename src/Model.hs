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
import Data.Time.Calendar.Month (Month)
import Data.Time
    ( NominalDiffTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime )
import Data.Time.LocalTime
    ( TimeZone (timeZoneMinutes), minutesToTimeZone, TimeOfDay )

import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Persist.Sql (PersistFieldSql (sqlType), fromSqlKey, toSqlKey)

import Text.Printf (printf, PrintfType)
import Text.Read (readMaybe)


data PayGateway = PayGatewayStripe | PayGatewayYookassa
    deriving (Show, Read, Eq, Ord)
derivePersistField "PayGateway"


data PayStatus = PayStatusUnpaid | PayStatusPaid | PayStatusCanceled | PayStatusError | PayStatusUnknown
    deriving (Show, Read, Eq, Ord)
derivePersistField "PayStatus"


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

instance PersistField NominalDiffTime where
    toPersistValue :: NominalDiffTime -> PersistValue
    toPersistValue x = PersistInt64 (truncate (nominalDiffTimeToSeconds x))

    fromPersistValue :: PersistValue -> Either Text NominalDiffTime
    fromPersistValue (PersistInt64 x) = Right (secondsToNominalDiffTime (fromIntegral x))
    fromPersistValue _ = Left "Invalid NominalDiffTime"


instance PersistFieldSql NominalDiffTime where
    sqlType :: DP.Proxy NominalDiffTime -> SqlType
    sqlType _ = SqlInt64


instance PersistField TimeZone where
  toPersistValue :: TimeZone -> PersistValue
  toPersistValue x = toPersistValue $ timeZoneMinutes x

  fromPersistValue :: PersistValue -> Either Text TimeZone
  fromPersistValue (PersistInt64 x) = Right $ minutesToTimeZone $ fromIntegral x
  fromPersistValue _ = Left "Invalid TimeZone"

instance PersistFieldSql TimeZone where
    sqlType :: DP.Proxy TimeZone -> SqlType
    sqlType _ = SqlInt64


instance PathPiece Month where
    toPathPiece :: Month -> Text
    toPathPiece = pack . show

    fromPathPiece :: Text -> Maybe Month
    fromPathPiece = readMaybe . unpack

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


newtype Sectors = Sectors { unSectors :: [SectorId] }
    deriving (Show, Read, Eq)

instance PathMultiPiece Sectors where
    toPathMultiPiece :: Sectors -> [Text]
    toPathMultiPiece (Sectors xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Sectors
    fromPathMultiPiece xs = Sectors <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


endpointYookassa :: Text
endpointYookassa = "https://api.yookassa.ru/v3/payments"

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


keyThemeMode :: Text
keyThemeMode = "reservare_theme_mode"


keyBacklinkAuth :: Text
keyBacklinkAuth = "backlinkAuth"

keyBacklink :: Text
keyBacklink = "backlink"
