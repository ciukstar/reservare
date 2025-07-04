{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import AtVenue ()
import AtVenue.Data (AtVenue(AtVenue))

import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Sqlite
    ( runSqlPool, sqlDatabase, createSqlitePoolWithConfig )

import Demo.DemoEn (fillDemoEn)
import Demo.DemoFr (fillDemoFr)
import Demo.DemoRo (fillDemoRo)
import Demo.DemoRu (fillDemoRu)
    
import Import
import Language.Haskell.TH.Syntax ( qLocation )
import Network.HTTP.Client.TLS ( getGlobalManager )
import qualified Network.Wai as W (Response)
import Network.Wai (Middleware, mapResponseHeaders)
import Network.Wai.Handler.Warp
    ( Settings, defaultSettings, defaultShouldDisplayException
    , runSettings, setHost, setOnException, setPort, getPort
    )
import Network.Wai.Middleware.Gzip
    ( GzipSettings(gzipFiles), GzipFiles (GzipCompress), gzip )
import Network.Wai.Middleware.RequestLogger
    ( Destination (Logger), IPAddrSource (..), OutputFormat (..)
    , destination, mkRequestLogger, outputFormat
    )

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import Handler.Catalog
    ( getCatalogR
    , getCatalogServiceR
    , getCatalogServiceBusinessR
    , getCatalogServicePhotoR
    , getCatalogServicePhotoDefaultR
    , getStaffPhotoR
    , getCatalogServiceAssignmentsR
    , getCatalogServiceAssignmentR
    , getCatalogStaffScheduleR
    , getCatalogStaffScheduleSlotsR
    , getCatalogBusinessLogoR
    )

import Handler.Appointments
    ( getAppointmentStaffR, postAppointmentStaffR
    , getAppointmentTimingR, postAppointmentTimingR
    , getAppointmentTimeSlotsR, postAppointmentTimeSlotsR
    , getAppointmentPaymentR, postAppointmentPaymentR
    )

import Handler.Booking
    ( getBookServicesR, postBookServicesR
    , getBookStaffR, postBookStaffR
    , getBookTimingR, postBookTimingR
    , getBookTimeSlotsR, postBookTimeSlotsR
    , getBookPaymentR, postBookPaymentR
    , getBookDetailsR
    )

import Handler.Accounts
    ( getAccountR, postAccountR
    , getAccountEditR
    , getAccountPhotoR
    , getAccountInfoR, postAccountInfoR
    , getAccountInfoEditR
    , getAccountSettingsR
    )

import Handler.Resources (getDocsR)

import Handler.Home ( getHomeR )

import Handler.Business
    ( getBusinessesR, postBusinessesR
    , getBusinessNewR
    , getBusinessR, postBusinessR
    , getBusinessEditR
    , postBusinessDeleR
    , getWorkspacesR, postWorkspacesR
    , getWorkspaceNewR
    , getWorkspaceR, postWorkspaceR
    , getWorkspaceEditR
    , postWorkspaceDeleR
    , getBusinessLogoR
    )

import Handler.Data.Services
    ( getServicesR, getServiceR, getServiceNewR, postServicesR
    , getServiceEditR, postServiceDeleR, postServiceR
    , getServiceAssignmentsR, getServiceAssignmentNewR
    , postServiceAssignmentsR, getServiceAssignmentR
    , getServiceAssignmentEditR, postServiceAssignmentDeleR
    , postServiceAssignmentR
    , getServicePhotoR, postServicePhotoR
    , getServicePhotosR, postServicePhotosR
    , getServicePhotoDefaultR
    , getServicePhotoEditR
    , getServicePhotoNewR
    , postServicePhotoDeleR
    )

import Handler.Data.Sectors
  ( getSectorsR, postSectorsR
  , getSectorR, postSectorR
  , getSectorNewR
  , getSectorEditR
  , postSectorDeleR
  , getSectorServicesR, postSectorServicesR
  , getSectorServiceR, postSectorServiceR
  , getSectorServiceNewR
  , getSectorServiceEditR
  , postSectorServiceDeleR
  , getSectorServicePhotosR, postSectorServicePhotosR
  , postSectorServicePhotoR
  , getSectorServicePhotoNewR
  , getSectorServicePhotoEditR
  , postSectorServicePhotoDeleR
  , getSectorServiceAssignmentsR, postSectorServiceAssignmentsR
  , getSectorServiceAssignmentR, postSectorServiceAssignmentR
  , getSectorServiceAssignmentNewR
  , getSectorServiceAssignmentEditR
  , postSectorServiceAssignmentDeleR
  )

import Handler.Data.Staff
    ( getStaffR, getEmployeeR, getEmployeeNewR, postStaffR, getEmployeeEditR
    , postEmployeeDeleR, postEmployeeR, getStaffAssignmentsR
    , postStaffAssignmentsR, getStaffAssignmentR, getStaffAssignmentNewR
    , getStaffAssignmentEditR, postStaffAssignmentDeleR, postStaffAssignmentR
    , getStaffScheduleR, getStaffScheduleSlotsR, postStaffScheduleSlotsR
    , getStaffScheduleSlotR, postStaffScheduleSlotR
    , getStaffScheduleSlotNewR, getStaffScheduleSlotEditR
    , postStaffScheduleSlotDeleR, postStaffScheduleFillFromWorkingHoursR
    , postStaffScheduleFillFromPreviousMonthR, getEmployeePhotoR
    )

import Handler.Data.Business
    ( getDataBusinessesR
    , getDataBusinessNewR
    , postDataBusinessesR
    , getDataBusinessR
    , getDataBusinessEditR
    , postDataBusinessDeleR
    , postDataBusinessR
    , getDataBusinessLogoR
    , getDataWorkspacesR
    , getDataWorkspaceNewR
    , postDataWorkspacesR
    , getDataWorkspaceR
    , getDataWorkspaceEditR
    , postDataWorkspaceR
    , postDataWorkspaceDeleR
    , getDataWorkingHoursR
    , getDataWorkingSlotsR, postDataWorkingSlotsR
    , getDataWorkingSlotNewR
    , getDataWorkingSlotR, postDataWorkingSlotR
    , getDataWorkingSlotEditR
    , postDataWorkingSlotDeleR
    , getBusinessServicesR, postBusinessServicesR
    , getBusinessServiceR, postBusinessServiceR
    , getBusinessServiceNewR
    , getBusinessServiceEditR
    , postBusinessServiceDeleR
    , getBusinessServicePhotosR, postBusinessServicePhotosR
    , getBusinessServicePhotoNewR
    , getBusinessServicePhotoEditR
    , postBusinessServicePhotoR
    , postBusinessServicePhotoDeleR
    , getBusinessServiceAssignmentsR, postBusinessServiceAssignmentsR
    , getBusinessServiceAssignmentR, postBusinessServiceAssignmentR
    , getBusinessServiceAssignmentNewR
    , getBusinessServiceAssignmentEditR
    , postBusinessServiceAssignmentDeleR
    , getPayOptionsR
    , postPayOptionsR
    , getPayOptionR
    , postPayOptionR
    , getPayOptionNewR
    , getPayOptionEditR
    , postPayOptionDeleR
    , getWorkspaceServicesR, postWorkspaceServicesR
    , getWorkspaceServiceR, postWorkspaceServiceR
    , getWorkspaceServiceNewR
    , getWorkspaceServiceEditR
    , postWorkspaceServiceDeleR
    , getWorkspaceServicePhotosR, postWorkspaceServicePhotosR
    , getWorkspaceServicePhotoEditR
    , getWorkspaceServicePhotoNewR
    , getWorkspaceServicePhotoR, postWorkspaceServicePhotoR
    , postWorkspaceServicePhotoDeleR
    , getWorkspaceServiceAssignmentsR, postWorkspaceServiceAssignmentsR
    , getWorkspaceServiceAssignmentNewR
    , getWorkspaceServiceAssignmentR, postWorkspaceServiceAssignmentR
    , getWorkspaceServiceAssignmentEditR
    , postWorkspaceServiceAssignmentDeleR
    )

import Handler.Data.Users
    ( getUsersR, getUserR, postUserDeleR, getUserEditR, postUserR
    )
    
import Handler.Data.Tokens
    ( getTokensR, postTokensR, getTokensGoogleapisHookR
    , postTokensGoogleapisClearR
    )

import Handler.Common
    ( getFaviconR, getRobotsR, getSitemapR, getWebAppManifestR
    )

import Stripe ()
import Stripe.Data (Stripe(Stripe))
    
import System.Environment.Blank (getEnv)
import System.Log.FastLogger
    ( defaultBufSize, newStdoutLoggerSet, toLogStr )
    
import Yesod.Auth.Email (saltPass)

import Yookassa ()
import Yookassa.Data (Yookassa(Yookassa))

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    let getAtVenue = AtVenue
    let getStripe = Stripe
    let getYookassa = Yookassa

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePoolWithConfig
        (sqlDatabase $ appDatabaseConf appSettings)
        (appConnectionPoolConfig appSettings)

    -- Perform database migration using our application's logging settings.

    flip runLoggingT logFunc $ flip runSqlPool pool $ do
        
        runMigration migrateAll

        superpass <- liftIO $ saltPass (superuserPassword . appSuperuser $ appSettings)
        insert_ User { userEmail = superuserUsername . appSuperuser $ appSettings
                     , userAuthType = UserAuthTypePassword
                     , userPassword = Just superpass
                     , userVerkey = Nothing
                     , userVerified = True
                     , userName = Just "Super User"
                     , userSuperuser = True
                     , userAdmin = True
                     }
        demo <- liftIO $ getEnv "YESOD_DEMO_LANG"
        case demo of
          Just "FR" -> fillDemoFr appSettings
          Just "RO" -> fillDemoRo appSettings
          Just "RU" -> fillDemoRu appSettings
          _ -> fillDemoEn appSettings
    
    -- runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging $
        (withHeader ("Service-Worker-Allowed","/") . gzip def { gzipFiles = GzipCompress })
        appPlain

withHeader :: Header -> Middleware
withHeader h app req res = app req $ res . addH h

addH :: Header -> W.Response -> W.Response
addH h = mapResponseHeaders (h :)
        

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
