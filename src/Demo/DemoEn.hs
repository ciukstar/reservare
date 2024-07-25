{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Import.NoFoundation
    ( forM_, ReaderT, Workspace (workspaceTzo, workspaceCurrency)
    , Service (servicePrice)
    , Assignment (assignmentSlotInterval, assignmentPriority, assignmentRole)
    , WorkingHours
      ( WorkingHours, workingHoursWorkspace, workingHoursDay
      , workingHoursStart, workingHoursEnd
      ), unless, when
    )

import Data.FileEmbed (embedFile)
import Data.Maybe (fromMaybe)
import Data.Time
    ( getCurrentTime, utc, nominalDay, dayOfWeek, utctDay
    , TimeOfDay (TimeOfDay), DayOfWeek (Saturday, Sunday, Friday)
    , DayPeriod (periodFirstDay, periodLastDay)
    )
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.Month (pattern YearMonth)

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( AuthenticationType (UserAuthTypeEmail)
    , User
      ( User, userEmail, userAuthType, userPassword, userVerkey, userVerified
      , userName, userSuperuser, userAdmin
      )
    , UserPhoto
      ( UserPhoto, userPhotoMime, userPhotoPhoto, userPhotoUser
      , userPhotoAttribution
      )
    , Business (Business, businessOwner, businessName)
    , Workspace (Workspace, workspaceBusiness, workspaceName, workspaceAddress)
    , Service (Service, serviceWorkspace, serviceName, serviceDescr)
    , Staff (Staff, staffName, staffAccount, staffMobile, staffPhone)
    , Assignment (Assignment, assignmentService, assignmentStaff, assignmentTime)
    )

import Settings (AppSettings)

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoEn :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoEn appSettings = do

    now <- liftIO getCurrentTime
    let today = utctDay now
    
    pass1 <- liftIO $ saltPass "marylopez"
    let user1 = User { userEmail = "marylopez@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Mary Lopez"
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

    pass2 <- liftIO $ saltPass "jjohnson"
    let user2 = User { userEmail = "jjohnson@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "John Johnson"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr2 <- insert user2

    insert_ UserPhoto { userPhotoUser = usr2
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/222.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                      }

    pass3 <- liftIO $ saltPass "jmaulsby"
    let user3 = User { userEmail = "jmaulsby@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Julian Maulsby"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr3 <- insert user3

    insert_ UserPhoto { userPhotoUser = usr3
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/2148213406.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                      }

    pass4 <- liftIO $ saltPass "vschoen"
    let user4 = User { userEmail = "vschoen@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Valentina Schoen"
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

    let business1 = Business { businessOwner = usr1
                             , businessName = "ML & Co"
                             }

    b1 <- insert business1

    let workspace1 = Workspace { workspaceBusiness = b1
                               , workspaceName = "ML & Co Central"
                               , workspaceAddress = "9796 Cherry Court Taylors, SC 29687"
                               , workspaceTzo = utc
                               , workspaceCurrency = "GBP"
                               }

    w1 <- insert workspace1

    let (y,m,_) = toGregorian today

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w1
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 9 0 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w1
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 18 0 0
                                 }
        when (Friday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w1
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 10 30 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w1
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 17 45 0
                                 }

    let service1 = Service { serviceWorkspace = w1
                           , serviceName = "Leisure travel"
                           , serviceDescr = Just "Travel as you like"
                           , servicePrice = 10000
                           }

    s1 <- insert service1

    let service2 = Service { serviceWorkspace = w1
                           , serviceName = "Italia travel"
                           , serviceDescr = Just "Italian joy"
                           , servicePrice = 20000
                           }

    s2 <- insert service2

    let service3 = Service { serviceWorkspace = w1
                           , serviceName = "Hello Paris"
                           , serviceDescr = Just "France awaits"
                           , servicePrice = 25000
                           }

    s3 <- insert service3

    let business2 = Business { businessOwner = usr2
                             , businessName = "JJ & Co"
                             }

    b21 <- insert business2

    let workspace21 = Workspace { workspaceBusiness = b21
                                , workspaceName = "JJ & Co Office #1"
                                , workspaceAddress = "9 Queensway SOUTHALL UB72 2KS"
                                , workspaceTzo = utc
                                , workspaceCurrency = "GBP"
                                }

    w21 <- insert workspace21

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w21
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 9 0 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w21
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 18 0 0
                                 }
        when (Friday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w21
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 10 30 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w21
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 17 45 0
                                 }

    let service211 = Service { serviceWorkspace = w21
                             , serviceName = "Leisure travel"
                             , serviceDescr = Just "Travel as you like"
                             , servicePrice = 10000
                             }

    s211 <- insert service211

    let service212 = Service { serviceWorkspace = w21
                             , serviceName = "Italia travel"
                             , serviceDescr = Just "Italian joy"
                             , servicePrice = 20000
                             }

    s212 <- insert service212

    let service213 = Service { serviceWorkspace = w21
                             , serviceName = "Hello Paris"
                             , serviceDescr = Just "France awaits"
                             , servicePrice = 25000
                             }

    s213 <- insert service213

    let workspace22 = Workspace { workspaceBusiness = b21
                                , workspaceName = "JJ & Co Office #2"
                                , workspaceAddress = "9811 Grove Road NORTHAMPTON NN81 7MQ"
                                , workspaceTzo = utc
                                , workspaceCurrency = "GBP"
                                }

    w22 <- insert workspace22

    let service221 = Service { serviceWorkspace = w22
                             , serviceName = "Leisure travel"
                             , serviceDescr = Just "Travel as you like"
                             , servicePrice = 10000
                             }

    s221 <- insert service221

    let service222 = Service { serviceWorkspace = w22
                             , serviceName = "Italia travel"
                             , serviceDescr = Just "Italian joy"
                             , servicePrice = 20000
                             }

    s222 <- insert service222

    let service223 = Service { serviceWorkspace = w22
                             , serviceName = "Hello Paris"
                             , serviceDescr = Just "France awaits"
                             , servicePrice = 25000
                             }

    s223 <- insert service223

    let business3 = Business { businessOwner = usr3
                             , businessName = "JM & Co"
                             }

    b31 <- insert business3

    let employee1 = Staff { staffName = fromMaybe (userEmail user1) (userName user1)
                          , staffAccount = Just usr1
                          , staffMobile = Just "(206) 342-8631"
                          , staffPhone = Just "(717) 550-1675"
                          }

    empl1 <- insert employee1
    let oneHour = nominalDay / 24
    let halfHour = oneHour / 2
    let quarterHour = halfHour / 2

    let assignment1 = Assignment { assignmentStaff = empl1
                                 , assignmentService = s1
                                 , assignmentRole = "Tourism agent"
                                 , assignmentTime = now
                                 , assignmentSlotInterval = quarterHour
                                 , assignmentPriority = 1
                                 }

    assig1 <- insert assignment1

    let assignment2 = Assignment { assignmentStaff = empl1
                                 , assignmentService = s2
                                 , assignmentRole = "Tourism agent"
                                 , assignmentTime = now
                                 , assignmentSlotInterval = halfHour
                                 , assignmentPriority = 1
                                 }

    assig2 <- insert assignment2

    let assignment3 = Assignment { assignmentStaff = empl1
                                 , assignmentService = s3
                                 , assignmentRole = "Tourism agent"
                                 , assignmentTime = now
                                 , assignmentSlotInterval = oneHour
                                 , assignmentPriority = 1
                                 }

    assig3 <- insert assignment3
        
    return ()
