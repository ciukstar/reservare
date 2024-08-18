{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad (unless, when, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT) 

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
    , Business (Business, businessOwner, businessName, businessFullName, businessDescr)
    , Workspace (Workspace, workspaceBusiness, workspaceName, workspaceAddress)
    , Service
      ( Service, serviceWorkspace, serviceName, serviceDescr, serviceAvailable
      , serviceDuration, serviceType
      )
    , Staff (Staff, staffName, staffAccount, staffMobile, staffPhone)
    , Assignment (Assignment, assignmentService, assignmentStaff, assignmentTime)
    , Workspace (workspaceTzo, workspaceCurrency)
    , Service (servicePrice)
    , Assignment (assignmentSlotInterval, assignmentPriority, assignmentRole)
    , Schedule (Schedule, scheduleStart, scheduleEnd, scheduleAssignment, scheduleDay)
    , WorkingHours
      ( WorkingHours, workingHoursWorkspace, workingHoursDay
      , workingHoursStart, workingHoursEnd
      )
    , PayOption
      ( PayOption, payOptionWorkspace, payOptionType, payOptionName, payOptionGateway
      , payOptionDescr, payOptionIcon
      )
    , PayMethod (PayNow, PayAtVenue)
    , PayGateway (PayGatewayStripe)
    , Sector (Sector, sectorName, sectorDescr, sectorParent)
    , BusinessLogo
      ( BusinessLogo, businessLogoBusiness, businessLogoMime, businessLogoAttribution
      , businessLogoPhoto
      )
    , StaffPhoto
      ( StaffPhoto, staffPhotoStaff, staffPhotoMime, staffPhotoPhoto, staffPhotoAttribution
      )
    )

import Settings (AppSettings)

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoEn :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoEn appSettings = do

    now <- liftIO getCurrentTime
    let today = utctDay now
    
    let oneHour = nominalDay / 24
    let halfHour = oneHour / 2
    let quarterHour = halfHour / 2
    
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
                             , businessName = "FinCo"
                             , businessFullName = Just "Finance consulting, Mary Lopez & Co"
                             , businessDescr = Just "Finco provides financial advice to help you start and grow your business."
                             }

    b1 <- insert business1

    insert_ $ BusinessLogo { businessLogoBusiness = b1
                           , businessLogoMime = "image/svg+xml"
                           , businessLogoPhoto = $(embedFile "demo/logo_finco_120x120.svg")
                           , businessLogoAttribution = Nothing
                           }

    let workspace11 = Workspace { workspaceBusiness = b1
                                , workspaceName = "FinCo Central"
                                , workspaceAddress = "9796 Cherry Court Taylors, SC 29687"
                                , workspaceTzo = utc
                                , workspaceCurrency = "GBP"
                                }

    w11 <- insert workspace11

    let (y,m,_) = toGregorian today

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w11
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 9 0 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w11
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 18 0 0
                                 }
        when (Friday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w11
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 10 30 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w11
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 17 45 0
                                 }

    insert_ PayOption { payOptionWorkspace = w11
                      , payOptionType = PayNow
                      , payOptionName = "Pay now"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Pay using a credit card or PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w11
                      , payOptionType = PayAtVenue
                      , payOptionName = "Pay at venue"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Pay after the service is provided"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let sector1 = Sector { sectorName = "Tourism"
                         , sectorDescr = Just "Tourism consulting"
                         , sectorParent = Nothing
                         }
    sec1 <- insert sector1

    let sector2 = Sector { sectorName = "Hotel"
                         , sectorDescr = Just "Hotel and Hospitality industry"
                         , sectorParent = Nothing
                         }
    sec2 <- insert sector2

    let sector3 = Sector { sectorName = "Financial"
                         , sectorDescr = Just "Financial services"
                         , sectorParent = Nothing
                         }
    sec3 <- insert sector3

    let sector4 = Sector { sectorName = "Healthcare"
                         , sectorDescr = Just "Health care"
                         , sectorParent = Nothing
                         }
    sec4 <- insert sector4

    let sector5 = Sector { sectorName = "Legal"
                         , sectorDescr = Just "Practice of law"
                         , sectorParent = Nothing
                         }
    sec5 <- insert sector5

    let sector6 = Sector { sectorName = "Management"
                         , sectorDescr = Just "Management consulting"
                         , sectorParent = Nothing
                         }
    sec6 <- insert sector6

    let sector7 = Sector { sectorName = "IT"
                         , sectorDescr = Just "Information technology consulting"
                         , sectorParent = Nothing
                         }
    sec7 <- insert sector7

    let sector8 = Sector { sectorName = "Music"
                         , sectorDescr = Just "Music industry"
                         , sectorParent = Nothing
                         }
    sec8 <- insert sector8

    let service111 = Service { serviceWorkspace = w11
                             , serviceName = "Banking Risk & Resilience"
                             , serviceDescr = Just "We help financial services clients achieve extraordinary risk-adjusted performance"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }
    
    s111 <- insert service111

    let service112 = Service { serviceWorkspace = w11
                             , serviceName = "Corporate & Investment Banking"
                             , serviceDescr = Just "We help investment banking clients meet a wide range of strategic, organizational, and operational demands"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s112 <- insert service112

    let service113 = Service { serviceWorkspace = w11
                             , serviceName = "Law Firms & Professional Services"
                             , serviceDescr = Just "We advise professional services institutions, including law firms, accounting firms, corporate legal departments, and other knowledge worker organizations, on a broad range of strategic, organizational, and operational issues"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s113 <- insert service113

    let business2 = Business { businessOwner = usr2
                             , businessName = "JJ & Co"
                             , businessFullName = Just "John Johnson & Co"
                             , businessDescr = Just "John Johnson & Company"
                             }

    b2 <- insert business2

    let workspace21 = Workspace { workspaceBusiness = b2
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

    insert_ PayOption { payOptionWorkspace = w21
                      , payOptionType = PayNow
                      , payOptionName = "Pay now"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Pay using a credit card or PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w21
                      , payOptionType = PayAtVenue
                      , payOptionName = "Pay at venue"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Pay after the service is provided"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service211 = Service { serviceWorkspace = w21
                             , serviceName = "Leisure travel"
                             , serviceDescr = Just "Travel as you like"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s211 <- insert service211

    let service212 = Service { serviceWorkspace = w21
                             , serviceName = "Italia travel"
                             , serviceDescr = Just "Italian joy"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s212 <- insert service212

    let service213 = Service { serviceWorkspace = w21
                             , serviceName = "Hello Paris"
                             , serviceDescr = Just "France awaits"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s213 <- insert service213

    let workspace22 = Workspace { workspaceBusiness = b2
                                , workspaceName = "JJ & Co Office #2"
                                , workspaceAddress = "9811 Grove Road NORTHAMPTON NN81 7MQ"
                                , workspaceTzo = utc
                                , workspaceCurrency = "GBP"
                                }

    w22 <- insert workspace22

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w22
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 9 0 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w22
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 18 0 0
                                 }
        when (Friday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w22
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 10 30 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w22
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 17 45 0
                                 }

    insert_ PayOption { payOptionWorkspace = w22
                      , payOptionType = PayNow
                      , payOptionName = "Pay now"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Pay using a credit card or PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w22
                      , payOptionType = PayAtVenue
                      , payOptionName = "Pay at venue"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Pay after the service is provided"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service221 = Service { serviceWorkspace = w22
                             , serviceName = "Leisure travel"
                             , serviceDescr = Just "Travel as you like"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s221 <- insert service221

    let service222 = Service { serviceWorkspace = w22
                             , serviceName = "Italia travel"
                             , serviceDescr = Just "Italian joy"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s222 <- insert service222

    let service223 = Service { serviceWorkspace = w22
                             , serviceName = "Hello Paris"
                             , serviceDescr = Just "France awaits"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s223 <- insert service223

    let business3 = Business { businessOwner = usr3
                             , businessName = "JM & Co"
                             , businessFullName = Just "Julian Maulsby & Co"
                             , businessDescr = Just "Julian Maulsby & Company"
                             }

    b3 <- insert business3

    let workspace31 = Workspace { workspaceBusiness = b3
                                , workspaceName = "JM & Co Central"
                                , workspaceAddress = "7 Mill Road SOUTHEND-ON-SEA SS35 4IL"
                                , workspaceTzo = utc
                                , workspaceCurrency = "GBP"
                                }

    w31 <- insert workspace31

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w31
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 9 0 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w31
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 18 0 0
                                 }
        when (Friday == dayOfWeek day) $ do
            insert_ WorkingHours { workingHoursWorkspace = w31
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 10 30 0
                                 , workingHoursEnd = TimeOfDay 13 0 0
                                 }
            insert_ WorkingHours { workingHoursWorkspace = w31
                                 , workingHoursDay = day
                                 , workingHoursStart = TimeOfDay 14 0 0
                                 , workingHoursEnd = TimeOfDay 17 45 0
                                 }

    insert_ PayOption { payOptionWorkspace = w31
                      , payOptionType = PayNow
                      , payOptionName = "Pay now"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Pay using a credit card or PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w31
                      , payOptionType = PayAtVenue
                      , payOptionName = "Pay at venue"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Pay after the service is provided"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let employee1 = Staff { staffName = fromMaybe (userEmail user1) (userName user1)
                          , staffAccount = Just usr1
                          , staffMobile = Just "+44 07366 783859"
                          , staffPhone = Just "+44 09691 575048"
                          }

    empl1 <- insert employee1

    insert_ $ StaffPhoto { staffPhotoStaff = empl1
                         , staffPhotoMime = "image/avif"
                         , staffPhotoPhoto = $(embedFile "demo/2148728586.avif")
                         , staffPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                         }

    let employee2 = Staff { staffName = fromMaybe (userEmail user2) (userName user2)
                          , staffAccount = Just usr2
                          , staffMobile = Just "+44 01788 494865"
                          , staffPhone = Just "+44 02774 546857"
                          }

    empl2 <- insert employee2

    insert_ $ StaffPhoto { staffPhotoStaff = empl2
                         , staffPhotoMime = "image/avif"
                         , staffPhotoPhoto = $(embedFile "demo/222.avif")
                         , staffPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                         }

    let employee3 = Staff { staffName = fromMaybe (userEmail user3) (userName user3)
                          , staffAccount = Just usr3
                          , staffMobile = Just "+44 7103 26014564"
                          , staffPhone = Just "+44 7103 26014564"
                          }

    empl3 <- insert employee3

    insert_ $ StaffPhoto { staffPhotoStaff = empl3
                         , staffPhotoMime = "image/avif"
                         , staffPhotoPhoto = $(embedFile "demo/2148213406.avif")
                         , staffPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                         }

    let employee4 = Staff { staffName = fromMaybe (userEmail user4) (userName user4)
                          , staffAccount = Just usr4
                          , staffMobile = Just "+44 4759604365"
                          , staffPhone = Just "+44 4759604365"
                          }

    empl4 <- insert employee4

    insert_ $ StaffPhoto { staffPhotoStaff = empl4
                         , staffPhotoMime = "image/avif"
                         , staffPhotoPhoto = $(embedFile "demo/2148728638.avif")
                         , staffPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                         }

    let assignment111 = Assignment { assignmentStaff = empl1
                                   , assignmentService = s111
                                   , assignmentRole = "Tourism agent"
                                   , assignmentTime = now
                                   , assignmentSlotInterval = quarterHour
                                   , assignmentPriority = 1
                                   }

    assig111 <- insert assignment111

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment221 = Assignment { assignmentStaff = empl2
                                   , assignmentService = s112
                                   , assignmentRole = "Tourism agent"
                                   , assignmentTime = now
                                   , assignmentSlotInterval = halfHour
                                   , assignmentPriority = 1
                                   }

    assig221 <- insert assignment221

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig221
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig221
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig221
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig221
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment331 = Assignment { assignmentStaff = empl3
                                    , assignmentService = s113
                                    , assignmentRole = "Tourism agent"
                                    , assignmentTime = now
                                    , assignmentSlotInterval = oneHour
                                    , assignmentPriority = 1
                                    }

    assig331 <- insert assignment331

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig331
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig331
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig331
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig331
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }
