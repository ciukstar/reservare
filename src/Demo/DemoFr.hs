{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoFr (fillDemoFr) where

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
      ( payOptionWorkspace, payOptionType, payOptionName, payOptionGateway, payOptionDescr
      , payOptionIcon, PayOption
      )
    , PayMethod (PayNow, PayAtVenue)
    , PayGateway (PayGatewayStripe)
    , Sector (Sector, sectorName, sectorDescr, sectorParent)
    , StaffPhoto
      ( StaffPhoto, staffPhotoStaff, staffPhotoMime, staffPhotoPhoto
      , staffPhotoAttribution
      )
    , BusinessLogo
      ( BusinessLogo, businessLogoBusiness, businessLogoMime, businessLogoPhoto
      , businessLogoAttribution
      )
    )

import Settings (AppSettings)

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoFr :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoFr appSettings = do

    now <- liftIO getCurrentTime
    let today = utctDay now
    
    let oneHour = nominalDay / 24
    let halfHour = oneHour / 2
    let quarterHour = halfHour / 2
    
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

    insert_ UserPhoto { userPhotoUser = usr2
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/222.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                      }

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

    insert_ UserPhoto { userPhotoUser = usr3
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/2148213406.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                      }

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

    let business1 = Business { businessOwner = usr1
                             , businessName = "FinCo"
                             , businessFullName = Just "Conseil financier, Bernard Jade & Cie"
                             , businessDescr = Just "Finco fournit des conseils financiers pour vous aider à démarrer et à développer votre entreprise."
                             }

    b1 <- insert business1

    insert_ $ BusinessLogo { businessLogoBusiness = b1
                           , businessLogoMime = "image/svg+xml"
                           , businessLogoPhoto = $(embedFile "demo/logo_finco_120x120.svg")
                           , businessLogoAttribution = Nothing
                           }

    let workspace11 = Workspace { workspaceBusiness = b1
                                , workspaceName = "FinCo Centrale"
                                , workspaceAddress = "7 étage, 1967 Route De Longchamp, 58718 Lens"
                                , workspaceTzo = utc
                                , workspaceCurrency = "EUR"
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
                      , payOptionName = "Payez maintenant"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Payer avec une carte de crédit ou PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w11
                      , payOptionType = PayAtVenue
                      , payOptionName = "Payer sur place"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Payer après la prestation du service"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let sector1 = Sector { sectorName = "Tourisme"
                         , sectorDescr = Just "Conseil en tourisme"
                         , sectorParent = Nothing
                         }
    sec1 <- insert sector1

    let sector2 = Sector { sectorName = "Hôtel"
                         , sectorDescr = Just "Hôtellerie et restauration"
                         , sectorParent = Nothing
                         }
    sec2 <- insert sector2

    let sector3 = Sector { sectorName = "Financière"
                         , sectorDescr = Just "Services financiers"
                         , sectorParent = Nothing
                         }
    sec3 <- insert sector3

    let sector4 = Sector { sectorName = "Soins de santé"
                         , sectorDescr = Just "Soins de santé"
                         , sectorParent = Nothing
                         }
    sec4 <- insert sector4

    let sector5 = Sector { sectorName = "Juridique"
                         , sectorDescr = Just "Pratique du droit"
                         , sectorParent = Nothing
                         }
    sec5 <- insert sector5

    let sector6 = Sector { sectorName = "Gestion"
                         , sectorDescr = Just "Conseil en gestion"
                         , sectorParent = Nothing
                         }
    sec6 <- insert sector6

    let sector7 = Sector { sectorName = "IT"
                         , sectorDescr = Just "Conseil en technologies de l'information"
                         , sectorParent = Nothing
                         }
    sec7 <- insert sector7

    let sector8 = Sector { sectorName = "Musique"
                         , sectorDescr = Just "Industrie de la musique"
                         , sectorParent = Nothing
                         }
    sec8 <- insert sector8

    let service111 = Service { serviceWorkspace = w11
                             , serviceName = "Risques et résilience bancaires"
                             , serviceDescr = Just "Nous aidons les clients des services financiers à atteindre des performances extraordinaires ajustées au risque"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s111 <- insert service111

    let service112 = Service { serviceWorkspace = w11
                             , serviceName = "Banque d'investissement et de financement"
                             , serviceDescr = Just "Nous aidons les clients de la banque d'investissement à répondre à un large éventail d'exigences stratégiques, organisationnelles et opérationnelles"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s112 <- insert service112

    let service113 = Service { serviceWorkspace = w11
                             , serviceName = "Cabinets d'avocats et services professionnels"
                             , serviceDescr = Just "We advise professional services institutions, including law firms, accounting firms, corporate legal departments, and other knowledge worker organizations, on a broad range of strategic, organizational, and operational issues"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s113 <- insert service113

    let business2 = Business { businessOwner = usr2
                             , businessName = "TGR & Cie"
                             , businessFullName = Just "Thomas Gabriel Raphaël & Cie"
                             , businessDescr = Just "Thomas Gabriel Raphaël & Compagnie"
                             }

    b21 <- insert business2

    let workspace21 = Workspace { workspaceBusiness = b21
                                , workspaceName = "TGR & Cie Bureau #1"
                                , workspaceAddress = "Apt. 120, 84 Promenade De la Republique, 19703 Le Havre"
                                , workspaceTzo = utc
                                , workspaceCurrency = "EUR"
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
                      , payOptionName = "Payez maintenant"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Payer avec une carte de crédit ou PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w21
                      , payOptionType = PayAtVenue
                      , payOptionName = "Payer sur place"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Payer après la prestation du service"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service211 = Service { serviceWorkspace = w21
                             , serviceName = "Voyages d'agrément"
                             , serviceDescr = Just "Voyagez comme vous le souhaitez"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s211 <- insert service211

    let service212 = Service { serviceWorkspace = w21
                             , serviceName = "Voyage Italie"
                             , serviceDescr = Just "La joie italienne"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s212 <- insert service212

    let service213 = Service { serviceWorkspace = w21
                             , serviceName = "Bonjour Paris"
                             , serviceDescr = Just "France awaits"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s213 <- insert service213

    let workspace22 = Workspace { workspaceBusiness = b21
                                , workspaceName = "TGR & Cie Bureau #2"
                                , workspaceAddress = "9811 Grove Road NORTHAMPTON NN81 7MQ"
                                , workspaceTzo = utc
                                , workspaceCurrency = "EUR"
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
                      , payOptionName = "Payez maintenant"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Payer avec une carte de crédit ou PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w22
                      , payOptionType = PayAtVenue
                      , payOptionName = "Payer sur place"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Payer après la prestation du service"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service221 = Service { serviceWorkspace = w22
                             , serviceName = "Voyages d'agrément"
                             , serviceDescr = Just "Voyagez comme vous le souhaitez"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s221 <- insert service221

    let service222 = Service { serviceWorkspace = w22
                             , serviceName = "Voyage Italie"
                             , serviceDescr = Just "La joie italienne"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s222 <- insert service222

    let service223 = Service { serviceWorkspace = w22
                             , serviceName = "Bonjour Paris"
                             , serviceDescr = Just "La France attend"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s223 <- insert service223

    let business3 = Business { businessOwner = usr3
                             , businessName = "RAL & Cie"
                             , businessFullName = Just "Richard Arthur Louis & Cie"
                             , businessDescr = Just "Richard Arthur Louis & Compagnie"
                             }

    b31 <- insert business3

    let employee1 = Staff { staffName = fromMaybe (userEmail user1) (userName user1)
                          , staffAccount = Just usr1
                          , staffMobile = Just "+33 3122502799"
                          , staffPhone = Just "+33 4480722669"
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
                          , staffMobile = Just "+33 1323257129"
                          , staffPhone = Just "+33 2112838506"
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
                          , staffMobile = Just "+33 2546544622"
                          , staffPhone = Just "+33 5617507392"
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
                          , staffMobile = Just "+33 4179354938"
                          , staffPhone = Just "+33 4720367174"
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
                                   , assignmentRole = "Agent de tourisme"
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
                                   , assignmentRole = "Agent de tourisme"
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
                                    , assignmentRole = "Agent de tourisme"
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
