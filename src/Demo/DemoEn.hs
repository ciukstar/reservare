{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad (unless, when, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS (readFile)

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
    , ServicePhoto
      ( ServicePhoto, servicePhotoService, servicePhotoMime, servicePhotoAttribution
      , servicePhotoPhoto
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

    let freepik = [shamlet|
                          Designed by #
                          <a.link href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    liftIO (BS.readFile "demo/2148728586.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = usr1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
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

    liftIO (BS.readFile "demo/222.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = usr2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
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

    liftIO (BS.readFile "demo/2148213406.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = usr3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
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

    liftIO (BS.readFile "demo/2148728638.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = usr4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik

                        }

    let business1 = Business { businessOwner = usr1
                             , businessName = "FinCo"
                             , businessFullName = Just "Finance consulting, Mary Lopez & Co"
                             , businessDescr = Just "Finco provides financial advice to help you start and grow your business."
                             }

    b1 <- insert business1

    liftIO (BS.readFile "demo/logo_finco_120x120.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b1
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto =  bs
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

    liftIO (BS.readFile "demo/logo_finco_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s111
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/banking_risk_resilience_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s111
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    liftIO (BS.readFile "demo/banking_risk_resilience_2.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s111
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    let service112 = Service { serviceWorkspace = w11
                             , serviceName = "Corporate & Investment Banking"
                             , serviceDescr = Just "We help investment banking clients meet a wide range of strategic, organizational, and operational demands"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = 1.25 * oneHour
                             , serviceType = Just sec3
                             }

    s112 <- insert service112

    liftIO (BS.readFile "demo/logo_finco_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s112
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/corporate_investment_banking_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s112
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    liftIO (BS.readFile "demo/corporate_investment_banking_2.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s112
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    let service113 = Service { serviceWorkspace = w11
                             , serviceName = "Law Firms & Professional Services"
                             , serviceDescr = Just "We advise professional services institutions, including law firms, accounting firms, corporate legal departments, and other knowledge worker organizations, on a broad range of strategic, organizational, and operational issues"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = 1.5 * oneHour
                             , serviceType = Just sec3
                             }

    s113 <- insert service113

    liftIO (BS.readFile "demo/logo_finco_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s113
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/law_firms_professional_services_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s113
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    liftIO (BS.readFile "demo/law_firms_professional_services_2.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s113
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    let business2 = Business { businessOwner = usr2
                             , businessName = "MedCab"
                             , businessFullName = Just "MedCab, John Johnson & Co"
                             , businessDescr = Just "We provide patient-centered healthcare with excellence in quality, service, and access"
                             }

    b2 <- insert business2

    liftIO (BS.readFile "demo/logo_medcab_120x120.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b2
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto = bs
                             , businessLogoAttribution = Nothing
                             }

    let workspace21 = Workspace { workspaceBusiness = b2
                                , workspaceName = "MedCab, Office #1"
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
                             , serviceName = "Community physiotherapy"
                             , serviceDescr = Just "Community Physiotherapy aims to optimise physical function and enhance independence and quality of life for eligible people within the community"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = 1.75 * oneHour
                             , serviceType = Just sec4
                             }

    s211 <- insert service211

    liftIO (BS.readFile "demo/logo_medcab_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s211
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/community_physiotherapy_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s211
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    liftIO (BS.readFile "demo/community_physiotherapy_2.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s211
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    let service212 = Service { serviceWorkspace = w21
                             , serviceName = "Musculoskeletal therapy"
                             , serviceDescr = Just "Musculoskeletal physiotherapy will focus on the biomechanical and structural rehabilitation of the client"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = 2 * oneHour
                             , serviceType = Just sec4
                             }

    s212 <- insert service212

    liftIO (BS.readFile "demo/logo_medcab_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s212
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/musculoskeletal_therapy_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s212
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    liftIO (BS.readFile "demo/musculoskeletal_therapy_2.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s212
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }


    let service213 = Service { serviceWorkspace = w21
                             , serviceName = "Community podiatry"
                             , serviceDescr = Just "Provides assessment, diagnosis and treatment to a range of patients with lower limb and foot problems, including"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = 2.25 * oneHour
                             , serviceType = Just sec4
                             }

    s213 <- insert service213

    liftIO (BS.readFile "demo/logo_medcab_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s213
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/community_podiatry_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s213
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    liftIO (BS.readFile "demo/community_podiatry_2.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s213
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }
    
    let workspace22 = Workspace { workspaceBusiness = b2
                                , workspaceName = "MedCab, Office #2"
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
                             , serviceName = "Community speech and language therapy"
                             , serviceDescr = Just "The Community Speech and Language team provides assessment, therapy and advice to adults with speech, language, communication and swallowing difficulties"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = 2.5 * oneHour
                             , serviceType = Just sec4
                             }

    s221 <- insert service221

    liftIO (BS.readFile "demo/logo_medcab_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s221
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/community_speech_and_language_therapy_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s221
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    liftIO (BS.readFile "demo/community_speech_and_language_therapy_2.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s221
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    let service222 = Service { serviceWorkspace = w22
                             , serviceName = "Falls prevention services"
                             , serviceDescr = Just "Our falls prevention service provides assessment, advice and exercise for older people who are at risk of falling"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = 2.75 * oneHour
                             , serviceType = Just sec4
                             }

    s222 <- insert service222

    liftIO (BS.readFile "demo/logo_medcab_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s222
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/AdobeStock_144297416.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s222
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/AdobeStock_232406830.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s222
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/AdobeStock_393507521.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s222
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    let service223 = Service { serviceWorkspace = w22
                             , serviceName = "Bed-based community rehabilitation"
                             , serviceDescr = Just "The team will offer you time limited support you to regain your independence after a hospital admission and help you plan a safe departure home when you are ‘functionally fit’"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = 3 * oneHour
                             , serviceType = Just sec4
                             }

    s223 <- insert service223

    liftIO (BS.readFile "demo/logo_medcab_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s223
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/bed_based_community_rehabilitation_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s223
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    let business3 = Business { businessOwner = usr3
                             , businessName = "Oasis"
                             , businessFullName = Just "Oasis, Julian Maulsby & Co"
                             , businessDescr = Just "Oasis is a hospitality consulting firm offering comprehensive consulting services through a diverse team of consultants"
                             }

    b3 <- insert business3

    liftIO (BS.readFile "demo/logo_oasis_120x120.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b3
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto =  bs
                             , businessLogoAttribution = Nothing
                             }

    let workspace31 = Workspace { workspaceBusiness = b3
                                , workspaceName = "Oasis Central"
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

    let service311 = Service { serviceWorkspace = w31
                             , serviceName = "Asset Management"
                             , serviceDescr = Just "Oasis Asset Management is a team of hotel experts that is focused on optimizing the returns from operating hotel business"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = 3.25 * oneHour
                             , serviceType = Just sec2
                             }

    s311 <- insert service311

    liftIO (BS.readFile "demo/logo_oasis_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s311
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/asset_management_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s311
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    let service312 = Service { serviceWorkspace = w31
                             , serviceName = "Investment Advisory"
                             , serviceDescr = Just "Oasis Investment Advisory specializes in BUY-SIDE and SELL-SIDE activities for existing and proposed hotel assets"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = 3.5 * oneHour
                             , serviceType = Just sec2
                             }

    s312 <- insert service312

    liftIO (BS.readFile "demo/logo_oasis_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s312
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/investment_advisory_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s312
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    let service313 = Service { serviceWorkspace = w31
                             , serviceName = "Project Execution Planning & Advisory"
                             , serviceDescr = Just "Oasis offers Project Execution Planning & Advisory (PEPA) services that provide strategic guidance and leadership throughout the initial development stages of a hotel project"
                             , servicePrice = 30000
                             , serviceAvailable = True
                             , serviceDuration = 3.75 * oneHour
                             , serviceType = Just sec2
                             }

    s313 <- insert service313

    liftIO (BS.readFile "demo/logo_oasis_120x120.svg") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s313
                           , servicePhotoMime = "image/svg+xml"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Nothing
                           }

    liftIO (BS.readFile "demo/project_execution_planning_advisory_1.avif") >>= \bs ->
      insert_ ServicePhoto { servicePhotoService = s313
                           , servicePhotoMime = "image/avif"
                           , servicePhotoPhoto = bs
                           , servicePhotoAttribution = Just freepik
                           }

    let employee1 = Staff { staffName = fromMaybe (userEmail user1) (userName user1)
                          , staffAccount = Just usr1
                          , staffMobile = Just "+44 07366 783859"
                          , staffPhone = Just "+44 09691 575048"
                          }

    empl1 <- insert employee1

    liftIO (BS.readFile "demo/2148728586.avif") >>= \bs ->
      insert_ $ StaffPhoto { staffPhotoStaff = empl1
                           , staffPhotoMime = "image/avif"
                           , staffPhotoPhoto = bs
                           , staffPhotoAttribution = Just freepik
                           }

    let employee2 = Staff { staffName = fromMaybe (userEmail user2) (userName user2)
                          , staffAccount = Just usr2
                          , staffMobile = Just "+44 01788 494865"
                          , staffPhone = Just "+44 02774 546857"
                          }

    empl2 <- insert employee2

    liftIO (BS.readFile "demo/222.avif") >>= \bs ->
      insert_ $ StaffPhoto { staffPhotoStaff = empl2
                           , staffPhotoMime = "image/avif"
                           , staffPhotoPhoto = bs
                           , staffPhotoAttribution = Just freepik
                           }

    let employee3 = Staff { staffName = fromMaybe (userEmail user3) (userName user3)
                          , staffAccount = Just usr3
                          , staffMobile = Just "+44 7103 26014564"
                          , staffPhone = Just "+44 7103 26014564"
                          }

    empl3 <- insert employee3

    liftIO (BS.readFile "demo/2148213406.avif") >>= \bs ->
      insert_ $ StaffPhoto { staffPhotoStaff = empl3
                           , staffPhotoMime = "image/avif"
                           , staffPhotoPhoto = bs
                           , staffPhotoAttribution = Just freepik
                           }

    let employee4 = Staff { staffName = fromMaybe (userEmail user4) (userName user4)
                          , staffAccount = Just usr4
                          , staffMobile = Just "+44 4759604365"
                          , staffPhone = Just "+44 4759604365"
                          }

    empl4 <- insert employee4

    liftIO (BS.readFile "demo/2148728638.avif") >>= \bs ->
      insert_ $ StaffPhoto { staffPhotoStaff = empl4
                           , staffPhotoMime = "image/avif"
                           , staffPhotoPhoto = bs
                           , staffPhotoAttribution = Just freepik
                           }

    let employee5 = Staff { staffName = "Robert Black"
                          , staffAccount = Nothing
                          , staffMobile = Just "+44 4759604365"
                          , staffPhone = Just "+44 4759604365"
                          }

    empl5 <- insert employee5

    liftIO (BS.readFile "demo/employee_5.avif") >>= \bs ->
      insert_ $ StaffPhoto { staffPhotoStaff = empl5
                           , staffPhotoMime = "image/avif"
                           , staffPhotoPhoto = bs
                           , staffPhotoAttribution = Just freepik
                           }

    let assignment111 = Assignment { assignmentStaff = empl1
                                   , assignmentService = s111
                                   , assignmentRole = "Senior Risk Manager"
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
                                   , assignmentRole = "Analyst"
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
                                    , assignmentRole = "Associate Attorney"
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

    let assignment42121 = Assignment { assignmentStaff = empl4
                                     , assignmentService = s212
                                     , assignmentRole = "Physical Therapist"
                                     , assignmentTime = now
                                     , assignmentSlotInterval = oneHour
                                     , assignmentPriority = 1
                                     }

    assig42121 <- insert assignment42121

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42121
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42121
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42121
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42121
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment42131 = Assignment { assignmentStaff = empl4
                                     , assignmentService = s213
                                     , assignmentRole = "Community Podiatrist"
                                     , assignmentTime = now
                                     , assignmentSlotInterval = oneHour
                                     , assignmentPriority = 1
                                     }

    assig42131 <- insert assignment42131

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42131
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42131
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42131
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42131
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment42111 = Assignment { assignmentStaff = empl4
                                     , assignmentService = s211
                                     , assignmentRole = "Community Neuro-Physiotherapist"
                                     , assignmentTime = now
                                     , assignmentSlotInterval = oneHour
                                     , assignmentPriority = 1
                                     }

    assig42111 <- insert assignment42111

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment42221 = Assignment { assignmentStaff = empl4
                                     , assignmentService = s222
                                     , assignmentRole = "Clinical Support Worker"
                                     , assignmentTime = now
                                     , assignmentSlotInterval = oneHour
                                     , assignmentPriority = 1
                                     }

    assig42221 <- insert assignment42221

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42221
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42221
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42221
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42221
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment42211 = Assignment { assignmentStaff = empl4
                                     , assignmentService = s221
                                     , assignmentRole = "Community Speech and Language Therapist"
                                     , assignmentTime = now
                                     , assignmentSlotInterval = oneHour
                                     , assignmentPriority = 1
                                     }

    assig42211 <- insert assignment42211

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42211
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42211
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42211
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42211
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment42231 = Assignment { assignmentStaff = empl4
                                     , assignmentService = s223
                                     , assignmentRole = "Nursing Rehab Assistant"
                                     , assignmentTime = now
                                     , assignmentSlotInterval = oneHour
                                     , assignmentPriority = 1
                                     }

    assig42231 <- insert assignment42231

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42231
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42231
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig42231
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig42231
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment53131 = Assignment { assignmentStaff = empl5
                                     , assignmentService = s313
                                     , assignmentRole = "Project Manager"
                                     , assignmentTime = now
                                     , assignmentSlotInterval = oneHour
                                     , assignmentPriority = 1
                                     }

    assig53131 <- insert assignment53131

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig53131
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig53131
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig53131
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig53131
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment53121 = Assignment { assignmentStaff = empl5
                                     , assignmentService = s312
                                     , assignmentRole = "Financial advisor"
                                     , assignmentTime = now
                                     , assignmentSlotInterval = oneHour
                                     , assignmentPriority = 1
                                     }

    assig53121 <- insert assignment53121

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig53121
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig53121
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig53121
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig53121
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }

    let assignment53111 = Assignment { assignmentStaff = empl5
                                     , assignmentService = s311
                                     , assignmentRole = "Equity analyst"
                                     , assignmentTime = now
                                     , assignmentSlotInterval = oneHour
                                     , assignmentPriority = 1
                                     }

    assig53111 <- insert assignment53111

    forM_ [periodFirstDay (YearMonth y m) .. periodLastDay (YearMonth y m)] $ \day -> do
        unless (Friday == dayOfWeek day || Saturday == dayOfWeek day || Sunday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig53111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 9 0 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig53111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 18 0 0
                             }
        when (Friday == dayOfWeek day) $ do
            insert_ Schedule { scheduleAssignment = assig53111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 10 30 0
                             , scheduleEnd = TimeOfDay 13 0 0
                             }
            insert_ Schedule { scheduleAssignment = assig53111
                             , scheduleDay = day
                             , scheduleStart = TimeOfDay 14 0 0
                             , scheduleEnd = TimeOfDay 17 45 0
                             }
