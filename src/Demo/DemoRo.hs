{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoRo (fillDemoRo) where

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
    , StaffPhoto
      ( StaffPhoto, staffPhotoStaff, staffPhotoMime, staffPhotoPhoto
      , staffPhotoAttribution
      )
    , BusinessLogo
      ( BusinessLogo, businessLogoBusiness, businessLogoMime, businessLogoPhoto
      , businessLogoAttribution
      )
    , ServicePhoto
      ( ServicePhoto, servicePhotoService, servicePhotoMime, servicePhotoPhoto
      , servicePhotoAttribution
      )
    )

import Settings (AppSettings)

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoRo :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoRo appSettings = do

    now <- liftIO getCurrentTime
    let today = utctDay now
    
    let oneHour = nominalDay / 24
    let halfHour = oneHour / 2
    let quarterHour = halfHour / 2
    
    pass1 <- liftIO $ saltPass "raduam"
    let user1 = User { userEmail = "raduam@mail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Radu Ana-Maria"
                     , userSuperuser = False
                     , userAdmin = True
                     }

    usr1 <- insert user1

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    liftIO (BS.readFile "demo/2148728586.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = usr1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "ionescuav"
    let user2 = User { userEmail = "ionescuav@mail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Ionescu Alexandru Victor"
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

    pass3 <- liftIO $ saltPass "rususa"
    let user3 = User { userEmail = "rususa@mail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Rusu Ştefan Alexandru"
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

    pass4 <- liftIO $ saltPass "mateiaa"
    let user4 = User { userEmail = "mateiaa@mail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Matei Andreea Alexandra"
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
                             , businessFullName = Just "Consultanță financiară, Radu Ana-Maria & Co"
                             , businessDescr = Just "Finco oferă sfaturi financiare pentru a vă ajuta să începeți și să vă dezvoltați afacerea."
                             }

    b1 <- insert business1

    liftIO (BS.readFile "demo/logo_finco_120x120.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b1
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto = bs
                             , businessLogoAttribution = Nothing
                             }

    let workspace11 = Workspace { workspaceBusiness = b1
                                , workspaceName = "RAM & Co Central"
                                , workspaceAddress = "11043, STR. MICLE VERONICA nr. 24 bl. C1 ap. 39"
                                , workspaceTzo = utc
                                , workspaceCurrency = "RON"
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
                      , payOptionName = "Plătește acum"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Plătiți folosind un card de credit sau PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w11
                      , payOptionType = PayAtVenue
                      , payOptionName = "Plătiți la locație"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Plătiți după prestarea serviciului"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let sector1 = Sector { sectorName = "Turism"
                         , sectorDescr = Just "Consultanță în turism"
                         , sectorParent = Nothing
                         }
    sec1 <- insert sector1

    let sector2 = Sector { sectorName = "Hotel"
                         , sectorDescr = Just "Industria hotelieră și a ospitalității"
                         , sectorParent = Nothing
                         }
    sec2 <- insert sector2

    let sector3 = Sector { sectorName = "Financiar"
                         , sectorDescr = Just "Servicii financiare"
                         , sectorParent = Nothing
                         }
    sec3 <- insert sector3

    let sector4 = Sector { sectorName = "Sănătate"
                         , sectorDescr = Just "Servicii de asistență medicală"
                         , sectorParent = Nothing
                         }
    sec4 <- insert sector4

    let sector5 = Sector { sectorName = "Juridică"
                         , sectorDescr = Just "Practica dreptului"
                         , sectorParent = Nothing
                         }
    sec5 <- insert sector5

    let sector6 = Sector { sectorName = "Management"
                         , sectorDescr = Just "Consultanță în management"
                         , sectorParent = Nothing
                         }
    sec6 <- insert sector6

    let sector7 = Sector { sectorName = "IT"
                         , sectorDescr = Just "Consultanță în tehnologia informației"
                         , sectorParent = Nothing
                         }
    sec7 <- insert sector7

    let sector8 = Sector { sectorName = "Muzică"
                         , sectorDescr = Just "Industria muzicală"
                         , sectorParent = Nothing
                         }
    sec8 <- insert sector8

    let service111 = Service { serviceWorkspace = w11
                             , serviceName = "Risc bancar și rezistență"
                             , serviceDescr = Just "Ajutăm clienții din serviciile financiare să obțină o performanță extraordinară ajustată la risc"
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
                             , serviceName = "Servicii bancare pentru corporate și investiții"
                             , serviceDescr = Just "Ajutăm clienții din sectorul bancar de investiții să îndeplinească o gamă largă de cerințe strategice, organizaționale și operaționale"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                             , serviceName = "Firme de avocatură și servicii profesionale"
                             , serviceDescr = Just "Consiliem instituții de servicii profesionale, inclusiv firme de avocatură, firme de contabilitate, departamente juridice corporative și alte organizații de lucrători ai cunoștințelor, cu privire la o gamă largă de probleme strategice, organizaționale și operaționale"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                             , businessFullName = Just "MedCab, Ionescu Alexandru Victor & Co"
                             , businessDescr = Just "Oferim asistență medicală centrată pe pacient cu excelență în calitate, servicii și acces"
                             }

    b2 <- insert business2

    liftIO (BS.readFile "demo/logo_medcab_120x120.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b2
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto = bs
                             , businessLogoAttribution = Nothing
                             }

    let workspace21 = Workspace { workspaceBusiness = b2
                                , workspaceName = "MedCab, Biroul nr. 1"
                                , workspaceAddress = "STR. FRASINULUI nr. 1 sc. B ap. 13, TIMIŞ"
                                , workspaceTzo = utc
                                , workspaceCurrency = "RON"
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
                      , payOptionName = "Plătește acum"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Plătiți folosind un card de credit sau PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w21
                      , payOptionType = PayAtVenue
                      , payOptionName = "Plătiți la locație"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Plătiți după prestarea serviciului"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service211 = Service { serviceWorkspace = w21
                             , serviceName = "Kinetoterapie comunitară"
                             , serviceDescr = Just "Fizioterapia comunitară își propune să optimizeze funcția fizică și să sporească independența și calitatea vieții pentru persoanele eligibile din comunitate"
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                             , serviceName = "Terapie musculo-scheletică"
                             , serviceDescr = Just "Kinetoterapie musculo-scheletică se va concentra pe reabilitarea biomecanică și structurală a clientului"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                             , serviceName = "Podiatrie comunitară"
                             , serviceDescr = Just "Oferă evaluare, diagnostic și tratament unei game de pacienți cu probleme la membrele inferioare și la picioare, inclusiv"
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                                , workspaceName = "MedCab, Biroul nr. 2"
                                , workspaceAddress = "STR. PRIMĂVERII nr. 2 bl. S4 ap. 352, CLUJ"
                                , workspaceTzo = utc
                                , workspaceCurrency = "RON"
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
                      , payOptionName = "Plătește acum"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Plătiți folosind un card de credit sau PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w22
                      , payOptionType = PayAtVenue
                      , payOptionName = "Plătiți la locație"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Plătiți după prestarea serviciului"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service221 = Service { serviceWorkspace = w22
                             , serviceName = "Terapie comunitară de vorbire și limbaj"
                             , serviceDescr = Just "Echipa comunitară de vorbire și limbaj oferă evaluare, terapie și consiliere adulților cu dificultăți de vorbire, limbaj, comunicare și înghițire."
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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

    let service222 = Service { serviceWorkspace = w22
                             , serviceName = "Servicii de prevenire a căderilor"
                             , serviceDescr = Just "Our falls prevention service provides assessment, advice and exercise for older people who are at risk of falling"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                             , serviceName = "Reabilitare comunitară bazată pe pat"
                             , serviceDescr = Just "Echipa vă va oferi sprijin limitat în timp pentru a vă recâștiga independența după o internare la spital și vă va ajuta să planificați o plecare în siguranță acasă atunci când sunteți „apt funcțional”."
                             , servicePrice = 25000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                             , businessFullName = Just "Oasis, Rusu Ştefan Alexandru & Co"
                             , businessDescr = Just "Oasis este o firma de consultanta in ospitalitate care ofera servicii complete de consultanta printr-o echipa diversificata de consultanti"
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
                                , workspaceAddress = "830831, București, Intrarea Popa Nr. 964 Bl. 11 Sc. 27 Ap. 715"
                                , workspaceTzo = utc
                                , workspaceCurrency = "RON"
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
                      , payOptionName = "Plătește acum"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Plătiți folosind un card de credit sau PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w31
                      , payOptionType = PayAtVenue
                      , payOptionName = "Plătiți la locație"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Plătiți după prestarea serviciului"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service311 = Service { serviceWorkspace = w31
                             , serviceName = "Gestionarea activelor"
                             , serviceDescr = Just "Managementul activelor Oasis este realizat de o echipă de experți hotelieri care se concentrează pe optimizarea profiturilor din operațiunile de afaceri hoteliere."
                             , servicePrice = 10000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                             , serviceName = "Consultanță pentru investiții"
                             , serviceDescr = Just "Consultanta de investitii Oasis este specializata in activitati BUY-SIDE si SELL-SIDE pentru activele hoteliere existente si propuse"
                             , servicePrice = 20000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                             , serviceName = "Planificarea executării proiectelor și consultanță"
                             , serviceDescr = Just "Oasis oferă servicii de planificare și consiliere în execuție a proiectelor care oferă îndrumare strategică și conducere pe parcursul etapelor inițiale de dezvoltare a unui proiect hotelier."
                             , servicePrice = 30000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
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
                          , staffMobile = Just "+40 07366 783859"
                          , staffPhone = Just "+40 09691 575048"
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
                          , staffMobile = Just "+40 01788 494865"
                          , staffPhone = Just "+40 02774 546857"
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
                          , staffMobile = Just "+40 7103 26014564"
                          , staffPhone = Just "+40 7103 26014564"
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
                          , staffMobile = Just "+40 4759604365"
                          , staffPhone = Just "+40 4759604365"
                          }

    empl4 <- insert employee4

    liftIO (BS.readFile "demo/2148728638.avif") >>= \bs ->
      insert_ $ StaffPhoto { staffPhotoStaff = empl4
                           , staffPhotoMime = "image/avif"
                           , staffPhotoPhoto = bs
                           , staffPhotoAttribution = Just freepik
                           }

    let employee5 = Staff { staffName = "Dragoș Zaituc"
                          , staffAccount = Nothing
                          , staffMobile = Just "+40 4759604365"
                          , staffPhone = Just "+40 4759604365"
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
                                   , assignmentRole = "Analist"
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
                                   , assignmentRole = "Avocat asociat"
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
                                     , assignmentRole = "Kinetoterapeut"
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
                                     , assignmentRole = "Podolog comunitar"
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
                                     , assignmentRole = "Neuro-Fizioterapeut comunitar"
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
                                     , assignmentRole = "Lucrator Suport Clinic"
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
                                     , assignmentRole = "Terapeut comunitar de vorbire și limbaj"
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
                                     , assignmentRole = "Asistentă de reabilitare medicală"
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
                                     , assignmentRole = "Manager de proiect"
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
                                     , assignmentRole = "Consilier financiar"
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
                                     , assignmentRole = "Analist de capitaluri proprii"
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
