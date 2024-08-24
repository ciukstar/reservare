{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoFr (fillDemoFr) where

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
    , ServicePhoto
      ( ServicePhoto, servicePhotoService, servicePhotoMime, servicePhotoPhoto
      , servicePhotoAttribution
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

    liftIO (BS.readFile "demo/222.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = usr2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
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

    liftIO (BS.readFile "demo/2148213406.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = usr3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
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

    liftIO (BS.readFile "demo/2148728638.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = usr4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik

                        }

    let business1 = Business { businessOwner = usr1
                             , businessName = "FinCo"
                             , businessFullName = Just "Conseil financier, Bernard Jade & Cie"
                             , businessDescr = Just "Finco fournit des conseils financiers pour vous aider à démarrer et à développer votre entreprise."
                             }

    b1 <- insert business1

    liftIO (BS.readFile "demo/logo_finco_120x120.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b1
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto = bs
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
                             , serviceName = "Banque d'investissement et de financement"
                             , serviceDescr = Just "Nous aidons les clients de la banque d'investissement à répondre à un large éventail d'exigences stratégiques, organisationnelles et opérationnelles"
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
                             , serviceName = "Cabinets d'avocats et services professionnels"
                             , serviceDescr = Just "We advise professional services institutions, including law firms, accounting firms, corporate legal departments, and other knowledge worker organizations, on a broad range of strategic, organizational, and operational issues"
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
                             , businessFullName = Just "MedCab, Thomas Gabriel Raphaël & Cie"
                             , businessDescr = Just "Nous offrons des soins de santé centrés sur le patient avec une excellence en matière de qualité, de service et d'accès"
                             }

    b2 <- insert business2

    liftIO (BS.readFile "demo/logo_medcab_120x120.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b2
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto = bs
                             , businessLogoAttribution = Nothing
                             }

    let workspace21 = Workspace { workspaceBusiness = b2
                                , workspaceName = "MedCab, Bureau #1"
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
                             , serviceName = "Physiothérapie communautaire"
                             , serviceDescr = Just "La physiothérapie communautaire vise à optimiser la fonction physique et à améliorer l'indépendance et la qualité de vie des personnes éligibles au sein de la communauté."
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
                             , serviceName = "Thérapie musculo-squelettique"
                             , serviceDescr = Just "La physiothérapie musculo-squelettique se concentrera sur la rééducation biomécanique et structurelle du client"
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
                             , serviceName = "Podologie communautaire"
                             , serviceDescr = Just "Fournit une évaluation, un diagnostic et un traitement à une gamme de patients souffrant de problèmes des membres inférieurs et des pieds, notamment"
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
                                , workspaceName = "MedCab, Bureau #2"
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
                             , serviceName = "Orthophonie communautaire"
                             , serviceDescr = Just "L'équipe communautaire d'orthophonie fournit une évaluation, une thérapie et des conseils aux adultes ayant des difficultés d'élocution, de langage, de communication et de déglutition."
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
                             , serviceName = "Services de prévention des chutes"
                             , serviceDescr = Just "Notre service de prévention des chutes propose une évaluation, des conseils et des exercices pour les personnes âgées qui risquent de tomber."
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
                             , serviceName = "Réadaptation communautaire au lit"
                             , serviceDescr = Just "L'équipe vous offrira un soutien limité dans le temps pour vous permettre de retrouver votre indépendance après une hospitalisation et vous aidera à planifier un retour à la maison en toute sécurité lorsque vous serez « fonctionnellement apte »."
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
                             , businessFullName = Just "Oasis, Richard Arthur Louis & Cie"
                             , businessDescr = Just "Oasis est un cabinet de conseil en hôtellerie offrant des services de conseil complets grâce à une équipe diversifiée de consultants"
                             }

    b3 <- insert business3

    liftIO (BS.readFile "demo/logo_oasis_120x120.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b3
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto =  bs
                             , businessLogoAttribution = Nothing
                             }

    let workspace31 = Workspace { workspaceBusiness = b3
                                , workspaceName = "Oasis Centrale"
                                , workspaceAddress = "Apt. 720, 1372 Quai Seine Bras de Gennevillier, 72388 Issy-les-Moulineaux"
                                , workspaceTzo = utc
                                , workspaceCurrency = "EUR"
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
                      , payOptionName = "Payez maintenant"
                      , payOptionGateway = Just PayGatewayStripe
                      , payOptionDescr = Just "Payer avec une carte de crédit ou PayPal"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w31
                      , payOptionType = PayAtVenue
                      , payOptionName = "Payer sur place"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Payer après la prestation du service"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service311 = Service { serviceWorkspace = w31
                             , serviceName = "Gestion d'actifs"
                             , serviceDescr = Just "La gestion d'actifs d'Oasis est assurée par une équipe d'experts hôteliers qui se concentre sur l'optimisation des rendements de l'exploitation des activités hôtelières"
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
                             , serviceName = "Conseil en investissement"
                             , serviceDescr = Just "Le conseil en investissement d'Oasis est spécialisé dans les activités BUY-SIDE et SELL-SIDE pour les actifs hôteliers existants et proposés"
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
                             , serviceName = "Planification et conseil en exécution de projet"
                             , serviceDescr = Just "Oasis propose des services de planification et de conseil en exécution de projet qui fournissent des conseils stratégiques et un leadership tout au long des étapes initiales de développement d'un projet hôtelier."
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
                          , staffMobile = Just "+33 3122502799"
                          , staffPhone = Just "+33 4480722669"
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
                          , staffMobile = Just "+33 1323257129"
                          , staffPhone = Just "+33 2112838506"
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
                          , staffMobile = Just "+33 2546544622"
                          , staffPhone = Just "+33 5617507392"
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
                          , staffMobile = Just "+33 4179354938"
                          , staffPhone = Just "+33 4720367174"
                          }

    empl4 <- insert employee4

    liftIO (BS.readFile "demo/2148728638.avif") >>= \bs ->
      insert_ $ StaffPhoto { staffPhotoStaff = empl4
                           , staffPhotoMime = "image/avif"
                           , staffPhotoPhoto = bs
                           , staffPhotoAttribution = Just freepik
                           }

    let employee5 = Staff { staffName = "Pierre-Louis Boffrand"
                          , staffAccount = Nothing
                          , staffMobile = Just "+33 4759604365"
                          , staffPhone = Just "+33 4759604365"
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
                                   , assignmentRole = "Responsable senior des risques"
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
                                   , assignmentRole = "Analyste"
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
                                   , assignmentRole = "Avocat associé"
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
                                     , assignmentRole = "Physiothérapeute"
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
                                     , assignmentRole = "Podologue communautaire"
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
                                     , assignmentRole = "Neuro-physiothérapeute communautaire"
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
                                     , assignmentRole = "Travailleur de soutien clinique"
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
                                     , assignmentRole = "Orthophoniste communautaire"
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
                                     , assignmentRole = "Infirmière auxiliaire en réadaptation"
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
                                     , assignmentRole = "Chef de projet"
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
                                     , assignmentRole = "Conseiller financier"
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
                                     , assignmentRole = "Analyste actions"
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
