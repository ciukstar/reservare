{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoRu (fillDemoRu) where

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
      ( payOptionWorkspace, payOptionType, payOptionName, payOptionGateway
      , payOptionDescr, payOptionIcon, PayOption
      )
    , PayMethod (PayNow, PayAtVenue)
    , PayGateway (PayGatewayYookassa)
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


fillDemoRu :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoRu appSettings = do

    now <- liftIO getCurrentTime
    let today = utctDay now

    let oneHour = nominalDay / 24
    let halfHour = oneHour / 2
    let quarterHour = halfHour / 2

    pass1 <- liftIO $ saltPass "bulanovalm"
    let user1 = User { userEmail = "bulanovalm@mail.ru"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Буланова Любовь Михайловна"
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

    pass2 <- liftIO $ saltPass "petrovia"
    let user2 = User { userEmail = "petrovia@mail.ru"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Петров Иван Александрович"
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

    pass3 <- liftIO $ saltPass "smirnovav"
    let user3 = User { userEmail = "smirnovav@mail.ru"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Смирнов Андрей Васильевич"
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

    pass4 <- liftIO $ saltPass "sergeevaav"
    let user4 = User { userEmail = "sergeevaav@mail.ru"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Сергеева Александра Владимировна"
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
                             , businessName = "ФинКо"
                             , businessFullName = Just "Финансовый консалтинг, Буланова Любовь Михайловна и Ко"
                             , businessDescr = Just "ФинКо предоставляет финансовые консультации, которые помогут вам начать и развить свой бизнес."
                             }

    b1 <- insert business1

    liftIO (BS.readFile "demo/logo_finco_120x120_ru.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b1
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto = bs
                             , businessLogoAttribution = Nothing
                             }

    let workspace11 = Workspace { workspaceBusiness = b1
                                , workspaceName = "БЛМ и Ко Центральный"
                                , workspaceAddress = "Россия, г. Бердск, Тихая ул., д. 18 кв.209"
                                , workspaceTzo = utc
                                , workspaceCurrency = "RUB"
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
                      , payOptionName = "Оплатить сейчас"
                      , payOptionGateway = Just PayGatewayYookassa
                      , payOptionDescr = Just "Оплатить кредитной картой"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w11
                      , payOptionType = PayAtVenue
                      , payOptionName = "Оплата на месте"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Оплата после оказания услуги"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let sector1 = Sector { sectorName = "Туризм"
                         , sectorDescr = Just "Туристический консалтинг"
                         , sectorParent = Nothing
                         }
    sec1 <- insert sector1

    let sector2 = Sector { sectorName = "Отель"
                         , sectorDescr = Just "Гостиничный и ресторанный бизнес"
                         , sectorParent = Nothing
                         }
    sec2 <- insert sector2

    let sector3 = Sector { sectorName = "Финансовый"
                         , sectorDescr = Just "Финансовые услуги"
                         , sectorParent = Nothing
                         }
    sec3 <- insert sector3

    let sector4 = Sector { sectorName = "Здравоохранение"
                         , sectorDescr = Just "Здравоохранение"
                         , sectorParent = Nothing
                         }
    sec4 <- insert sector4

    let sector5 = Sector { sectorName = "Юридический"
                         , sectorDescr = Just "Юридическая практика"
                         , sectorParent = Nothing
                         }
    sec5 <- insert sector5

    let sector6 = Sector { sectorName = "Управление"
                         , sectorDescr = Just "Управленческий консалтинг"
                         , sectorParent = Nothing
                         }
    sec6 <- insert sector6

    let sector7 = Sector { sectorName = "ИТ"
                         , sectorDescr = Just "Консалтинг в области информационных технологий"
                         , sectorParent = Nothing
                         }
    sec7 <- insert sector7

    let sector8 = Sector { sectorName = "Музыка"
                         , sectorDescr = Just "Музыкальная индустрия"
                         , sectorParent = Nothing
                         }
    sec8 <- insert sector8

    let service111 = Service { serviceWorkspace = w11
                             , serviceName = "Банковский риск и устойчивость"
                             , serviceDescr = Just "Мы помогаем клиентам сферы финансовых услуг достигать исключительных показателей с поправкой на риск"
                             , servicePrice = 1000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s111 <- insert service111

    liftIO (BS.readFile "demo/logo_finco_120x120_ru.svg") >>= \bs ->
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
                             , serviceName = "Корпоративный и инвестиционный банкинг"
                             , serviceDescr = Just "Мы помогаем клиентам инвестиционного банкинга удовлетворять широкий спектр стратегических, организационных и операционных потребностей."
                             , servicePrice = 2000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s112 <- insert service112

    liftIO (BS.readFile "demo/logo_finco_120x120_ru.svg") >>= \bs ->
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
                             , serviceName = "Юридические фирмы и профессиональные услуги"
                             , serviceDescr = Just "Мы консультируем учреждения профессиональных услуг, включая юридические фирмы, бухгалтерские фирмы, корпоративные юридические отделы и другие организации работников умственного труда, по широкому кругу стратегических, организационных и операционных вопросов."
                             , servicePrice = 2500000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s113 <- insert service113

    liftIO (BS.readFile "demo/logo_finco_120x120_ru.svg") >>= \bs ->
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
                             , businessName = "МедКаб"
                             , businessFullName = Just "МедКаб, Петров Иван Александрович и Ко"
                             , businessDescr = Just "Мы предоставляем ориентированную на пациента медицинскую помощь с превосходным качеством, обслуживанием и доступом."
                             }

    b2 <- insert business2

    liftIO (BS.readFile "demo/logo_medcab_120x120_ru.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b2
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto = bs
                             , businessLogoAttribution = Nothing
                             }

    let workspace21 = Workspace { workspaceBusiness = b2
                                , workspaceName = "МедКаб, Офис №1"
                                , workspaceAddress = "Россия, г. Тюмень, Дзержинского ул., д. 20 кв.17"
                                , workspaceTzo = utc
                                , workspaceCurrency = "RUB"
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
                      , payOptionName = "Оплатить сейчас"
                      , payOptionGateway = Just PayGatewayYookassa
                      , payOptionDescr = Just "Оплатить кредитной картой"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w21
                      , payOptionType = PayAtVenue
                      , payOptionName = "Оплата на месте"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Оплата после оказания услуги"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service211 = Service { serviceWorkspace = w21
                             , serviceName = "Общественная физиотерапия"
                             , serviceDescr = Just "Целью общественной физиотерапии является оптимизация физических функций, повышение независимости и качества жизни людей, имеющих на это право в рамках сообщества."
                             , servicePrice = 1000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec4
                             }

    s211 <- insert service211

    liftIO (BS.readFile "demo/logo_medcab_120x120_ru.svg") >>= \bs ->
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
                             , serviceName = "Терапия опорно-двигательного аппарата"
                             , serviceDescr = Just "Физиотерапия опорно-двигательного аппарата будет направлена на биомеханическую и структурную реабилитацию клиента."
                             , servicePrice = 2000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec4
                             }

    s212 <- insert service212

    liftIO (BS.readFile "demo/logo_medcab_120x120_ru.svg") >>= \bs ->
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
                             , serviceName = "Общественная подология"
                             , serviceDescr = Just "Обеспечивает оценку, диагностику и лечение пациентов с проблемами нижних конечностей и стоп, в том числе"
                             , servicePrice = 2500000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec4
                             }

    s213 <- insert service213

    liftIO (BS.readFile "demo/logo_medcab_120x120_ru.svg") >>= \bs ->
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
                                , workspaceName = "МедКаб, Офис №2"
                                , workspaceAddress = "Россия, г. Люберцы, Радужная ул., д. 6 кв.59"
                                , workspaceTzo = utc
                                , workspaceCurrency = "RUB"
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
                      , payOptionName = "Оплатить сейчас"
                      , payOptionGateway = Just PayGatewayYookassa
                      , payOptionDescr = Just "Оплатить кредитной картой"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w22
                      , payOptionType = PayAtVenue
                      , payOptionName = "Оплата на месте"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Оплата после оказания услуги"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service221 = Service { serviceWorkspace = w22
                             , serviceName = "Общественная логопедия и языковая терапия"
                             , serviceDescr = Just "Команда по общению в области речи и языка проводит оценку, терапию и консультации для взрослых с трудностями речи, языка, общения и глотания."
                             , servicePrice = 1000000
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
                             , serviceName = "Услуги по профилактике падений"
                             , serviceDescr = Just "Наша служба профилактики падений предоставляет оценку, рекомендации и упражнения для пожилых людей, которые подвержены риску падения."
                             , servicePrice = 2000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec4
                             }

    s222 <- insert service222

    liftIO (BS.readFile "demo/logo_medcab_120x120_ru.svg") >>= \bs ->
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
                             , serviceName = "Реабилитация в стационаре"
                             , serviceDescr = Just "Команда окажет вам ограниченную по времени поддержку, чтобы вы смогли восстановить свою независимость после госпитализации, и поможет вам спланировать безопасный отъезд домой, когда вы будете «функционально здоровы»."
                             , servicePrice = 2500000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec4
                             }

    s223 <- insert service223

    liftIO (BS.readFile "demo/logo_medcab_120x120_ru.svg") >>= \bs ->
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
                             , businessName = "Оазис"
                             , businessFullName = Just "ОАЗИС, Смирнов Андрей Васильевич и Ко"
                             , businessDescr = Just "Оазис — консалтинговая фирма в сфере гостеприимства, предлагающая комплексные консалтинговые услуги через разнообразную команду консультантов."
                             }

    b3 <- insert business3

    liftIO (BS.readFile "demo/logo_oasis_120x120_ru.svg") >>= \bs ->
      insert_ $ BusinessLogo { businessLogoBusiness = b3
                             , businessLogoMime = "image/svg+xml"
                             , businessLogoPhoto =  bs
                             , businessLogoAttribution = Nothing
                             }

    let workspace31 = Workspace { workspaceBusiness = b3
                                , workspaceName = "Оазис Центральный"
                                , workspaceAddress = "Россия, г. Щёлково, Набережная ул., д. 19 кв.12"
                                , workspaceTzo = utc
                                , workspaceCurrency = "RUB"
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
                      , payOptionName = "Оплатить сейчас"
                      , payOptionGateway = Just PayGatewayYookassa
                      , payOptionDescr = Just "Оплатить кредитной картой"
                      , payOptionIcon = Just "credit_card"
                      }

    insert_ PayOption { payOptionWorkspace = w31
                      , payOptionType = PayAtVenue
                      , payOptionName = "Оплата на месте"
                      , payOptionGateway = Nothing
                      , payOptionDescr = Just "Оплата после оказания услуги"
                      , payOptionIcon = Just "point_of_sale"
                      }

    let service311 = Service { serviceWorkspace = w31
                             , serviceName = "Управление активами"
                             , serviceDescr = Just "Управление активами Oasis осуществляется командой гостиничных экспертов, которые уделяют особое внимание оптимизации доходов от операций гостиничного бизнеса."
                             , servicePrice = 1000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec2
                             }

    s311 <- insert service311

    liftIO (BS.readFile "demo/logo_oasis_120x120_ru.svg") >>= \bs ->
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
                             , serviceName = "Инвестиционный консалтинг"
                             , serviceDescr = Just "Инвестиционное консультирование Oasis специализируется на покупках и продажах существующих и предлагаемых гостиничных активов."
                             , servicePrice = 2000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec2
                             }

    s312 <- insert service312

    liftIO (BS.readFile "demo/logo_oasis_120x120_ru.svg") >>= \bs ->
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
                             , serviceName = "Планирование и консультирование по реализации проекта"
                             , serviceDescr = Just "Oasis предлагает услуги по планированию и консультированию по реализации проекта, которые обеспечивают стратегическое руководство и лидерство на начальных этапах разработки гостиничного проекта."
                             , servicePrice = 3000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec2
                             }

    s313 <- insert service313

    liftIO (BS.readFile "demo/logo_oasis_120x120_ru.svg") >>= \bs ->
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
                          , staffMobile = Just "+7 (075) 808-01-84"
                          , staffPhone = Just "+7 (805) 623-96-71"
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
                          , staffMobile = Just "+7 (276) 529-24-34"
                          , staffPhone = Just "+7 (664) 606-85-71"
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
                          , staffMobile = Just "+7 (704) 920-06-70"
                          , staffPhone = Just "+7 (535) 343-22-29"
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
                          , staffMobile = Just "+7 (565) 242-29-46"
                          , staffPhone = Just "+7 (766) 139-19-88"
                          }

    empl4 <- insert employee4

    liftIO (BS.readFile "demo/2148728638.avif") >>= \bs ->
      insert_ $ StaffPhoto { staffPhotoStaff = empl4
                           , staffPhotoMime = "image/avif"
                           , staffPhotoPhoto = bs
                           , staffPhotoAttribution = Just freepik
                           }

    let employee5 = Staff { staffName = "Плотников Богдан Кириллович"
                          , staffAccount = Nothing
                          , staffMobile = Just "+7 (565) 242-29-45"
                          , staffPhone = Just "+7 (766) 139-19-89"
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
                                   , assignmentRole = "Старший менеджер по управлению рисками"
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
                                   , assignmentRole = "Аналитик"
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
                                   , assignmentRole = "Помощник адвоката"
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
                                     , assignmentRole = "Физиотерапевт"
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
                                     , assignmentRole = "Общественный подолог"
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
                                     , assignmentRole = "Общественный нейрофизиотерапевт"
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
                                     , assignmentRole = "Сотрудник клинической поддержки"
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
                                     , assignmentRole = "Общественный логопед и языковой терапевт"
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
                                     , assignmentRole = "Помощник медсестры по реабилитации"
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
                                     , assignmentRole = "Руководитель проекта"
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
                                     , assignmentRole = "Финансовый консультант"
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
                                     , assignmentRole = "Аналитик по акциям"
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
