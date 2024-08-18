{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoRu (fillDemoRu) where

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

    insert_ UserPhoto { userPhotoUser = usr1
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/2148728586.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
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

    insert_ UserPhoto { userPhotoUser = usr2
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/222.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
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

    insert_ UserPhoto { userPhotoUser = usr3
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/2148213406.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
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
                             , businessName = "ФинКо"
                             , businessFullName = Just "Финансовый консалтинг, Буланова Любовь Михайловна и Ко"
                             , businessDescr = Just "ФинКо предоставляет финансовые консультации, которые помогут вам начать и развить свой бизнес."
                             }

    b1 <- insert business1

    insert_ $ BusinessLogo { businessLogoBusiness = b1
                           , businessLogoMime = "image/svg+xml"
                           , businessLogoPhoto = $(embedFile "demo/logo_finco_120x120_ru.svg")
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

    let service112 = Service { serviceWorkspace = w11
                             , serviceName = "Корпоративный и инвестиционный банкинг"
                             , serviceDescr = Just "Мы помогаем клиентам инвестиционного банкинга удовлетворять широкий спектр стратегических, организационных и операционных потребностей."
                             , servicePrice = 2000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s112 <- insert service112

    let service113 = Service { serviceWorkspace = w11
                             , serviceName = "Юридические фирмы и профессиональные услуги"
                             , serviceDescr = Just "Мы консультируем учреждения профессиональных услуг, включая юридические фирмы, бухгалтерские фирмы, корпоративные юридические отделы и другие организации работников умственного труда, по широкому кругу стратегических, организационных и операционных вопросов."
                             , servicePrice = 2500000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec3
                             }

    s113 <- insert service113

    let business2 = Business { businessOwner = usr2
                             , businessName = "ПИА и Ко"
                             , businessFullName = Just "Петров Иван Александрович и Ко"
                             , businessDescr = Just "Петров Иван Александрович и Компания"
                             }

    b21 <- insert business2

    let workspace21 = Workspace { workspaceBusiness = b21
                                , workspaceName = "ПИА и Ко Офис №1"
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
                             , serviceName = "Путешествие на отдых"
                             , serviceDescr = Just "Путешествуйте так, как вам нравится"
                             , servicePrice = 1000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s211 <- insert service211

    let service212 = Service { serviceWorkspace = w21
                             , serviceName = "Путешествие по Италии"
                             , serviceDescr = Just "Итальянская радость"
                             , servicePrice = 2000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s212 <- insert service212

    let service213 = Service { serviceWorkspace = w21
                             , serviceName = "Привет, Париж."
                             , serviceDescr = Just "Франция ждет"
                             , servicePrice = 2500000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s213 <- insert service213

    let workspace22 = Workspace { workspaceBusiness = b21
                                , workspaceName = "ПИА и Ко Офис №2"
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
                             , serviceName = "Путешествие на отдых"
                             , serviceDescr = Just "Путешествуйте так, как вам нравится"
                             , servicePrice = 1000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s221 <- insert service221

    let service222 = Service { serviceWorkspace = w22
                             , serviceName = "Путешествие по Италии"
                             , serviceDescr = Just "Итальянская радость"
                             , servicePrice = 2000000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s222 <- insert service222

    let service223 = Service { serviceWorkspace = w22
                             , serviceName = "Привет, Париж."
                             , serviceDescr = Just "Франция ждет"
                             , servicePrice = 2500000
                             , serviceAvailable = True
                             , serviceDuration = oneHour
                             , serviceType = Just sec1
                             }

    s223 <- insert service223

    let business3 = Business { businessOwner = usr3
                             , businessName = "САВ и Ко"
                             , businessFullName = Just "Смирнов Андрей Васильевич и Ко"
                             , businessDescr = Just "Смирнов Андрей Васильевич и Компания"
                             }

    b31 <- insert business3

    let employee1 = Staff { staffName = fromMaybe (userEmail user1) (userName user1)
                          , staffAccount = Just usr1
                          , staffMobile = Just "+7 (075) 808-01-84"
                          , staffPhone = Just "+7 (805) 623-96-71"
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
                          , staffMobile = Just "+7 (276) 529-24-34"
                          , staffPhone = Just "+7 (664) 606-85-71"
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
                          , staffMobile = Just "+7 (704) 920-06-70"
                          , staffPhone = Just "+7 (535) 343-22-29"
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
                          , staffMobile = Just "+7 (565) 242-29-46"
                          , staffPhone = Just "+7 (766) 139-19-88"
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
                                   , assignmentRole = "Туристический агент"
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
                                   , assignmentRole = "Туристический агент"
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
                                    , assignmentRole = "Туристический агент"
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

