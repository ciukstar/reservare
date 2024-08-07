{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Yookassa.Data where

import Data.Text (Text)

import Model (BookId, PayOptionId, PaymentId)

import Yesod.Core
    ( RenderRoute(renderRoute, Route), Yesod, HandlerFor
    , RenderMessage
    )
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)
import Yesod.Form (FormMessage)


data Yookassa = Yookassa


mkYesodSubData "Yookassa" [parseRoutes|
/completion/#BookId/#PaymentId/#Text CompletionR GET
/checkout/#BookId/#PayOptionId       CheckoutR   GET
|]


class (Yesod m, RenderMessage m FormMessage, RenderMessage m YookassaMessage) => YesodYookassa m where
    getHomeR :: HandlerFor m (Route m)
    getYookassaConfShopId :: HandlerFor m Text
    getYookassaConfSecret :: HandlerFor m Text
    getBookDetailsR :: BookId -> HandlerFor m (Route m)


data YookassaMessage = MsgBack
                   | MsgCheckout
                   | MsgPaymentAmount
                   | MsgPay
                   | MsgCancel
                   | MsgYooKassa
                   | MsgUnhandledError
                   | MsgInvalidPaymentAmount
                   | MsgYourBookingHasBeenCreatedSuccessfully
                   | MsgPaymentStatus
                   | MsgSomethingWentWrong
                   | MsgViewBookingDetails
                   | MsgReturnToHomePage
                   | MsgFinish
                   | MsgInvalidArguments

englishYookassaMessage :: YookassaMessage -> Text
englishYookassaMessage MsgBack = "Back"
englishYookassaMessage MsgCheckout = "Checkout"
englishYookassaMessage MsgPaymentAmount = "Payment amount"
englishYookassaMessage MsgPay = "Pay"
englishYookassaMessage MsgCancel = "Cancel"
englishYookassaMessage MsgYooKassa = "YooKassa"
englishYookassaMessage MsgUnhandledError = "Unhandled error"
englishYookassaMessage MsgInvalidPaymentAmount = "Invalid payment amount"
englishYookassaMessage MsgYourBookingHasBeenCreatedSuccessfully = "Your booking has been created successfully"
englishYookassaMessage MsgPaymentStatus = "Payment status"
englishYookassaMessage MsgSomethingWentWrong = "Something went wrong"
englishYookassaMessage MsgViewBookingDetails = "View booking details"
englishYookassaMessage MsgReturnToHomePage = "Return to Home Page"
englishYookassaMessage MsgFinish = "Finish"
englishYookassaMessage MsgInvalidArguments = "Invalid arguments"



frenchYookassaMessage :: YookassaMessage -> Text
frenchYookassaMessage MsgBack = "Retour"
frenchYookassaMessage MsgCheckout = "Vérifier"
frenchYookassaMessage MsgPaymentAmount = "Montant du paiement"
frenchYookassaMessage MsgPay = "Payez"
frenchYookassaMessage MsgCancel = "Annuler"
frenchYookassaMessage MsgYooKassa = "YooKassa"
frenchYookassaMessage MsgUnhandledError = "Erreur non gérée"
frenchYookassaMessage MsgInvalidPaymentAmount = "Montant du paiement incorrect"
frenchYookassaMessage MsgYourBookingHasBeenCreatedSuccessfully = "Votre réservation a été créée avec succès"
frenchYookassaMessage MsgPaymentStatus = "Statut de paiement"
frenchYookassaMessage MsgSomethingWentWrong = "Quelque chose n'a pas fonctionné"
frenchYookassaMessage MsgViewBookingDetails = "Afficher les détails de la réservation"
frenchYookassaMessage MsgReturnToHomePage = "Retour à la page d'accueil"
frenchYookassaMessage MsgFinish = "Finir"
frenchYookassaMessage MsgInvalidArguments = "Arguments invalides"


romanianYookassaMessage :: YookassaMessage -> Text
romanianYookassaMessage MsgBack = "Înapoi"
romanianYookassaMessage MsgCheckout = "Verifică"
romanianYookassaMessage MsgPaymentAmount = "Suma de plată"
romanianYookassaMessage MsgPay = "Plătiți"
romanianYookassaMessage MsgCancel = "Renunță"
romanianYookassaMessage MsgYooKassa = "YooKassa"
romanianYookassaMessage MsgUnhandledError = "Eroare nerezolvată"
romanianYookassaMessage MsgInvalidPaymentAmount = "Sumă de plată nevalidă"
romanianYookassaMessage MsgYourBookingHasBeenCreatedSuccessfully = "Rezervarea dvs. a fost creată cu succes"
romanianYookassaMessage MsgPaymentStatus = "Starea plății"
romanianYookassaMessage MsgSomethingWentWrong = "Ceva n-a mers bine"
romanianYookassaMessage MsgViewBookingDetails = "Vizualizați detaliile rezervării"
romanianYookassaMessage MsgReturnToHomePage = "Înapoi la pagina de start"
romanianYookassaMessage MsgFinish = "Finalizare"
romanianYookassaMessage MsgInvalidArguments = "Argumente nevalide"


russianYookassaMessage :: YookassaMessage -> Text
russianYookassaMessage MsgBack = "Вернуться"
russianYookassaMessage MsgCheckout = "Оформить заказ"
russianYookassaMessage MsgPaymentAmount = "Сумма платежа"
russianYookassaMessage MsgPay = "Заплатить"
russianYookassaMessage MsgCancel = "Отмена"
russianYookassaMessage MsgYooKassa = "ЮKassa"
russianYookassaMessage MsgUnhandledError = "Необработанная ошибка"
russianYookassaMessage MsgInvalidPaymentAmount = "Неверная сумма платежа"
russianYookassaMessage MsgYourBookingHasBeenCreatedSuccessfully = "Ваше бронирование успешно создано"
russianYookassaMessage MsgPaymentStatus = "Статус платежа"
russianYookassaMessage MsgSomethingWentWrong = "Что-то пошло не так"
russianYookassaMessage MsgViewBookingDetails = "Посмотреть детали бронирования"
russianYookassaMessage MsgReturnToHomePage = "Вернуться на главную страницу"
russianYookassaMessage MsgFinish = "Завершить"
russianYookassaMessage MsgInvalidArguments = "Неверные аргументы"


defaultYookassaMessage :: YookassaMessage -> Text
defaultYookassaMessage = englishYookassaMessage
