{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module AtVenue.Data where

import Data.Text (Text)

import Model (BookId, PayOptionId)

import Yesod.Core
    ( RenderRoute(renderRoute, Route), Yesod, HandlerFor
    , RenderMessage
    )
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)
import Yesod.Form (FormMessage)

data AtVenue = AtVenue

mkYesodSubData "AtVenue" [parseRoutes|
/checkout/#BookId/#PayOptionId CheckoutR GET
|]


class (Yesod m, RenderMessage m FormMessage, RenderMessage m AtVenueMessage) => YesodAtVenue m where
    getHomeR :: HandlerFor m (Route m)
    getBookDetailsR :: BookId -> HandlerFor m (Route m)


data AtVenueMessage = MsgBack
                   | MsgCheckout
                   | MsgPaymentAmount
                   | MsgPay
                   | MsgCancel
                   | MsgStripe
                   | MsgUnhandledError
                   | MsgInvalidPaymentAmount
                   | MsgYourBookingHasBeenCreatedSuccessfully
                   | MsgPaymentStatus
                   | MsgSomethingWentWrong
                   | MsgViewBookingDetails
                   | MsgReturnToHomePage
                   | MsgFinish
                   | MsgPaymentIntentCancelled

englishAtVenueMessage :: AtVenueMessage -> Text
englishAtVenueMessage MsgBack = "Back"
englishAtVenueMessage MsgCheckout = "Checkout"
englishAtVenueMessage MsgPaymentAmount = "Payment amount"
englishAtVenueMessage MsgPay = "Pay"
englishAtVenueMessage MsgCancel = "Cancel"
englishAtVenueMessage MsgStripe = "Stripe"
englishAtVenueMessage MsgUnhandledError = "Unhandled error"
englishAtVenueMessage MsgInvalidPaymentAmount = "Invalid payment amount"
englishAtVenueMessage MsgYourBookingHasBeenCreatedSuccessfully = "Your booking has been created successfully"
englishAtVenueMessage MsgPaymentStatus = "Payment status"
englishAtVenueMessage MsgSomethingWentWrong = "Something went wrong"
englishAtVenueMessage MsgViewBookingDetails = "View booking details"
englishAtVenueMessage MsgReturnToHomePage = "Return to Home Page"
englishAtVenueMessage MsgFinish = "Finish"
englishAtVenueMessage MsgPaymentIntentCancelled = "Payment intent was cancelled"


frenchAtVenueMessage :: AtVenueMessage -> Text
frenchAtVenueMessage MsgBack = "Retour"
frenchAtVenueMessage MsgCheckout = "Vérifier"
frenchAtVenueMessage MsgPaymentAmount = "Montant du paiement"
frenchAtVenueMessage MsgPay = "Payez"
frenchAtVenueMessage MsgCancel = "Annuler"
frenchAtVenueMessage MsgStripe = "Stripe"
frenchAtVenueMessage MsgUnhandledError = "Erreur non gérée"
frenchAtVenueMessage MsgInvalidPaymentAmount = "Montant du paiement incorrect"
frenchAtVenueMessage MsgYourBookingHasBeenCreatedSuccessfully = "Votre réservation a été créée avec succès"
frenchAtVenueMessage MsgPaymentStatus = "Statut de paiement"
frenchAtVenueMessage MsgSomethingWentWrong = "Quelque chose n'a pas fonctionné"
frenchAtVenueMessage MsgViewBookingDetails = "Afficher les détails de la réservation"
frenchAtVenueMessage MsgReturnToHomePage = "Retour à la page d'accueil"
frenchAtVenueMessage MsgFinish = "Finir"
frenchAtVenueMessage MsgPaymentIntentCancelled = "L'intention de paiement a été annulée"

romanianAtVenueMessage :: AtVenueMessage -> Text
romanianAtVenueMessage MsgBack = "Înapoi"
romanianAtVenueMessage MsgCheckout = "Verifică"
romanianAtVenueMessage MsgPaymentAmount = "Suma de plată"
romanianAtVenueMessage MsgPay = "Plătiți"
romanianAtVenueMessage MsgCancel = "Renunță"
romanianAtVenueMessage MsgStripe = "Stripe"
romanianAtVenueMessage MsgUnhandledError = "Eroare nerezolvată"
romanianAtVenueMessage MsgInvalidPaymentAmount = "Sumă de plată nevalidă"
romanianAtVenueMessage MsgYourBookingHasBeenCreatedSuccessfully = "Rezervarea dvs. a fost creată cu succes"
romanianAtVenueMessage MsgPaymentStatus = "Starea plății"
romanianAtVenueMessage MsgSomethingWentWrong = "Ceva n-a mers bine"
romanianAtVenueMessage MsgViewBookingDetails = "Vizualizați detaliile rezervării"
romanianAtVenueMessage MsgReturnToHomePage = "Înapoi la pagina de start"
romanianAtVenueMessage MsgFinish = "Finalizare"
romanianAtVenueMessage MsgPaymentIntentCancelled = "Intenția de plată a fost anulată"

russianAtVenueMessage :: AtVenueMessage -> Text
russianAtVenueMessage MsgBack = "Вернуться"
russianAtVenueMessage MsgCheckout = "Оформить заказ"
russianAtVenueMessage MsgPaymentAmount = "Сумма платежа"
russianAtVenueMessage MsgPay = "Заплатить"
russianAtVenueMessage MsgCancel = "Отмена"
russianAtVenueMessage MsgStripe = "Stripe"
russianAtVenueMessage MsgUnhandledError = "Необработанная ошибка"
russianAtVenueMessage MsgInvalidPaymentAmount = "Неверная сумма платежа"
russianAtVenueMessage MsgYourBookingHasBeenCreatedSuccessfully = "Ваше бронирование успешно создано"
russianAtVenueMessage MsgPaymentStatus = "Статус платежа"
russianAtVenueMessage MsgSomethingWentWrong = "Что-то пошло не так"
russianAtVenueMessage MsgViewBookingDetails = "Посмотреть детали бронирования"
russianAtVenueMessage MsgReturnToHomePage = "Вернуться на главную страницу"
russianAtVenueMessage MsgFinish = "Завершить"
russianAtVenueMessage MsgPaymentIntentCancelled = "Намерение платежа было отменено"

defaultAtVenueMessage :: AtVenueMessage -> Text
defaultAtVenueMessage = englishAtVenueMessage
