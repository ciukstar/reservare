{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Stripe.Data where

import Data.Text (Text)

import Model (BookId, PayOptionId)

import Yesod.Core
    ( RenderRoute(renderRoute, Route), Yesod, HandlerFor
    , RenderMessage
    )
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)
import Yesod.Form (FormMessage)
    

data Stripe = Stripe

mkYesodSubData "Stripe" [parseRoutes|
/cancel                          CancelR     POST
/completion/#BookId/#PayOptionId CompletionR GET
/intent/#Int/#Text               IntentR     POST
/checkout/#BookId/#PayOptionId   CheckoutR   GET
|]


class (Yesod m, RenderMessage m FormMessage, RenderMessage m StripeMessage) => YesodStripe m where
    getHomeR :: HandlerFor m (Route m)
    getUserEmail :: HandlerFor m (Maybe Text)
    getStripeConfPk :: HandlerFor m Text
    getStripeConfSk :: HandlerFor m Text
    getBookDetailsR :: BookId -> HandlerFor m (Route m)


data StripeMessage = MsgBack
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
                   | MsgClose
                   | MsgAnUnexpectedErrorOccurred
                   | MsgPaymentSucceeded
                   | MsgYourPaymentIsProcessing
                   | MsgYourPaymentWasNotSuccessful


englishStripeMessage :: StripeMessage -> Text
englishStripeMessage MsgBack = "Back"
englishStripeMessage MsgCheckout = "Checkout"
englishStripeMessage MsgPaymentAmount = "Payment amount"
englishStripeMessage MsgPay = "Pay"
englishStripeMessage MsgCancel = "Cancel"
englishStripeMessage MsgStripe = "Stripe"
englishStripeMessage MsgUnhandledError = "Unhandled error"
englishStripeMessage MsgInvalidPaymentAmount = "Invalid payment amount"
englishStripeMessage MsgYourBookingHasBeenCreatedSuccessfully = "Your booking has been created successfully"
englishStripeMessage MsgPaymentStatus = "Payment status"
englishStripeMessage MsgSomethingWentWrong = "Something went wrong"
englishStripeMessage MsgViewBookingDetails = "View booking details"
englishStripeMessage MsgReturnToHomePage = "Return to Home Page"
englishStripeMessage MsgFinish = "Finish"
englishStripeMessage MsgPaymentIntentCancelled = "Payment intent was cancelled"
englishStripeMessage MsgClose = "Close"
englishStripeMessage MsgAnUnexpectedErrorOccurred = "An unexpected error occurred"
englishStripeMessage MsgPaymentSucceeded = "Payment succeeded"
englishStripeMessage MsgYourPaymentIsProcessing = "Your payment is processing"
englishStripeMessage MsgYourPaymentWasNotSuccessful = "Your payment was not successful, please try again"


frenchStripeMessage :: StripeMessage -> Text
frenchStripeMessage MsgBack = "Retour"
frenchStripeMessage MsgCheckout = "Vérifier"
frenchStripeMessage MsgPaymentAmount = "Montant du paiement"
frenchStripeMessage MsgPay = "Payez"
frenchStripeMessage MsgCancel = "Annuler"
frenchStripeMessage MsgStripe = "Stripe"
frenchStripeMessage MsgUnhandledError = "Erreur non gérée"
frenchStripeMessage MsgInvalidPaymentAmount = "Montant du paiement incorrect"
frenchStripeMessage MsgYourBookingHasBeenCreatedSuccessfully = "Votre réservation a été créée avec succès"
frenchStripeMessage MsgPaymentStatus = "Statut de paiement"
frenchStripeMessage MsgSomethingWentWrong = "Quelque chose n'a pas fonctionné"
frenchStripeMessage MsgViewBookingDetails = "Afficher les détails de la réservation"
frenchStripeMessage MsgReturnToHomePage = "Retour à la page d'accueil"
frenchStripeMessage MsgFinish = "Finir"
frenchStripeMessage MsgPaymentIntentCancelled = "L'intention de paiement a été annulée"
frenchStripeMessage MsgClose = "Fermer"
frenchStripeMessage MsgAnUnexpectedErrorOccurred = "Une erreur inattendue s'est produite"
frenchStripeMessage MsgPaymentSucceeded = "Paiement réussi"
frenchStripeMessage MsgYourPaymentIsProcessing = "Votre paiement est en cours de traitement"
frenchStripeMessage MsgYourPaymentWasNotSuccessful = "Votre paiement n'a pas réussi, veuillez réessayer"


romanianStripeMessage :: StripeMessage -> Text
romanianStripeMessage MsgBack = "Înapoi"
romanianStripeMessage MsgCheckout = "Verifică"
romanianStripeMessage MsgPaymentAmount = "Suma de plată"
romanianStripeMessage MsgPay = "Plătiți"
romanianStripeMessage MsgCancel = "Renunță"
romanianStripeMessage MsgStripe = "Stripe"
romanianStripeMessage MsgUnhandledError = "Eroare nerezolvată"
romanianStripeMessage MsgInvalidPaymentAmount = "Sumă de plată nevalidă"
romanianStripeMessage MsgYourBookingHasBeenCreatedSuccessfully = "Rezervarea dvs. a fost creată cu succes"
romanianStripeMessage MsgPaymentStatus = "Starea plății"
romanianStripeMessage MsgSomethingWentWrong = "Ceva n-a mers bine"
romanianStripeMessage MsgViewBookingDetails = "Vizualizați detaliile rezervării"
romanianStripeMessage MsgReturnToHomePage = "Înapoi la pagina de start"
romanianStripeMessage MsgFinish = "Finalizare"
romanianStripeMessage MsgPaymentIntentCancelled = "Intenția de plată a fost anulată"
romanianStripeMessage MsgClose = "Închide"
romanianStripeMessage MsgAnUnexpectedErrorOccurred = "A apărut o eroare neașteptată"
romanianStripeMessage MsgPaymentSucceeded = "Plata a reușit"
romanianStripeMessage MsgYourPaymentIsProcessing = "Plata dvs. este în curs de procesare"
romanianStripeMessage MsgYourPaymentWasNotSuccessful = "Plata dumneavoastră nu a reușit, vă rugăm să încercați din nou"


russianStripeMessage :: StripeMessage -> Text
russianStripeMessage MsgBack = "Вернуться"
russianStripeMessage MsgCheckout = "Оформить заказ"
russianStripeMessage MsgPaymentAmount = "Сумма платежа"
russianStripeMessage MsgPay = "Заплатить"
russianStripeMessage MsgCancel = "Отмена"
russianStripeMessage MsgStripe = "Stripe"
russianStripeMessage MsgUnhandledError = "Необработанная ошибка"
russianStripeMessage MsgInvalidPaymentAmount = "Неверная сумма платежа"
russianStripeMessage MsgYourBookingHasBeenCreatedSuccessfully = "Ваше бронирование успешно создано"
russianStripeMessage MsgPaymentStatus = "Статус платежа"
russianStripeMessage MsgSomethingWentWrong = "Что-то пошло не так"
russianStripeMessage MsgViewBookingDetails = "Посмотреть детали бронирования"
russianStripeMessage MsgReturnToHomePage = "Вернуться на главную страницу"
russianStripeMessage MsgFinish = "Завершить"
russianStripeMessage MsgPaymentIntentCancelled = "Намерение платежа было отменено"
russianStripeMessage MsgClose = "Закрыть"
russianStripeMessage MsgAnUnexpectedErrorOccurred = "Произошла непредвиденная ошибка"
russianStripeMessage MsgPaymentSucceeded = "Платеж прошел успешно"
russianStripeMessage MsgYourPaymentIsProcessing = "Ваш платеж обрабатывается"
russianStripeMessage MsgYourPaymentWasNotSuccessful = "Ваш платеж не был успешным, попробуйте еще раз"


defaultStripeMessage :: StripeMessage -> Text
defaultStripeMessage = englishStripeMessage
