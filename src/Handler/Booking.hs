{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Booking
  ( getBookServicesR
  , getBookStaffR
  , getBookTimingR
  , getBookPaymentR
  , getBookCheckoutR
  , getBookPayCompletionR
  , postBookPaymentIntentR
  , postBookPayR
  , postBookPaymentIntentCancelR
  ) where

import Control.Lens ((^?), (?~))

import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson as A (Value)
import qualified Data.ByteString as BS (empty)
import Data.Function ((&))
import Data.Text (pack, Text)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, on
    , (^.), (==.), (:&)((:&))
    )
import Database.Persist (Entity (Entity))
    
import Foundation
    ( Handler, App (appSettings)
    , Route
      ( HomeR, BookServicesR, BookStaffR, BookTimingR, BookPaymentR
      , BookCheckoutR, BookPayCompletionR, BookPaymentIntentR
      , BookPayR, BookPaymentIntentCancelR
      )
    , AppMessage
      ( MsgServices, MsgNext, MsgServices, MsgThereAreNoDataYet
      , MsgBack, MsgStaff, MsgBusiness, MsgWorkspace, MsgAppointmentTime
      , MsgPaymentMethod, MsgPayNow, MsgPayAtVenue, MsgCheckout
      , MsgPaymentAmount, MsgCancel, MsgPay, MsgPaymentIntentCancelled
      )
    )
    
import Model
    ( statusSuccess
    , ServiceId, Service(Service)
    , Workspace (Workspace)
    , Business (Business)
    , Assignment (Assignment)
    , Staff (Staff)
    , User (User), PayMethod (PayNow)
    , EntityField
      ( ServiceId, ServiceWorkspace, WorkspaceId, WorkspaceBusiness
      , BusinessId, ServiceName, AssignmentStaff, StaffId, StaffName
      , BusinessName, WorkspaceName
      )
    )
    
import Network.Wreq
    ( postWith, responseBody, auth, defaults, basicAuth
    , FormParam ((:=))
    )

import Settings
    ( widgetFile, StripeConf (stripeConfPk, stripeConfSk)
    , AppSettings (appStripeConf)
    )

import Text.Hamlet (Html)
import Text.Julius (rawJS)
import Text.Printf (printf)

import Widgets (widgetSnackbar, widgetBanner)

import Yesod.Auth (maybeAuth)
import Yesod.Core.Handler
    ( getMessages, newIdent, getYesod, getUrlRenderParams
    , getRequest, YesodRequest (reqGetParams), redirect, addMessageI
    )
import Yesod.Core (Yesod(defaultLayout), returnJson, MonadIO (liftIO), whamlet, addScriptRemote)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (textField)
import Yesod.Persist.Core (YesodPersist(runDB))


postBookPaymentIntentCancelR :: Handler ()
postBookPaymentIntentCancelR = do
    stati <- reqGetParams <$> getRequest
    intent <- runInputGet $ ireq textField "pi"
    sk <- encodeUtf8 . stripeConfSk . appStripeConf . appSettings <$> getYesod
    _ <- liftIO $ postWith
        (defaults & auth ?~ basicAuth sk BS.empty)
        (printf "https://api.stripe.com/v1/payment_intents/%s/cancel" intent)
        BS.empty
    addMessageI statusSuccess MsgPaymentIntentCancelled
    redirect (BookPayR,("pm",pack $ show PayNow) : stati)


postBookPayR :: Handler Html
postBookPayR = do
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        [whamlet| <h1>DONE |]
    


postBookPaymentIntentR :: Int -> Text -> Handler A.Value
postBookPaymentIntentR cents currency = do
    let api = "https://api.stripe.com/v1/payment_intents"
    sk <- encodeUtf8 . stripeConfSk . appStripeConf . appSettings <$> getYesod
    let opts = defaults & auth ?~ basicAuth sk ""
    r <- liftIO $ postWith opts api [ "amount" := cents
                                    , "currency" := currency
                                    , "payment_method_types[]" := ("card" :: Text)
                                    , "payment_method_types[]" := ("paypal" :: Text)
                                    ]

    returnJson $ object [ "paymentIntentId" .= (r ^? responseBody . key "id")
                        , "clientSecret"    .= (r ^? responseBody . key "client_secret")
                        ]


getBookPayCompletionR :: Handler Html
getBookPayCompletionR = do

    params <- reqGetParams <$> getRequest
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFabNext <- newIdent
        $(widgetFile "book/completion/completion")


getBookCheckoutR :: Handler Html
getBookCheckoutR = do

    stati <- reqGetParams <$> getRequest

    user <- maybeAuth
    case user of
      Nothing -> defaultLayout [whamlet|<h1>User Not Loged In|]
      Just (Entity uid (User email _ _ _ _ _ _ _)) -> do
    
          pk <- stripeConfPk . appStripeConf . appSettings <$> getYesod 

          let items = [object ["id" .= ("Liesure Time" :: Text)]]
          let amount = 10 :: Double
          let cents = truncate $ 100 * amount
          let currency = "USD"

          rndr <- getUrlRenderParams

          let confirmParams = encodeToLazyText $ object $ case user of
                Just (Entity uid (User email _ _ _ _ _ _ _)) ->
                    [ "return_url"    .= rndr BookPayCompletionR stati
                    , "receipt_email" .= email
                    ]
                _ -> [ "return_url" .= rndr BookPayCompletionR stati ]

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCheckout
              idButtonBack <- newIdent
              idBannerStripe <- newIdent
              idSectionPriceTag <- newIdent
              idFormPayment <- newIdent
              idElementPayment <- newIdent
              idButtonSubmitPayment <- newIdent
              idButtonCancelPayment <- newIdent
              $(widgetFile "book/checkout/checkout")
              addScriptRemote "https://js.stripe.com/v3/"


getBookPaymentR :: Handler Html
getBookPaymentR = do
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentMethod
        idFabNext <- newIdent
        $(widgetFile "book/payment/payment")


getBookTimingR :: Handler Html
getBookTimingR = do
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFabNext <- newIdent
        $(widgetFile "book/timing/timing")


getBookStaffR :: Handler Html
getBookStaffR = do

    staff <- runDB $ select $ do
        x :& e <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
        orderBy [asc (e ^. StaffName)]
        return (x,e)
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        idFabNext <- newIdent
        $(widgetFile "book/staff/staff")


getBookServicesR :: Handler Html
getBookServicesR = do

    businesses <- runDB $ select $ do
        x <- from $ table @Business
        orderBy [asc (x ^. BusinessName)]
        return x

    workspaces <- runDB $ select $ do
        x <- from $ table @Workspace
        orderBy [asc (x ^. WorkspaceName)]
        return x
    
    services <- runDB $ select $ do
        x :& w :& b <- from $ table @Service
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
        orderBy [asc (x ^. ServiceName)]
        return (x,(w,b))
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        idFabNext <- newIdent
        $(widgetFile "book/services")
