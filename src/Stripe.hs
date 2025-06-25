{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Stripe where

import Control.Exception.Safe
    (tryAny, SomeException (SomeException), Exception (fromException))
import Control.Lens ((^?), (?~), to)
import qualified Control.Lens as L ((^.))
    
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A (Value)
import Data.Aeson.Lens ( AsValue(_String), key, AsNumber (_Integer))
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString as BS (empty)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(getSum, Sum))
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( select, from, table, innerJoin, on, where_, val
    , (:&) ((:&)), (^.), (==.)
    )
import Database.Persist (Entity(Entity))
import Database.Persist.Sql (SqlBackend, insert_)

import Model
    ( endpointStripePaymentIntents, endpointStripePaymentIntentCancel
    , ultDestKey, scriptRemoteStripe, statusSuccess, statusError
    , BookId, PayOptionId
    , Service (Service)
    , Workspace (Workspace), Book
    , Payment
      ( Payment, paymentBook, paymentOption, paymentTime, paymentAmount
      , paymentCurrency, paymentIdetifier, paymentStatus, paymentError
      )
    , EntityField
      ( BookId, BookService, ServiceId, ServiceWorkspace, WorkspaceId
      , ServiceAvailable
      )
    )

import Network.HTTP.Client
    ( HttpExceptionContent(StatusCodeException)
    , HttpException (HttpExceptionRequest)
    )
import Network.Wreq
    ( postWith, responseBody, auth, defaults, basicAuth, getWith
    , FormParam ((:=))
    )
    
import Safe (headMay)

import Settings (widgetFile)

import Stripe.Data
    ( Stripe, resourcesStripe
    , YesodStripe
      ( getStripeConfPk, getUserEmail, getStripeConfSk, getBookDetailsR
      , getHomeR
      )
    , Route
      (CheckoutR, CompletionR, CancelR, IntentR)
    , StripeMessage
      ( MsgBack, MsgCheckout, MsgPaymentAmount, MsgPay, MsgCancel, MsgStripe
      , MsgInvalidPaymentAmount, MsgYourBookingHasBeenCreatedSuccessfully
      , MsgUnhandledError, MsgPaymentStatus, MsgSomethingWentWrong, MsgFinish
      , MsgViewBookingDetails, MsgReturnToHomePage, MsgPaymentIntentCancelled
      , MsgClose
      )
    )
    
import Text.Julius (rawJS)

import Yesod.Core
    ( MonadIO (liftIO), YesodSubDispatch (yesodSubDispatch), Application
    , mkYesodSubDispatch, Html, Yesod (defaultLayout), SubHandlerFor
    , MonadHandler (liftHandler), addScriptRemote, setTitleI, newIdent
    , getRouteToParent, getUrlRender, getMessages, returnJson, addMessage
    , toHtml, addMessageI, redirectUltDest, lookupSession
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Persist.Core (YesodPersist(runDB, YesodPersistBackend))
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (textField)


postCancelR :: (YesodStripe m) => SubHandlerFor Stripe m Html
postCancelR = do
    intent <- runInputGet $ ireq textField "pi"
    homeR <- liftHandler getHomeR
    let endpoint = endpointStripePaymentIntentCancel intent
    sk <- liftHandler $ encodeUtf8 <$> getStripeConfSk
    _ <- liftIO $ postWith (defaults & auth ?~ basicAuth sk BS.empty) endpoint BS.empty
            
    addMessageI statusSuccess MsgPaymentIntentCancelled
    redirectUltDest homeR


getCompletionR :: (YesodStripe m, YesodPersist m, YesodPersistBackend m ~ SqlBackend)
               => BookId -> PayOptionId -> SubHandlerFor Stripe m Html
getCompletionR bid oid = do

    intent <- runInputGet $ ireq textField "payment_intent"
    _ <- runInputGet $ ireq textField "payment_intent_client_secret"
    _ <- runInputGet $ ireq textField "redirect_status"

    sk <- liftHandler $ encodeUtf8 <$> getStripeConfSk
    let endpoint = endpointStripePaymentIntents <> "/" <> intent
    let opts = defaults & auth ?~ basicAuth sk ""
    response <- liftIO $ tryAny $ getWith opts (unpack endpoint)

    homeR <- liftHandler getHomeR
    bookDetailsR <- liftHandler $ getBookDetailsR bid

    case response of
      Left e@(SomeException _) -> case fromException e of
        Just (HttpExceptionRequest _ (StatusCodeException _ bs)) -> do
            addMessage statusError (bs L.^. key "error" . key "message" . _String . to toHtml)
            msgs <- getMessages
            liftHandler $ defaultLayout $ do
                setTitleI MsgCheckout
                idHeader <- newIdent
                idMain <- newIdent
                $(widgetFile "common/css/header")
                $(widgetFile "common/css/main")
                $(widgetFile "gateways/stripe/error")

        _otherwise -> do
            addMessageI statusError MsgUnhandledError
            msgs <- getMessages
            liftHandler $ defaultLayout $ do
                setTitleI MsgCheckout
                idHeader <- newIdent
                idMain <- newIdent
                $(widgetFile "common/css/header")
                $(widgetFile "common/css/main")
                $(widgetFile "gateways/stripe/error")
            
      Right r -> case r ^? responseBody . key "status" . _String of
        Just s@"succeeded" -> do
            
            let intentId = r L.^. responseBody . key "id" . _String
            let amountValue = r ^? responseBody . key "amount" . _Integer . to fromIntegral
            let currency = r L.^. responseBody . key "currency" . _String

            case amountValue of
              Nothing -> do
                addMessageI statusError MsgInvalidPaymentAmount
                msgs <- getMessages
                liftHandler $ defaultLayout $ do
                    setTitleI MsgStripe
                    idHeader <- newIdent
                    idMain <- newIdent
                    $(widgetFile "common/css/header")
                    $(widgetFile "common/css/main")
                    $(widgetFile "gateways/stripe/error")
                    
              Just amount -> do
                now <- liftIO getCurrentTime

                liftHandler $ runDB $ insert_ $ Payment { paymentBook = bid
                                                        , paymentOption = oid
                                                        , paymentTime = now
                                                        , paymentAmount = amount
                                                        , paymentCurrency = currency
                                                        , paymentIdetifier = intentId
                                                        , paymentStatus = s
                                                        , paymentError = Nothing
                                                        }

                addMessageI statusSuccess MsgYourBookingHasBeenCreatedSuccessfully
                msgs <- getMessages
                liftHandler $ defaultLayout $ do
                    setTitleI MsgPaymentStatus
                    idHeader <- newIdent
                    idMain <- newIdent
                    $(widgetFile "common/css/header")
                    $(widgetFile "common/css/main")
                    $(widgetFile "gateways/stripe/completion")

        Just s -> do
                
            addMessage statusError (toHtml s)
            msgs <- getMessages
            liftHandler $ defaultLayout $ do
                setTitleI MsgStripe
                idHeader <- newIdent
                idMain <- newIdent
                $(widgetFile "common/css/header")
                $(widgetFile "common/css/main")
                $(widgetFile "gateways/stripe/error")
                
        Nothing -> do                
            addMessageI statusError MsgSomethingWentWrong
            msgs <- getMessages
            liftHandler $ defaultLayout $ do
                setTitleI MsgStripe
                idHeader <- newIdent
                idMain <- newIdent
                $(widgetFile "common/css/header")
                $(widgetFile "common/css/main")
                $(widgetFile "gateways/stripe/error")


postIntentR :: (YesodStripe m) => Int -> Text -> SubHandlerFor Stripe m A.Value
postIntentR cents currency = do
    sk <- liftHandler $ encodeUtf8 <$> getStripeConfSk
    let opts = defaults & auth ?~ basicAuth sk ""
    response <- liftIO $ tryAny $ postWith opts (unpack endpointStripePaymentIntents)
        [ "amount" := cents
        , "currency" := currency
        , "payment_method_types[]" := ("card" :: Text)
        , "payment_method_types[]" := ("paypal" :: Text)
        ]

    case response of
      Left e@(SomeException _) -> case fromException e of
        Just (HttpExceptionRequest _ (StatusCodeException _ bs)) -> do
            returnJson $ object [ "status" .= ("error" :: Text)
                                , "message" .= (bs L.^. key "error" . key "message" . _String)
                                ]

        _otherwise -> do
            returnJson $ object [ "status" .= ("error" :: Text)
                                , "message" .= ("Unhandled error" :: Text)
                                ]
            
      Right r -> do
          let intent = r ^? responseBody . key "id" . _String

          returnJson $ object [ "status" .= ("ok" :: Text)
                              , "paymentIntentId" .= intent
                              , "clientSecret"    .= (r ^? responseBody . key "client_secret")
                              ]


getCheckoutR :: (YesodStripe m, YesodPersist m, YesodPersistBackend m ~ SqlBackend)
             => BookId -> PayOptionId -> SubHandlerFor Stripe m Html
getCheckoutR bid oid = do

    pk <- liftHandler getStripeConfPk

    services <- liftHandler $ runDB ( select $ do
        x :& s :& w <- from $ table @Book
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. BookService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. BookId ==. val bid
        where_ $ s ^. ServiceAvailable ==. val True
        return (s,w) )

    let items = (\(Entity _ (Service _ name _ _ _ _ _),_) -> object ["id" .= name]) <$> services
    let cents = getSum $ mconcat $ (\(Entity _ (Service _ _ _ price _ _ _),_) -> Sum price) <$> services
    let currency = maybe "USD" (\(_, Entity _ (Workspace _ _ _ _ c)) -> c) (headMay services)

    rtp <- getRouteToParent
    rndr <- getUrlRender
    email <- liftHandler getUserEmail
    homeR <- liftHandler getHomeR
    ult <- fromMaybe (rndr homeR) <$> lookupSession ultDestKey

    let confirmParams = encodeToLazyText $ object
            [ "return_url"    .= (rndr $ rtp $ CompletionR bid oid :: Text)
            , "receipt_email" .= email
            ]

    msgs <- getMessages
    liftHandler $ defaultLayout $ do
        setTitleI MsgCheckout
        idHeader <- newIdent
        idButtonBack <- newIdent
        idMain <- newIdent
        idBannerStripe <- newIdent
        idSectionPriceTag <- newIdent
        classCurrency <- newIdent
        idFormPayment <- newIdent
        idElementPayment <- newIdent
        idButtonSubmitPayment <- newIdent
        idButtonCancelPayment <- newIdent
        addScriptRemote scriptRemoteStripe
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "gateways/stripe/checkout")


        
instance (YesodStripe m, YesodPersist m, YesodPersistBackend m ~ SqlBackend) => YesodSubDispatch Stripe m where
    yesodSubDispatch :: YesodSubRunnerEnv Stripe m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesStripe)
