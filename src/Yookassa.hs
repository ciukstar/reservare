{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Yookassa where

import Control.Exception.Safe
    ( tryAny, SomeException (SomeException), Exception (fromException)
    )
import Control.Lens ((?~), (.~))
import qualified Control.Lens as L ((^.))
    
import Data.Aeson (object, (.=), Value (String, Number))
import Data.Aeson.Lens ( AsValue(_String), key)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (unpack, pack, Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toASCIIBytes)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, innerJoin, on, where_, val
    , (:&) ((:&)), (^.), (==.), (=.)
    , update, set
    )
import Database.Persist (Entity(Entity))
import Database.Persist.Sql (SqlBackend, insert)

import Model
    ( endpointYookassa
    , ultDestKey, statusSuccess, statusError
    , BookId, PayOptionId
    , Service (Service)
    , Workspace (Workspace), Book
    , Payment
      ( Payment, paymentBook, paymentOption, paymentTime, paymentAmount
      , paymentCurrency, paymentIdetifier, paymentStatus, paymentError
      )
    , EntityField
      ( BookId, BookService, ServiceId, ServiceWorkspace, WorkspaceId
      , ServiceAvailable, PaymentStatus, PaymentId
      ), PaymentId
    )

import Network.HTTP.Client
    ( HttpExceptionContent(StatusCodeException)
    , HttpException (HttpExceptionRequest)
    )
import Network.Wreq
    ( postWith, responseBody, auth, defaults, basicAuth
    , header, getWith
    )
    
import Settings (widgetFile)

import Text.Julius (RawJS(rawJS))
import Text.Read (readMaybe)

import Yesod.Core
    ( YesodSubDispatch (yesodSubDispatch), Application
    , mkYesodSubDispatch, Html, Yesod (defaultLayout), SubHandlerFor
    , MonadHandler (liftHandler), addScriptRemote, setTitleI, newIdent
    , getUrlRender, getMessages
    , MonadIO (liftIO), addMessage, toHtml, addMessageI
    , lookupSession, invalidArgs, invalidArgsI, getRouteToParent
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Persist.Core (YesodPersist(runDB, YesodPersistBackend))

import Yookassa.Data
    ( Yookassa, resourcesYookassa
    , YesodYookassa
      ( getYookassaConfShopId, getYookassaConfSecret
      , getBookDetailsR, getHomeR
      )
    , Route (CheckoutR, CompletionR)
    , YookassaMessage
      ( MsgBack, MsgCheckout, MsgPaymentAmount, MsgCancel
      , MsgYooKassa, MsgUnhandledError, MsgInvalidPaymentAmount
      , MsgYourBookingHasBeenCreatedSuccessfully, MsgPaymentStatus
      , MsgViewBookingDetails, MsgReturnToHomePage, MsgFinish
      , MsgPaymentDeclined, MsgSomethingWentWrong, MsgClose
      )
    )


getCompletionR :: (YesodYookassa m, YesodPersist m, YesodPersistBackend m ~ SqlBackend)
               => BookId -> PaymentId -> Text -> SubHandlerFor Yookassa m Html
getCompletionR bid pid paymentId = do

    shopid <- liftHandler $ encodeUtf8 <$> getYookassaConfShopId
    secret <- liftHandler $ encodeUtf8 <$> getYookassaConfSecret

    response <- liftIO $ getWith
              ( defaults & auth ?~ basicAuth shopid secret )
              ( unpack (endpointYookassa <> "/" <> paymentId) )

    let status = response L.^. responseBody . key "status" . _String
    
    liftHandler $ runDB $ update $ \x -> do
        set x [PaymentStatus =. val status]
        where_ $ x ^. PaymentId ==. val pid
    
    homeR <- liftHandler getHomeR
    bookDetailsR <- liftHandler $ getBookDetailsR bid

    addMessageI statusSuccess MsgYourBookingHasBeenCreatedSuccessfully
    msgs <- getMessages
    liftHandler $ defaultLayout $ do
        setTitleI MsgPaymentStatus
        idHeader <- newIdent
        idMain <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "common/css/main")
        $(widgetFile "gateways/yookassa/completion")


getCheckoutR :: (YesodYookassa m, YesodPersist m, YesodPersistBackend m ~ SqlBackend)
             => BookId -> PayOptionId -> SubHandlerFor Yookassa m Html
getCheckoutR bid oid = do

    book <- liftHandler $ runDB $ selectOne $ do
        x :& s :& w <- from $ table @Book
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. BookService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
        where_ $ x ^. BookId ==. val bid
        where_ $ s ^. ServiceAvailable ==. val True
        return (x,s,w)

    case book of
      Nothing -> invalidArgs [pack $ show bid]
      Just (_,Entity _ (Service _ sname _ cents _ _ _),Entity _ (Workspace _ _ _ _ currency)) -> do

          homeR <- liftHandler getHomeR
          
          shopid <- liftHandler $ encodeUtf8 <$> getYookassaConfShopId
          secret <- liftHandler $ encodeUtf8 <$> getYookassaConfSecret

          idempotenceKey <- liftIO nextRandom

          response <- liftIO $ tryAny $ postWith
              ( defaults & auth ?~ basicAuth shopid secret
                & header "Idempotence-Key" .~ [toASCIIBytes idempotenceKey]
                & header "Content-Type" .~ ["application/json"]
              )
              ( unpack endpointYookassa )
              ( object [ "amount" .= object [ "value" .= Number (fromIntegral cents / 100)
                                            , "currency" .= currency
                                            ]
                       , "confirmation" .= object [ "type" .= String "embedded" ]
                       , "capture" .= True
                       , "description" .= sname
                       ]
              )

          case response of
            Left e@(SomeException _) -> case fromException e of
              Just (HttpExceptionRequest _ (StatusCodeException _ bs)) -> do
                  addMessage statusError (toHtml $ bs L.^. key "description" . _String)
                  msgs <- getMessages
                  liftHandler $ defaultLayout $ do
                      setTitleI MsgCheckout
                      idHeader <- newIdent
                      idMain <- newIdent
                      $(widgetFile "common/css/header")
                      $(widgetFile "common/css/main") 
                      $(widgetFile "gateways/yookassa/error")

              _otherwise -> do
                  addMessageI statusError MsgUnhandledError
                  msgs <- getMessages
                  liftHandler $ defaultLayout $ do
                      setTitleI MsgCheckout
                      idHeader <- newIdent
                      idMain <- newIdent
                      $(widgetFile "common/css/header")
                      $(widgetFile "common/css/main")
                      $(widgetFile "gateways/yookassa/error")

            Right r -> do
                now <- liftIO getCurrentTime
                
                let paymentId = r L.^. responseBody . key "id" . _String
                let status = r L.^. responseBody . key "status" . _String
                let amountValue = r L.^. responseBody . key "amount" . key "value" . _String
                let amountCurrency = r L.^. responseBody . key "amount" . key "currency" . _String
                let token = r L.^. responseBody . key "confirmation" . key "confirmation_token" . _String
                
                case (truncate . (*100) <$>) . readMaybe @Double . unpack $ amountValue of
                  Nothing -> invalidArgsI [MsgInvalidPaymentAmount]
                  Just amount -> do
                    pid <- liftHandler $ runDB $ insert $ Payment { paymentBook = bid
                                                                  , paymentOption = oid
                                                                  , paymentTime = now
                                                                  , paymentAmount = amount
                                                                  , paymentCurrency = amountCurrency
                                                                  , paymentIdetifier = paymentId
                                                                  , paymentStatus = status
                                                                  , paymentError = Nothing
                                                                  }

                    msgs <- getMessages
                    rndr <- getUrlRender
                    ult <- fromMaybe (rndr homeR) <$> lookupSession ultDestKey
                    rtp <- getRouteToParent
                    
                    liftHandler $ defaultLayout $ do
                        setTitleI MsgCheckout
                        idHeader <- newIdent
                        idMain <- newIdent
                        classCurrency <- newIdent
                        idBannerYookassa <- newIdent
                        idSectionPriceTag <- newIdent
                        idPaymentForm <- newIdent
                        addScriptRemote "https://yookassa.ru/checkout-widget/v1/checkout-widget.js"
                        $(widgetFile "common/css/header")
                        $(widgetFile "common/css/main")
                        $(widgetFile "gateways/yookassa/checkout")


        
instance (YesodYookassa m, YesodPersist m, YesodPersistBackend m ~ SqlBackend) => YesodSubDispatch Yookassa m where
    yesodSubDispatch :: YesodSubRunnerEnv Yookassa m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesYookassa)
