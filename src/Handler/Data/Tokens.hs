{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Data.Tokens
  ( getTokensR
  , postTokensR
  , getTokensGoogleapisHookR
  , postTokensGoogleapisClearR
  ) where

import qualified Control.Lens as L ((^.), (?~))
import Control.Monad ((<=<), void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key, AsValue (_String))
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Function ((&))
import qualified Data.List.Safe as LS (last)
import Data.Text (Text, pack, unpack, splitOn)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist
    ( Entity(Entity, entityVal)
    , PersistStoreWrite (delete)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.))

import Import.NoFoundation (tryAny)

import Foundation
    ( App (appSettings), Form, Handler, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (DataR)
    , DataR
      ( TokensR, TokensGoogleapisHookR, TokensGoogleapisClearR
      )
    , AppMessage
      ( MsgTokens, MsgInitialize, MsgUserSession, MsgDatabase
      , MsgStoreType, MsgInvalidStoreType, MsgRecordEdited, MsgClearSettings
      , MsgRecordDeleted, MsgInvalidFormData, MsgCleared, MsgEmailAddress
      , MsgGoogleSecretManager
      )
    )

import Material3 (md3radioField, md3widget)

import Model
    ( gmailAccessToken, gmailRefreshToken, apiInfoGoogle
    , StoreType
      ( StoreTypeDatabase, StoreTypeSession, StoreTypeGoogleSecretManager
      )
    , Store (Store), Token (Token, tokenStore)
    , EntityField (StoreVal, TokenApi, TokenStore)
    , gmailSender, statusSuccess, statusError, gmailAccessTokenExpiresIn
    , secretVolumeGmail
    )
    
import Network.Wreq
    ( post, FormParam ((:=)), responseBody, defaults, auth, oauth2Bearer
    , postWith, getWith
    )
import Network.Wreq.Lens (statusCode, responseStatus)

import Settings
    ( widgetFile, AppSettings (appGoogleApiConf, appGcloudConf)
    , GoogleApiConf (googleApiConfClientSecret, googleApiConfClientId)
    , GcloudConf (gcloudProjectId)
    )
import System.IO (readFile')

import Text.Blaze.Html (preEscapedToHtml, toHtml)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.Text (st)

import Yesod.Core
    ( Yesod(defaultLayout), whamlet, SomeMessage (SomeMessage), getYesod
    , getUrlRender, deleteSession, getMessages, logWarn
    , addMessage, setUltDestCurrent, newIdent
    )
import Yesod.Core.Handler (redirect, addMessageI, setSession)
import Yesod.Core.Widget (setTitleI)
import Yesod (YesodPersist(runDB))
import Yesod.Form.Input (ireq, runInputGet)
import Yesod.Form.Fields (optionsPairs, textField, emailField)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    ( FormResult (FormSuccess), FieldView (fvInput)
    , FieldSettings (FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    )


getTokensGoogleapisHookR :: Handler Html
getTokensGoogleapisHookR = do
    rndr <- getUrlRender
    app <- appSettings <$> getYesod
    let googleClientId = googleApiConfClientId . appGoogleApiConf $ app
    let googleClientSecret = googleApiConfClientSecret . appGoogleApiConf $ app

    code <- runInputGet $ ireq textField "code"
    state <- readMaybe .  unpack <$> runInputGet (ireq textField "state")

    r <- liftIO $ post "https://oauth2.googleapis.com/token"
         [ "code" := code
         , "redirect_uri" := rndr (DataR TokensGoogleapisHookR)
         , "client_id" := googleClientId
         , "client_secret" := googleClientSecret
         , "grant_type" := ("authorization_code" :: Text)
         ]

    let _status = r L.^. responseStatus . statusCode
    let _tokenType = r L.^. responseBody . key "token_type" . _String
    let _scope = r L.^. responseBody . key "scope" . _String

    let accessToken = r L.^. responseBody . key "access_token" . _String
    let refreshToken = r L.^. responseBody . key "refresh_token" . _String
    let expiresIn = r L.^. responseBody . key "expires_in" . _String

    setSession gmailAccessToken accessToken
    setSession gmailRefreshToken refreshToken
    setSession gmailAccessTokenExpiresIn expiresIn

    case state of
      Just (email,x@StoreTypeSession) -> do
          setSession gmailSender email
          _ <- runDB $ upsert (Token apiInfoGoogle x) [TokenStore P.=. x]
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR TokensR
      Just (email,x@StoreTypeDatabase) -> do
          setSession gmailSender email
          Entity tid _ <- runDB $ upsert (Token apiInfoGoogle x) [TokenStore P.=. x]
          _ <- runDB $ upsert (Store tid gmailAccessToken accessToken) [StoreVal P.=. accessToken]
          _ <- runDB $ upsert (Store tid gmailRefreshToken refreshToken) [StoreVal P.=. refreshToken]
          _ <- runDB $ upsert (Store tid gmailSender email) [StoreVal P.=. email]
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR TokensR
      Just (email,x@StoreTypeGoogleSecretManager) -> do
          setSession gmailSender email

          let project = gcloudProjectId . appGcloudConf $ app

          let opts = defaults & auth L.?~ oauth2Bearer (encodeUtf8 accessToken)
          response <- liftIO $ tryAny $ postWith opts
              (unpack [st|#{projects}/#{project}/secrets/#{gmailRefreshToken}:addVersion|])
              (object [ "payload" .= object [ "data" .= decodeUtf8 (B64.encode (encodeUtf8 refreshToken)) ]])

          -- destroy previous version
          case response of
            Right res -> do

                let prev :: Maybe Int
                    prev = (fmap (\y -> y - 1) . readMaybe . unpack) <=< (LS.last . splitOn "/")
                        $ res L.^. responseBody . key "name" . _String
                
                case prev of
                  Just v | v > 0 -> do
                               void $ liftIO $ tryAny $ postWith opts
                                   (unpack [st|#{projects}/#{project}/secrets/#{gmailRefreshToken}/versions/#{v}:destroy|])
                                   (object [])
                         | otherwise -> return ()
                  Nothing -> return ()

            Left e -> do
                let msg = pack $ show e
                addMessage statusError (toHtml msg)
                $(logWarn) msg

          Entity tid _ <- runDB $ upsert (Token apiInfoGoogle x) [TokenStore P.=. x]
          _ <- runDB $ upsert (Store tid gmailSender email) [StoreVal P.=. email]

          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR TokensR
      Nothing -> do
          addMessageI statusError MsgInvalidStoreType
          redirect $ DataR TokensR


postTokensGoogleapisClearR :: Handler Html
postTokensGoogleapisClearR = do

    tokenGmail <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val apiInfoGoogle
        return x

    ((fr,fwGmailClear),etGmailClear) <- runFormPost formTokensClear
    case (fr,tokenGmail) of
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeSession))) -> do
          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          deleteSession gmailAccessTokenExpiresIn
          deleteSession gmailSender
          runDB $ delete tid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR TokensR
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeDatabase))) -> do
          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          deleteSession gmailAccessTokenExpiresIn
          deleteSession gmailSender
          runDB $ delete tid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR TokensR
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeGoogleSecretManager))) -> do
          app <- appSettings <$> getYesod
          -- 1. read refresh token from mounted volume
          refreshToken <- liftIO $ readFile' secretVolumeGmail

          -- 2. get access token from googleapi
          refreshResponse <- liftIO $ post "https://oauth2.googleapis.com/token"
              [ "refresh_token" := refreshToken
              , "client_id" := (googleApiConfClientId . appGoogleApiConf $ app)
              , "client_secret" := (googleApiConfClientSecret . appGoogleApiConf $ app)
              , "grant_type" := ("refresh_token" :: Text)
              ]

          let newAccessToken = refreshResponse L.^. responseBody . key "access_token" . _String

          let opts = defaults & auth L.?~ oauth2Bearer (encodeUtf8 newAccessToken)

          let project = gcloudProjectId . appGcloudConf $ app
          
          res <- liftIO $ getWith opts
              (unpack [st|#{projects}/#{project}/secrets/#{gmailRefreshToken}/versions/latest|])

          let ver :: Maybe Int
              ver = (readMaybe . unpack) <=< (LS.last . splitOn "/") $ res L.^. responseBody . key "name" . _String

          case ver of
            Just v -> do

                void $ liftIO $ tryAny $ postWith opts
                    (unpack [st|#{projects}/#{project}/secrets/#{gmailRefreshToken}/versions/#{v}:destroy|])
                    (object [])

            Nothing -> return ()

          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          deleteSession gmailAccessTokenExpiresIn
          deleteSession gmailSender

          runDB $ delete tid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR TokensR

      (FormSuccess (),Nothing) -> do
          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          deleteSession gmailAccessTokenExpiresIn
          deleteSession gmailSender
          addMessageI statusSuccess MsgCleared
          redirect $ DataR TokensR
      _otherwise -> do
          (fwGmail,etGmail) <- generateFormPost $ formStoreOptions tokenGmail
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgTokens
              idHeader <- newIdent
              idMain <- newIdent
              idOverlay <- newIdent
              idDialogMainMenu <- newIdent
              formTokensGmail <- newIdent
              formTokensGmailClear <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/tokens/tokens")


formTokensClear :: Form ()
formTokensClear extra = return (FormSuccess (),[whamlet|#{extra}|])


postTokensR :: Handler Html
postTokensR = do

    tokenGmail <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val apiInfoGoogle
        return x

    ((fr,fwGmail),etGmail) <- runFormPost $ formStoreOptions tokenGmail
    case fr of
      FormSuccess x -> do
          app <- appSettings <$> getYesod
          rndr <- getUrlRender

          let scope :: Text
              scope = "https://www.googleapis.com/auth/gmail.send https://www.googleapis.com/auth/cloud-platform"

          r <- liftIO $ post "https://accounts.google.com/o/oauth2/v2/auth"
              [ "redirect_uri" := rndr (DataR TokensGoogleapisHookR)
              , "response_type" := ("code" :: Text)
              , "prompt" := ("consent" :: Text)
              , "client_id" := (googleApiConfClientId . appGoogleApiConf $ app)
              , "scope" := scope
              , "access_type" := ("offline" :: Text)
              , "state" := pack (show x)
              ]

          return $ preEscapedToHtml $ decodeUtf8 $ toStrict (r L.^. responseBody)

      _otherwise -> do
          (fwGmailClear,etGmailClear) <- generateFormPost formTokensClear
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgTokens
              idHeader <- newIdent
              idMain <- newIdent
              idOverlay <- newIdent
              idDialogMainMenu <- newIdent
              formTokensGmail <- newIdent
              formTokensGmailClear <- newIdent
              $(widgetFile "common/css/header")
              $(widgetFile "data/tokens/tokens")


getTokensR :: Handler Html
getTokensR = do
    
    tokenGmail <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val apiInfoGoogle
        return x

    (fwGmail,etGmail) <- generateFormPost $ formStoreOptions tokenGmail
    (fwGmailClear,etGmailClear) <- generateFormPost formTokensClear
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgTokens
        idOverlay <- newIdent
        idHeader <- newIdent
        idMain <- newIdent
        idDialogMainMenu <- newIdent
        formTokensGmail <- newIdent
        formTokensGmailClear <- newIdent
        $(widgetFile "common/css/header")
        $(widgetFile "data/tokens/tokens")


formStoreOptions :: Maybe (Entity Token)-> Form (Text,StoreType)
formStoreOptions token extra = do
    
    let storeOptions = [ (MsgUserSession, StoreTypeSession)
                       , (MsgDatabase, StoreTypeDatabase)
                       , (MsgGoogleSecretManager, StoreTypeGoogleSecretManager)
                       ]
                       
    (emailR,emailV) <- mreq emailField FieldSettings
        { fsLabel = SomeMessage MsgEmailAddress
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = []
        } (Just "ciukstar@gmail.com")
        
    (storeR,storeV) <- mreq (md3radioField (optionsPairs storeOptions)) FieldSettings
        { fsLabel = SomeMessage MsgStoreType
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = [("class","row vertical")]
        } (tokenStore . entityVal <$> token)
        
    return ( (,) <$> emailR <*> storeR
           , [whamlet|
               #{extra}
               ^{md3widget emailV}
               <fieldset>
                 <legend>_{MsgStoreType}<sup>*
                 ^{fvInput storeV}
             |]
           )


projects :: Text
projects = "https://secretmanager.googleapis.com/v1/projects" :: Text

