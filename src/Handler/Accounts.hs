{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Accounts
  ( getAccountR
  , getAccountPhotoR
  , getAccountInfoR
  , getAccountEditR
  , getAccountInfoEditR
  , postAccountInfoR
  , postAccountR
  ) where

import Control.Monad (void)

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, val, where_
    , (^.), (==.), (=.)
    , update, set
    )
import Database.Persist (Entity(Entity), entityVal, upsert)
import qualified Database.Persist as P ((=.))

import Foundation
    ( Handler, Form, widgetSnackbar
    , Route
      ( StaticR, AuthR, AccountPhotoR, AccountInfoR, AccountR
      , HomeR, AccountEditR, AccountInfoEditR
      )
    , AppMessage
      ( MsgUserAccount, MsgSuperuser, MsgAdministrator, MsgSignOut
      , MsgPhoto, MsgPersonalInfo, MsgAccount, MsgBack, MsgEdit, MsgCancel
      , MsgSave, MsgFullName, MsgBirthday, MsgNotIndicated, MsgRecordEdited
      , MsgClose, MsgUploadPhoto, MsgTakePhoto
      )
    )
    
import Material3 (md3widget)

import Model
    ( statusSuccess
    , UserId, UserPhoto (UserPhoto), User (User, userName)
    , UserInfo (UserInfo, userInfoBirthDate)
    , EntityField
      ( UserPhotoUser, UserInfoUser, UserInfoBirthDate, UserId
      , UserName, UserPhotoMime, UserPhotoPhoto
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg
    , img_camera_24dp_0000F5_FILL0_wght400_GRAD0_opsz24_svg
    , img_person_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod.Auth (maybeAuth, Route (LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, FileInfo, getMessageRender
    )
import Yesod.Core.Content (TypedContent (TypedContent), ToContent (toContent))
import Yesod.Core.Handler
    ( redirect, getMessages, addMessageI, fileSourceByteString, fileContentType)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Fields (fileField, dayField, textField)
import Yesod.Form.Functions (generateFormPost, runFormPost, mopt)
import Yesod.Form.Types
    ( FieldSettings (FieldSettings), FormResult (FormSuccess)
    , fsLabel, fsTooltip, fsId, fsName, fsAttrs, fvErrors
    , fvId, fvInput
    )
import Yesod.Persist.Core (runDB)


postAccountInfoR :: UserId -> Handler Html
postAccountInfoR uid = do
    ((fr,fw),et) <- runFormPost $ formUserInfo uid Nothing
    case fr of
      FormSuccess r@(UserInfo _ bday) -> do
          void $ runDB $ upsert r [ UserInfoBirthDate P.=. bday ]
          addMessageI statusSuccess MsgRecordEdited
          redirect $ AccountInfoR uid
      _otherwise -> do
          defaultLayout $ do
              setTitleI MsgPersonalInfo
              $(widgetFile "accounts/info/edit")


getAccountInfoEditR :: UserId -> Handler Html
getAccountInfoEditR uid = do
    info <- runDB $ selectOne $ do
        x <- from $ table @UserInfo
        where_ $ x ^. UserInfoUser ==. val uid
        return x
    (fw,et) <- generateFormPost $ formUserInfo uid info
    defaultLayout $ do
        setTitleI MsgPersonalInfo
        $(widgetFile "accounts/info/edit")


getAccountEditR :: UserId -> Handler Html
getAccountEditR uid = do
    user <- maybeAuth
    
    (fw,et) <- generateFormPost $ formAccount user
    
    defaultLayout $ do
        setTitleI MsgUserAccount
        $(widgetFile "accounts/edit")


formAccount :: Maybe (Entity User) -> Form (Maybe Text, Maybe FileInfo)
formAccount user extra = do
    (nameR,nameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (userName . entityVal <$> user)
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    idOverlay <- newIdent
    idUserIdent <- newIdent
    idLabelPhotoUser <- newIdent
    idFigurePhotoUser <- newIdent
    idImgPhotoUser <- newIdent
    idButtonUploadPhoto <- newIdent
    idButtonTakePhoto <- newIdent
    idDialogSnapshot <- newIdent
    idVideo <- newIdent
    idButtonCloseDialogSnapshot <- newIdent
    idButtonCapture <- newIdent

    return ( (,) <$> nameR <*> photoR
           , $(widgetFile "accounts/form")  
           )


getAccountInfoR :: UserId -> Handler Html
getAccountInfoR uid = do

    info <- runDB $ selectOne $ do
        x <- from $ table @UserInfo
        where_ $ x ^. UserInfoUser ==. val uid
        return x

    (fw,et) <- generateFormPost $ formUserInfo uid info

    msgs <- getMessages
    
    defaultLayout $ do
        setTitleI MsgPersonalInfo
        $(widgetFile "accounts/info/info")


formUserInfo :: UserId -> Maybe (Entity UserInfo) -> Form UserInfo
formUserInfo uid info extra = do
    rndr <- getMessageRender
    (bdayR,bdayV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgBirthday
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgBirthday)]
        } (userInfoBirthDate . entityVal <$> info)

    let r = UserInfo uid <$> bdayR
    let w = [whamlet|
                    #{extra}
                    ^{md3widget bdayV}
                    |]
    return (r,w)


postAccountR :: UserId -> Handler Html
postAccountR uid = do
    ((fr,fw),et) <- runFormPost $ formAccount Nothing
    case fr of
      FormSuccess (name,mfi) -> do
          runDB $ update $ \x -> do
              set x [ UserName =. val name ]
              where_ $ x ^. UserId ==. val uid
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                void $ runDB $ upsert (UserPhoto uid (fileContentType fi) bs Nothing)
                    [UserPhotoMime P.=. fileContentType fi, UserPhotoPhoto P.=. bs]
            Nothing -> return ()
          addMessageI statusSuccess MsgRecordEdited
          redirect $ AccountR uid
          
      _otherwise -> defaultLayout $ do
          setTitleI MsgUserAccount
          $(widgetFile "accounts/edit")


getAccountR :: UserId -> Handler Html
getAccountR uid = do
    user <- maybeAuth
    defaultLayout $ do
        setTitleI MsgUserAccount
        
        idPanelAccount <- newIdent
        $(widgetFile "accounts/account")


getAccountPhotoR :: UserId -> Handler TypedContent
getAccountPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_FILL0_wght400_GRAD0_opsz24_svg
