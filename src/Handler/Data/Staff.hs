{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Data.Staff
  ( getStaffR
  , getEmployeeR
  , getEmployeeNewR
  , postStaffR
  , getEmployeeEditR
  , postEmployeeDeleR
  , postEmployeeR
  ) where

import Control.Monad (void)

import Data.Maybe (fromMaybe)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, asc, selectOne, where_, val
    , (^.), (==.)
    )
import Database.Persist
    ( Entity (Entity), entityVal, entityKey, insert_)
import qualified Database.Persist as P (delete, PersistStoreWrite (replace))
    
import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR (EmployeeR, EmployeeNewR, StaffR, EmployeeEditR, EmployeeDeleR)
    , AppMessage
      ( MsgStaff, MsgAdd, MsgYouMightWantToAddAFew, MsgThereAreNoDataYet
      , MsgCancel, MsgSave, MsgBack, MsgFullName, MsgAccount, MsgMobile
      , MsgPhone, MsgRecordAdded, MsgEmployee, MsgServiceAssignments
      , MsgDetails, MsgDeleteAreYouSure, MsgEdit, MsgDele, MsgConfirmPlease
      , MsgInvalidFormData, MsgRecordDeleted, MsgRecordEdited
      )
    )
    
import Material3 (md3mreq, md3telField, md3textField, md3mopt, md3selectField)

import Model
    ( statusError, statusSuccess
    , StaffId, Staff (Staff, staffName, staffMobile, staffPhone, staffAccount)
    , EntityField (StaffId, UserName, UserId)
    , User (User, userName, userEmail)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetAccount, widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages, whamlet, addMessageI, redirect
    , MonadHandler (liftHandler), SomeMessage (SomeMessage)
    )
import Yesod.Core.Handler (getMessageRender)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( FieldSettings
      ( FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs
      )
    , FormResult (FormSuccess), FieldView (fvInput), optionsPairs, runFormPost
    )
import Yesod.Form.Functions (generateFormPost)
import Yesod.Persist (YesodPersist(runDB))


postEmployeeDeleR :: StaffId -> Handler Html
postEmployeeDeleR sid = do
    ((fr,_),_) <- runFormPost formEmployeeDelete
    case fr of
      FormSuccess () -> do
          void $ runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR StaffR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ EmployeeR sid


postEmployeeR :: StaffId -> Handler Html
postEmployeeR sid = do
    
    employee <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formEmployee employee

    case fr of
      FormSuccess r -> do
          void $ runDB $ P.replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ EmployeeR sid
      _otherwise -> do    
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgEmployee
              idFormStaff <- newIdent
              $(widgetFile "data/staff/edit")


getEmployeeEditR :: StaffId -> Handler Html
getEmployeeEditR sid = do
    
    employee <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formEmployee employee
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployee
        idFormStaff <- newIdent
        $(widgetFile "data/staff/edit")


postStaffR :: Handler Html
postStaffR = do

    ((fr,fw),et) <- runFormPost $ formEmployee Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR StaffR
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              idFormStaff <- newIdent
              $(widgetFile "data/staff/new")


getEmployeeNewR :: Handler Html
getEmployeeNewR = do

    (fw,et) <- generateFormPost $ formEmployee Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff 
        idFormStaff <- newIdent
        $(widgetFile "data/staff/new")


formEmployee :: Maybe (Entity Staff) -> Form Staff
formEmployee staff extra = do

    msgr <- getMessageRender
    
    (nameR,nameV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgFullName)]
        } (staffName . entityVal <$> staff)

    users <- liftHandler $ runDB $ select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserName), desc (x ^. UserId)]
        return x
        
    (accountR, accountV) <- md3mopt (md3selectField (optionsPairs (options <$> users))) FieldSettings
        { fsLabel = SomeMessage MsgAccount
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAccount)]
        } (staffAccount . entityVal <$> staff)
    
    (mobileR,mobileV) <- md3mopt md3telField FieldSettings
        { fsLabel = SomeMessage MsgMobile
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgMobile)]
        } (staffMobile . entityVal <$> staff)
    
    (phoneR,phoneV) <- md3mopt md3telField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgPhone)]
        } (staffPhone . entityVal <$> staff)

    return ( Staff <$> nameR <*> accountR <*> mobileR <*> phoneR
           , [whamlet|#{extra} ^{fvInput nameV} ^{fvInput accountV} ^{fvInput mobileV} ^{fvInput phoneV}|]
           )
        
  where
      options e = (fromMaybe (userEmail . entityVal $ e) (userName . entityVal $ e), entityKey e)


getEmployeeR :: StaffId -> Handler Html
getEmployeeR sid = do

    employee <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val sid
        return x

    (fw2,et2) <- generateFormPost formEmployeeDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployee 
        idPanelDetails <- newIdent
        idTabDetails <- newIdent
        $(widgetFile "data/staff/employee")


formEmployeeDelete :: Form ()
formEmployeeDelete extra = return (pure (), [whamlet|#{extra}|])


getStaffR :: Handler Html
getStaffR = do

    staff <- runDB $ select $ do
        x <- from $ table @Staff
        orderBy [desc (x ^. StaffId)]
        return x
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff 
        idFabAdd <- newIdent
        $(widgetFile "data/staff/staff")

