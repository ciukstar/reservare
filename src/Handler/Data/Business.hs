{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Data.Business
  ( getDataBusinessesR
  , getDataBusinessNewR
  , postDataBusinessesR
  , getDataBusinessR
  , getDataBusinessEditR
  , postDataBusinessDeleR
  , postDataBusinessR
  , getDataWorkspacesR
  , getDataWorkspaceNewR
  , postDataWorkspacesR
  , getDataWorkspaceR
  , getDataWorkspaceEditR
  , postDataWorkspaceR
  , postDataWorkspaceDeleR
  , getDataWorkingHoursR
  , getDataWorkingSlotsR
  , getDataWorkingSlotNewR
  , getDataWorkingSlotR
  , getDataWorkingSlotEditR
  , postDataWorkingSlotDeleR
  , postDataWorkingSlotsR
  , postDataWorkingSlotR
  , getPayOptionsR
  , postPayOptionsR
  , getPayOptionR
  , postPayOptionR
  , getPayOptionNewR
  , getPayOptionEditR
  , postPayOptionDeleR
  ) where

import qualified Data.Map as M (Map, fromListWith, member, notMember, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, pack)
import Data.Time
    ( UTCTime(utctDay), getCurrentTime, LocalTime (LocalTime)
    , DayPeriod (periodLastDay), diffLocalTime, nominalDiffTimeToSeconds
    )
import Data.Time.Calendar
    ( DayOfWeek (Monday), DayPeriod (periodFirstDay)
    , weekFirstDay, addDays, toGregorian, Day
    )
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , desc, selectOne, where_, val, between
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (delete)
    , entityVal, entityKey, insert_, replace
    )

import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR
      ( DataBusinessesR, DataBusinessNewR, DataBusinessR, DataBusinessEditR
      , DataBusinessDeleR, DataWorkspacesR, DataWorkspaceNewR, DataWorkspaceR
      , DataWorkspaceEditR, DataWorkspaceDeleR, DataWorkingHoursR
      , DataWorkingSlotsR, DataWorkingSlotNewR, DataWorkingSlotR
      , DataWorkingSlotEditR, DataWorkingSlotDeleR
      , PayOptionsR, PayOptionR, PayOptionNewR, PayOptionEditR, PayOptionDeleR
      )
    , AppMessage
      ( MsgBusinesses, MsgThereAreNoDataYet, MsgYouMightWantToAddAFew
      , MsgAdd, MsgOwner, MsgTheName, MsgSave, MsgCancel, MsgBusiness
      , MsgBack, MsgDele, MsgEdit, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordDeleted, MsgInvalidFormData, MsgRecordEdited, MsgRecordAdded
      , MsgDetails, MsgWorkspaces, MsgAddress, MsgWorkspace, MsgAlreadyExists
      , MsgCurrency, MsgTimeZone, MsgWorkingHours, MsgStartTime, MsgEndTime
      , MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgDay, MsgSymbolHour, MsgSymbolMinute, MsgPaymentOptions, MsgPayOptions
      , MsgPaymentOption, MsgPayNow, MsgPayAtVenue, MsgType, MsgYooKassa
      , MsgStripe, MsgDescription, MsgPaymentGateway, MsgPayOption, MsgIcon
      )
    )

import Material3 (md3selectField, md3mreq, md3textField, md3textareaField, md3timeField, md3mopt)

import Model
    ( statusError, statusSuccess
    , BusinessId, Business(Business, businessOwner, businessName)
    , User (User, userName, userEmail)
    , WorkspaceId
    , Workspace
      ( Workspace, workspaceName, workspaceAddress, workspaceCurrency, workspaceTzo
      )
    , WorkingHoursId, WorkingHours (WorkingHours, workingHoursStart, workingHoursEnd)
    , PayOptionId
    , PayOption (PayOption, payOptionType, payOptionName, payOptionDescr, payOptionGateway, payOptionIcon)
    , PayMethod (PayNow, PayAtVenue), PayGateway (PayGatewayStripe, PayGatewayYookassa)
    , EntityField
      ( UserName, UserId, BusinessId, BusinessOwner, WorkspaceId
      , WorkspaceBusiness, BusinessName, WorkspaceName, WorkingHoursWorkspace
      , WorkingHoursDay, WorkingHoursId, PayOptionWorkspace, PayOptionId, PayOptionType
      , PayOptionName
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Printf (printf)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Widgets (widgetBanner, widgetSnackbar, widgetAccount, widgetMenu)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getMessageRender
    , MonadHandler (liftHandler), redirect, whamlet, addMessageI
    , YesodRequest (reqGetParams), getRequest, MonadIO (liftIO)
    )
import Yesod.Core.Handler (newIdent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( Field, FieldSettings (FieldSettings, fsName, fsLabel, fsTooltip, fsId, fsAttrs)
    , FieldView (fvInput), FormResult (FormSuccess), Textarea (Textarea)
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Fields (optionsPairs)
import Yesod.Persist.Core (runDB)


postPayOptionDeleR :: BusinessId -> WorkspaceId -> PayOptionId -> Handler Html
postPayOptionDeleR bid wid oid = do
    
    ((fr,_),_) <- runFormPost formPayOptionDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete oid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ PayOptionsR bid wid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ PayOptionR bid wid oid


formPayOptionDelete :: Form ()
formPayOptionDelete extra = return (pure (), [whamlet|#{extra}|])


getPayOptionEditR :: BusinessId -> WorkspaceId -> PayOptionId -> Handler Html
getPayOptionEditR bid wid oid = do

    option <- runDB $ selectOne $ do
        x <- from $ table @PayOption
        where_ $ x ^. PayOptionId ==. val oid
        return x
        
    (fw,et) <- generateFormPost $ formPayOption wid option
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentOption
        idFormPayOption <- newIdent
        $(widgetFile "data/business/workspaces/pay/edit")


getPayOptionNewR :: BusinessId -> WorkspaceId -> Handler Html
getPayOptionNewR bid wid = do
    (fw,et) <- generateFormPost $ formPayOption wid Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentOption
        idFormPayOption <- newIdent
        $(widgetFile "data/business/workspaces/pay/new")


formPayOption :: WorkspaceId -> Maybe (Entity PayOption) -> Form PayOption
formPayOption wid option extra = do

    msgr <- getMessageRender
    
    (typeR,typeV) <- md3mreq (md3selectField (optionsPairs types)) FieldSettings
        { fsLabel = SomeMessage MsgType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgType)]
        } (payOptionType . entityVal <$> option)
    
    (nameR,nameV) <- md3mreq (uniqueNameField wid typeR) FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (payOptionName . entityVal <$> option)
    
    (gateR,gateV) <- md3mopt (md3selectField (optionsPairs gates)) FieldSettings
        { fsLabel = SomeMessage MsgPaymentGateway
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgPaymentGateway)]
        } (payOptionGateway . entityVal <$> option)
    
    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgDescription)]
        } (payOptionDescr . entityVal <$> option)
    
    (iconR,iconV) <- md3mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgIcon
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgIcon)]
        } (payOptionIcon . entityVal <$> option)
    
    let r = PayOption wid <$> typeR <*> nameR <*> gateR <*> descrR <*> iconR
    let w = [whamlet|
                    #{extra}
                    ^{fvInput typeV}
                    ^{fvInput nameV}
                    ^{fvInput gateV}
                    ^{fvInput descrV}
                    ^{fvInput iconV}
                    |]
    return (r,w)
  where
      types = [(MsgPayNow,PayNow),(MsgPayAtVenue,PayAtVenue)]
      gates = [(MsgStripe,PayGatewayStripe),(MsgYooKassa,PayGatewayYookassa)]

      uniqueNameField :: WorkspaceId -> FormResult PayMethod -> Field Handler Text
      uniqueNameField wid' typ = checkM (uniqueName wid' typ) md3textField

      uniqueName :: WorkspaceId -> FormResult PayMethod -> Text -> Handler (Either AppMessage Text)
      uniqueName wid' typ name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @PayOption
              where_ $ x ^. PayOptionWorkspace ==. val wid'
              case typ of
                FormSuccess typ' -> where_ $ x ^. PayOptionType ==. val typ'
                _otherwise -> where_ $ val True
              where_ $ x ^. PayOptionName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity oid _) -> case option of
              Nothing -> Left MsgAlreadyExists
              Just (Entity oid' _) | oid == oid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


postPayOptionR :: BusinessId -> WorkspaceId -> PayOptionId -> Handler Html
postPayOptionR bid wid oid = do

    option <- runDB $ selectOne $ do
        x <- from $ table @PayOption
        where_ $ x ^. PayOptionId ==. val oid
        return x
        
    ((fr,fw),et) <- runFormPost $ formPayOption wid option
    case fr of
      FormSuccess r -> do
          runDB $ replace oid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ PayOptionR bid wid oid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idFormPayOption <- newIdent
              $(widgetFile "data/business/workspaces/pay/edit")


getPayOptionR :: BusinessId -> WorkspaceId -> PayOptionId -> Handler Html
getPayOptionR bid wid oid = do

    option <- runDB $ selectOne $ do
        x :& w :& b <- from $ table @PayOption
            `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. PayOptionWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)
        where_ $ x ^. PayOptionId ==. val oid
        return (x,w,b)

    (fw2,et2) <- generateFormPost formPayOptionDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentOption
        $(widgetFile "data/business/workspaces/pay/option")


postPayOptionsR :: BusinessId -> WorkspaceId -> Handler Html
postPayOptionsR bid wid = do
    ((fr,fw),et) <- runFormPost $ formPayOption wid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ PayOptionsR bid wid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idFormPayOption <- newIdent
              $(widgetFile "data/business/workspaces/pay/new")


getPayOptionsR :: BusinessId -> WorkspaceId -> Handler Html
getPayOptionsR bid wid = do

    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    
    options <- runDB $ select $ do
        x <- from $ table @PayOption
        where_ $ x ^. PayOptionWorkspace ==. val wid
        orderBy [asc (x ^. PayOptionId)]
        return x
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentOptions
        idTabPayOptions <- newIdent
        idPanelPayOptions <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/business/workspaces/pay/options")


postDataWorkingSlotDeleR :: BusinessId -> WorkspaceId -> Day -> WorkingHoursId -> Handler Html
postDataWorkingSlotDeleR bid wid day sid = do
    
    ((fr,_),_) <- runFormPost formWorkingSlotDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete bid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ DataWorkingSlotsR bid wid day
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ DataWorkingSlotR bid wid day sid


postDataWorkingSlotR :: BusinessId -> WorkspaceId -> Day -> WorkingHoursId -> Handler Html
postDataWorkingSlotR bid wid day sid = do
    
    slot <- runDB $ selectOne $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formWorkSlot wid day slot

    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DataWorkingSlotsR bid wid day
      _otherwise -> do    
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkingHours
              idFormWorkSlot <- newIdent
              $(widgetFile "data/business/workspaces/hours/slots/edit")


getDataWorkingSlotEditR :: BusinessId -> WorkspaceId -> Day -> WorkingHoursId -> Handler Html
getDataWorkingSlotEditR bid wid day sid = do
    
    slot <- runDB $ selectOne $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursId ==. val sid
        return x
    
    (fw,et) <- generateFormPost $ formWorkSlot wid day slot
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idFormWorkSlot <- newIdent
        $(widgetFile "data/business/workspaces/hours/slots/edit")


postDataWorkingSlotsR :: BusinessId -> WorkspaceId -> Day -> Handler Html
postDataWorkingSlotsR bid wid day = do

    ((fr,fw),et) <- runFormPost $ formWorkSlot wid day Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR $ DataWorkingSlotsR bid wid day
      _otherwise -> do    
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkingHours
              idFormWorkSlot <- newIdent
              $(widgetFile "data/business/workspaces/hours/slots/new")


getDataWorkingSlotNewR :: BusinessId -> WorkspaceId -> Day -> Handler Html
getDataWorkingSlotNewR bid wid day = do

    (fw,et) <- generateFormPost $ formWorkSlot wid day Nothing
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idFormWorkSlot <- newIdent
        $(widgetFile "data/business/workspaces/hours/slots/new")


formWorkSlot :: WorkspaceId -> Day -> Maybe (Entity WorkingHours) -> Form WorkingHours
formWorkSlot wid day slot extra = do

    msgr <- getMessageRender
    
    (startR,startV) <- md3mreq md3timeField FieldSettings
        { fsLabel = SomeMessage MsgStartTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgStartTime)]
        } (workingHoursStart . entityVal <$> slot)
        
    (endR,endV) <- md3mreq md3timeField FieldSettings
        { fsLabel = SomeMessage MsgEndTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgEndTime)]
        } (workingHoursEnd . entityVal <$> slot)

    let r = WorkingHours wid day <$> startR <*> endR
    let w = [whamlet|#{extra} ^{fvInput startV} ^{fvInput endV}|]
    return (r,w)


getDataWorkingSlotR :: BusinessId -> WorkspaceId -> Day -> WorkingHoursId -> Handler Html
getDataWorkingSlotR bid wid day sid = do
    
    slot <- runDB $ selectOne $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursId ==. val sid
        return x

    (fw2,et2) <- generateFormPost formWorkingSlotDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        $(widgetFile "data/business/workspaces/hours/slots/slot")


formWorkingSlotDelete :: Form ()
formWorkingSlotDelete extra = return (pure (), [whamlet|#{extra}|]) 


getDataWorkingSlotsR :: BusinessId -> WorkspaceId -> Day -> Handler Html
getDataWorkingSlotsR bid wid day = do
    
    stati <- reqGetParams <$> getRequest
    
    slots <- runDB $ select $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursWorkspace ==. val wid
        where_ $ x ^. WorkingHoursDay ==. val day
        return x

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idFabAdd <- newIdent
        $(widgetFile "data/business/workspaces/hours/slots/slots")


getDataWorkingHoursR :: BusinessId -> WorkspaceId -> Month -> Handler Html
getDataWorkingHoursR bid wid month = do

    stati <- reqGetParams <$> getRequest

    let mapper (Entity _ (WorkingHours _ day start end)) = (day,diffLocalTime (LocalTime day end) (LocalTime day start))
    
    hours <- groupByKey mapper <$> runDB ( select $ do
        x <- from $ table @WorkingHours
        where_ $ x ^. WorkingHoursWorkspace ==. val wid
        where_ $ x ^. WorkingHoursDay `between` (val $ periodFirstDay month, val $ periodLastDay month)
        return x ) :: Handler (M.Map Day NominalDiffTime)

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        idTabWorkingHours <- newIdent
        idPanelWorkingHours <- newIdent
        idCalendarPage <- newIdent
        idFabAdd <- newIdent 
        $(widgetFile "data/business/workspaces/hours/hours")
  where
      
      groupByKey :: (Ord k, Num a) => (v -> (k,a)) -> [v] -> M.Map k a
      groupByKey key = M.fromListWith (+) . fmap key

      rest diff = mod (div (truncate @_ @Integer $ nominalDiffTimeToSeconds diff) 60) 60


postDataWorkspaceDeleR :: BusinessId -> WorkspaceId -> Handler Html
postDataWorkspaceDeleR bid wid = do
    
    ((fr,_),_) <- runFormPost formWorkspaceDelete
    
    case fr of
      FormSuccess () -> do
          runDB $ delete wid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ DataWorkspacesR bid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ DataWorkspaceR bid wid


postDataWorkspaceR :: BusinessId -> WorkspaceId -> Handler Html
postDataWorkspaceR bid wid = do

    workspace <- runDB $ selectOne $ do
        x <- from $ table @Workspace
        where_ $ x ^. WorkspaceId ==. val wid
        return x

    ((fr,fw),et) <- runFormPost $ formWorkspace bid workspace

    case fr of
      FormSuccess r -> do
          runDB $ replace wid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DataWorkspaceR bid wid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkspace
              idFormWorkspace <- newIdent
              $(widgetFile "data/business/workspaces/edit")


getDataWorkspaceEditR :: BusinessId -> WorkspaceId -> Handler Html
getDataWorkspaceEditR bid wid = do

    workspace <- runDB $ selectOne $ do
        x <- from $ table @Workspace
        where_ $ x ^. WorkspaceId ==. val wid
        return x

    (fw,et) <- generateFormPost $ formWorkspace bid workspace
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idFormWorkspace <- newIdent
        $(widgetFile "data/business/workspaces/edit")


getDataWorkspaceR :: BusinessId -> WorkspaceId -> Handler Html
getDataWorkspaceR bid wid = do

    workspace <- runDB $ selectOne $ do
        x :& b :& o <- from $ table @Workspace
            `innerJoin` table @Business `on` (\(x :& b) -> x ^. WorkspaceBusiness ==. b ^. BusinessId)
            `innerJoin` table @User `on` (\(_ :& b :& o) -> b ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. WorkspaceId ==. val wid
        return (x,b,o)

    (fw2,et2) <- generateFormPost formWorkspaceDelete
    
    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idTabDetails <- newIdent
        idPanelDetails <- newIdent
        $(widgetFile "data/business/workspaces/workspace")
        

formWorkspaceDelete :: Form ()
formWorkspaceDelete extra = return (pure (), [whamlet|#{extra}|])


postDataWorkspacesR :: BusinessId -> Handler Html
postDataWorkspacesR bid = do

    ((fr,fw),et) <- runFormPost $ formWorkspace bid Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DataWorkspacesR bid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgWorkspace
              idFormWorkspace <- newIdent
              $(widgetFile "data/business/workspaces/new")


getDataWorkspaceNewR :: BusinessId -> Handler Html
getDataWorkspaceNewR bid = do

    (fw,et) <- generateFormPost $ formWorkspace bid Nothing   
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspace
        idFormWorkspace <- newIdent
        $(widgetFile "data/business/workspaces/new")


formWorkspace :: BusinessId -> Maybe (Entity Workspace) -> Form Workspace
formWorkspace bid workspace extra = do

    msgr <- getMessageRender
    
    (nameR, nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (workspaceName . entityVal <$> workspace)
        
    (addrR, addrV) <- md3mreq md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAddress)]
        } (workspaceAddress . entityVal <$> workspace)
        
    (tzoR, tzoV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgTimeZone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTimeZone)]
        } (pack . show . workspaceTzo . entityVal <$> workspace)
        
    (currencyR, currencyV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgCurrency
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgCurrency)]
        } (workspaceCurrency . entityVal <$> workspace)

    return ( Workspace bid <$> nameR <*> addrR <*> (read . unpack <$> tzoR) <*> currencyR
           , [whamlet|#{extra} ^{fvInput nameV} ^{fvInput addrV} ^{fvInput tzoV} ^{fvInput currencyV}|]
           )
  where
      
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Workspace
              where_ $ x ^. WorkspaceBusiness ==. val bid
              where_ $ x ^. WorkspaceName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity wid _) -> case workspace of
              Nothing -> Left MsgAlreadyExists
              Just (Entity wid' _) | wid == wid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getDataWorkspacesR :: BusinessId -> Handler Html
getDataWorkspacesR bid = do

    workspaces <- runDB $ select $ do
        x <- from $ table @Workspace
        where_ $ x ^. WorkspaceBusiness ==. val bid
        orderBy [desc (x ^. WorkspaceId)]
        return x
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkspaces
        idTabWorkspaces <- newIdent
        idPanelWorkspaces <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/business/workspaces/workspaces")


postDataBusinessDeleR :: BusinessId -> Handler Html
postDataBusinessDeleR bid = do
    ((fr,_),_) <- runFormPost formBusinessDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete bid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR DataBusinessesR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ DataBusinessR bid


postDataBusinessR :: BusinessId -> Handler Html
postDataBusinessR bid = do
    
    business <- runDB $ selectOne $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessId ==. val bid
        return x

    ((fr,fw),et) <- runFormPost $ formBusiness business

    case fr of
      FormSuccess r -> do
          runDB $ replace bid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DataBusinessR bid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idFormBusiness <- newIdent
              $(widgetFile "data/business/edit")


getDataBusinessEditR :: BusinessId -> Handler Html
getDataBusinessEditR bid = do
    
    business <- runDB $ selectOne $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessId ==. val bid
        return x

    (fw,et) <- generateFormPost $ formBusiness business
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idFormBusiness <- newIdent
        $(widgetFile "data/business/edit")


getDataBusinessR :: BusinessId -> Handler Html
getDataBusinessR bid = do
    
    business <- runDB $ selectOne $ do
        x :& o <- from $ table @Business
            `innerJoin` table @User `on` (\(x :& o) -> x ^. BusinessOwner ==. o ^. UserId)
        where_ $ x ^. BusinessId ==. val bid
        return (x,o)

    (fw2,et2) <- generateFormPost formBusinessDelete
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idTabDetails <- newIdent
        idPanelDetails <- newIdent
        $(widgetFile "data/business/business")


formBusinessDelete :: Form ()
formBusinessDelete extra = return (pure (), [whamlet|#{extra}|])


postDataBusinessesR :: Handler Html
postDataBusinessesR = do
    ((fr,fw),et) <- runFormPost $ formBusiness Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR DataBusinessesR
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBusiness
              idFormBusiness <- newIdent
              $(widgetFile "data/business/new")


getDataBusinessNewR :: Handler Html
getDataBusinessNewR = do
    (fw,et) <- generateFormPost $ formBusiness Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusiness
        idFormBusiness <- newIdent
        $(widgetFile "data/business/new")


formBusiness :: Maybe (Entity Business) -> Form Business
formBusiness business extra = do
    
    owners <- liftHandler $ runDB ( select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserName)]
        return x )
    
    msgr <- getMessageRender
    
    (ownerR, ownerV) <- md3mreq (md3selectField (optionsPairs (options <$> owners))) FieldSettings
        { fsLabel = SomeMessage MsgOwner
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgOwner)]
        } (businessOwner . entityVal <$> business)
        
    (nameR, nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (businessName . entityVal <$> business)

    return ( Business <$> ownerR <*> nameR
           , $(widgetFile "data/business/form")
           )
  where
      options e = (fromMaybe (userEmail . entityVal $ e) (userName . entityVal $ e), entityKey e)
      
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Business
              where_ $ x ^. BusinessName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity bid _) -> case business of
              Nothing -> Left MsgAlreadyExists
              Just (Entity bid' _) | bid == bid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getDataBusinessesR :: Handler Html
getDataBusinessesR = do
    
    businesses <- runDB $ select $ do
        x <- from $ table @Business
        orderBy [desc (x ^. BusinessId)]
        return x
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusinesses
        idFabAdd <- newIdent
        $(widgetFile "data/business/businesses")
