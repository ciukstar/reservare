{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Handler.Appointments
  ( getAppointmentStaffR, postAppointmentStaffR
  , getAppointmentTimingR, postAppointmentTimingR
  , getAppointmentTimeSlotsR, postAppointmentTimeSlotsR
  , getAppointmentPaymentR, postAppointmentPaymentR
  ) where

import qualified AtVenue.Data as V (Route(CheckoutR))

import Control.Monad (when, unless, forM_)

import Data.Bifunctor (Bifunctor(first,bimap))
import Data.Foldable (find)
import qualified Data.Map as M (member, Map, fromListWith)
import Data.Maybe (fromMaybe)
import Data.Text (pack, Text, unpack)
import Data.Time
    ( UTCTime (utctDay), weekFirstDay, DayOfWeek (Monday)
    , DayPeriod (periodFirstDay, periodLastDay), addDays, toGregorian
    , Day, LocalTime (LocalTime, localDay)
    , addLocalTime, secondsToNominalDiffTime, getCurrentTime
    )
import Data.Time.Calendar.Month (addMonths, pattern YearMonth, Month)
import Data.Time.LocalTime (utcToLocalTime, utc, localTimeToUTC)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, innerJoin, on, in_
    , (^.), (==.), (:&)((:&))
    , toSqlKey, val, where_, selectOne, Value (unValue), between, valList
    , distinct, subSelectList, just
    )
import Database.Persist (Entity (Entity), entityKey, insert)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, App
    , Route
      ( AuthR, HomeR
      , AppointmentStaffR, AppointmentTimingR, AppointmentTimeSlotsR
      , AppointmentPaymentR, StripeR, YookassaR, AtVenueR
      )
    , AppMessage
      ( MsgServices, MsgNext, MsgServices, MsgThereAreNoDataYet
      , MsgBack, MsgStaff, MsgBusiness, MsgWorkspace, MsgAppointmentTime
      , MsgPaymentOption, MsgCancel, MsgService, MsgEmployee, MsgSelect
      , MsgSelectTime, MsgPaymentGatewayNotSpecified, MsgRole
      , MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgAppointmentSetFor, MsgSelectAvailableDayAndTimePlease
      , MsgEmployeeScheduleNotGeneratedYet, MsgNoEmployeesAvailableNow
      , MsgPrevious, MsgInvalidFormData, MsgServiceAssignment
      , MsgEmployeeWorkScheduleForThisMonthNotSetYet      
      )
    )

import Material3 (md3mreq)

import Model
    ( statusError
    , ServiceId, Service(Service)
    , WorkspaceId, Workspace (Workspace)
    , BusinessId, Business (Business)
    , AssignmentId, Assignment (Assignment)
    , StaffId, Staff (Staff)
    , PayMethod (PayNow, PayAtVenue)
    , Schedule (Schedule)
    , PayOptionId, PayOption (PayOption)
    , Book
      ( Book, bookService, bookStaff, bookAppointment, bookCustomer
      , bookCharge, bookCurrency
      )
    , PayGateway (PayGatewayStripe, PayGatewayYookassa)
    , EntityField
      ( ServiceWorkspace, WorkspaceId, WorkspaceBusiness
      , BusinessId, ServiceName, AssignmentStaff, StaffId, StaffName
      , BusinessName, WorkspaceName, ServiceId, AssignmentService
      , AssignmentSlotInterval, ScheduleAssignment, AssignmentId
      , ScheduleDay, PayOptionId, PayOptionWorkspace, AssignmentRole
      , ServiceAvailable, ServicePrice, WorkspaceCurrency
      )
    )

import Settings (widgetFile)
    
import qualified Stripe.Data as S (Route(CheckoutR))

import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (RenderMessage)

import Widgets (widgetSnackbar, widgetBanner)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core.Handler
    ( getMessages, newIdent, addMessageI
    , getRequest, YesodRequest (reqGetParams), redirect
    , getMessageRender, setUltDestCurrent, setUltDest
    )
import Yesod.Core
    ( Yesod(defaultLayout), MonadIO (liftIO), whamlet
    , MonadHandler (liftHandler), handlerToWidget
    , SomeMessage (SomeMessage)
    )
import Yesod.Core.Types (HandlerFor)
import Yesod.Core.Widget (setTitleI, toWidget)
import Yesod.Form
    ( FieldView(fvInput), Field (fieldView)
    , FormResult (FormSuccess, FormFailure, FormMissing)    
    , FieldSettings (fsLabel, fsTooltip, fsId, fsName, fsAttrs, FieldSettings)
    , Option (optionDisplay), checkboxesFieldList, runFormGet, mopt
    )
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields
    ( textField, intField, radioField, optionsPairs, OptionList (olOptions)
    , Option (optionExternalValue, optionInternalValue), datetimeLocalField
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))

import qualified Yookassa.Data as Y (Route(CheckoutR))


postAppointmentPaymentR :: Handler Html
postAppointmentPaymentR = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )
    oid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "oid" )

    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    let month = maybe today ((\(y,m,_) -> YearMonth y m) . toGregorian . utctDay) tid

    ((fr,fw),et) <- runFormPost $ formPayment aid eid sid tid oid

    user <- maybeAuth
    
    case (fr,user) of
      (FormSuccess (_,eid',sid',tid',oid'), Nothing) -> do
          setUltDest ( AppointmentPaymentR, [ ("sid", pack $ show $ fromSqlKey sid')
                                            , ("eid", pack $ show $ fromSqlKey eid')
                                            , ("tid", pack $ show (utcToLocalTime utc tid'))
                                            , ("oid", pack $ show oid')
                                            ]
                     )
          redirect $ AuthR LoginR
          
      (FormSuccess (_,eid',sid',tid',oid'), Just (Entity uid _)) -> do

          charge' <- (bimap unValue unValue <$>) <$> runDB ( selectOne $ do
              x :& w <- from $ table @Service
                  `innerJoin` table @Workspace `on` (\(x :& w) -> x ^. ServiceWorkspace ==. w ^. WorkspaceId)
              where_ $ x ^. ServiceId ==. val sid'
              return (x ^. ServicePrice, w ^. WorkspaceCurrency) )
          
          option' <- runDB $ selectOne $ do
              x <- from $ table @PayOption
              where_ $ x ^. PayOptionId ==. val oid'
              return x
          
          case (charge',option') of
            (Just (charge,currency),Just (Entity _ (PayOption _ PayNow _ (Just PayGatewayStripe) _ _))) -> do
                bid <- runDB $ insert $ Book { bookCustomer = uid
                                             , bookService = sid'
                                             , bookStaff = eid'
                                             , bookAppointment = tid'
                                             , bookCharge = charge
                                             , bookCurrency = currency
                                             }
                setUltDestCurrent
                redirect ( StripeR $ S.CheckoutR bid oid', [ ("sid", pack $ show $ fromSqlKey sid')
                                                           , ("eid", pack $ show $ fromSqlKey eid')
                                                           , ("tid", pack $ show (utcToLocalTime utc tid'))
                                                           , ("oid", pack $ show $ fromSqlKey oid')
                                                           ]
                         )
                    
            (Just (charge,currency),Just (Entity _ (PayOption _ PayNow _ (Just PayGatewayYookassa) _ _))) -> do
                bid <- runDB $ insert $ Book { bookCustomer = uid
                                             , bookService = sid'
                                             , bookStaff = eid'
                                             , bookAppointment = tid'
                                             , bookCharge = charge
                                             , bookCurrency = currency
                                             }
                redirect ( YookassaR $ Y.CheckoutR bid oid', [ ("sid", pack $ show $ fromSqlKey sid')
                                                             , ("eid", pack $ show $ fromSqlKey eid')
                                                             , ("tid", pack $ show (utcToLocalTime utc tid'))
                                                             , ("oid", pack $ show $ fromSqlKey oid')
                                                             ]
                         )
                    
            (Just (charge,currency),Just (Entity _ (PayOption _ PayAtVenue _ _ _ _))) -> do
                bid <- runDB $ insert $ Book { bookCustomer = uid
                                             , bookService = sid'
                                             , bookStaff = eid'
                                             , bookAppointment = tid'
                                             , bookCharge = charge
                                             , bookCurrency = currency
                                             }
                redirect (AtVenueR $ V.CheckoutR bid oid')
                
            (_,Just (Entity _ (PayOption _ PayNow _ Nothing _ _))) -> do
                addMessageI statusError MsgPaymentGatewayNotSpecified
                msgs <- getMessages
                defaultLayout $ do
                    setTitleI MsgPaymentOption
                    idFormPayment <- newIdent
                    idFabNext <- newIdent
                    $(widgetFile "appointments/payment/payment")
                
            _otherwise -> do
                addMessageI statusError MsgInvalidFormData
                msgs <- getMessages
                defaultLayout $ do
                    setTitleI MsgPaymentOption
                    idFormPayment <- newIdent
                    idFabNext <- newIdent
                    $(widgetFile "appointments/payment/payment")
          
      (FormFailure errs, _) -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idFormPayment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/payment/payment")
              
      (FormMissing, _) -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPaymentOption
              idFormPayment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/payment/payment")


getAppointmentPaymentR :: Handler Html
getAppointmentPaymentR = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- (localTimeToUTC utc <$>) <$> runInputGet ( iopt datetimeLocalField "tid" )
    oid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "oid" )

    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    let month = maybe today ((\(y,m,_) -> YearMonth y m) . toGregorian . utctDay) tid

    (fw,et) <- generateFormPost $ formPayment aid eid sid tid oid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPaymentOption
        idFormPayment <- newIdent
        idFabNext <- newIdent
        $(widgetFile "appointments/payment/payment")


formPayment :: Maybe AssignmentId -> Maybe StaffId -> Maybe ServiceId -> Maybe UTCTime -> Maybe PayOptionId
            -> Form (AssignmentId,StaffId,ServiceId,UTCTime,PayOptionId)
formPayment aid eid sid time option extra = do

    msgr <- getMessageRender

    (assignmentR,assignmentV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgServiceAssignment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgServiceAssignment),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> aid)

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgService),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)

    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgEmployee),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> eid)

    (timeR,timeV) <- mreq datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAppointmentTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAppointmentTime),("hidden","hidden")]
        } (utcToLocalTime utc <$> time)

    options <- liftHandler $ runDB $ select $ do
        x <- from $ table @PayOption
        where_ $ x ^. PayOptionWorkspace `in_` subSelectList
            ( do
                  s <- from $ table @Service
                  where_ $ just (s ^. ServiceId) ==. val sid
                  return $ s ^. ServiceWorkspace
            )
        return x

    (optionR,optionV) <- md3mreq (md3radioFieldList options) "" option

    let r = (,,,,) <$> assignmentR <*> staffR <*> serviceR <*> (localTimeToUTC utc <$> timeR) <*> optionR
    let w = [whamlet|
                    #{extra}
                    ^{fvInput assignmentV}
                    ^{fvInput staffV}
                    ^{fvInput serviceV}
                    ^{fvInput timeV}
                    ^{fvInput optionV}
                    |]

    return (r,w)

  where

      pairs (Entity oid' (PayOption _ _ name _ _ _)) = (name, oid')
      
      md3radioFieldList :: [Entity PayOption] -> Field (HandlerFor App) PayOptionId
      md3radioFieldList options = (radioField (optionsPairs (pairs <$> options)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs <$> options))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findOption opt = find (\(Entity oid _) -> oid == optionInternalValue opt)

                [whamlet|
<md-list ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    $maybe Entity _ (PayOption _ _ oname _ descr icon) <- findOption opt options
      <md-list-item type=button onclick="this.querySelector('md-radio').click()">
        $maybe icon <- icon
          <md-icon slot=start>
            #{icon}
        <div slot=headline>
          #{oname}
        $maybe descr <- descr
          <div slot=supporting-text>
            #{descr}
        <div slot=end>
          <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
            :sel x opt:checked touch-target=wrapper>
    <md-divider>
|]
              }


postAppointmentTimeSlotsR :: Day -> Handler Html
postAppointmentTimeSlotsR day = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day

    slots <- querySlots day aid

    ((fr,fw),et) <- runFormPost $ formTimeSlot slots aid eid sid tid
    case fr of
      FormSuccess (aid',eid',sid',tid') -> redirect
          ( AppointmentTimingR month
          , [ ( "aid", pack $ show $ fromSqlKey aid')
            , ( "eid", pack $ show $ fromSqlKey eid')
            , ( "sid", pack $ show $ fromSqlKey sid')
            , ( "tid", pack $ show tid')
            ]
          )
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormSlots <- newIdent
              $(widgetFile "appointments/timing/slots/slots")
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormSlots <- newIdent
              $(widgetFile "appointments/timing/slots/slots")


getAppointmentTimeSlotsR :: Day -> Handler Html
getAppointmentTimeSlotsR day = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day

    slots <- querySlots day aid

    (fw,et) <- generateFormPost $ formTimeSlot slots aid eid sid tid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFormSlots <- newIdent
        $(widgetFile "appointments/timing/slots/slots")


querySlots :: Day -> Maybe AssignmentId -> Handler [LocalTime]
querySlots day aid = do

    interval <- maybe (secondsToNominalDiffTime (15 * 60)) unValue <$> liftHandler ( runDB $ selectOne $ do
        x <- from $ table @Assignment
        case aid of
          Just y -> where_ $ x ^. AssignmentId ==. val y
          Nothing -> where_ $ val False
        return (x ^. AssignmentSlotInterval) )

    schedule <- liftHandler $ runDB $ select $ do
        x :& a <- from $ table @Schedule
            `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
        case aid of
          Just y -> where_ $ a ^. AssignmentId ==. val y
          Nothing -> where_ $ val False
        where_ $ x ^. ScheduleDay ==. val day
        return x

    return $ concatMap
            (\(Entity _ (Schedule _ d s e)) -> let end = LocalTime d e in
                takeWhile (< end) [ addLocalTime (fromInteger i * interval) (LocalTime d s) | i <- [0..] ]
            )
            schedule


formTimeSlot :: [LocalTime] -> Maybe AssignmentId -> Maybe StaffId -> Maybe ServiceId -> Maybe LocalTime
             -> Form (AssignmentId,StaffId,ServiceId,LocalTime)
formTimeSlot slots aid eid sid tid extra = do

    msgr <- getMessageRender

    (assignmentR,assignmentV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgServiceAssignment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgServiceAssignment),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> aid)

    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgEmployee),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> eid)

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgService),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)


    (slotsR,slotsV) <- mreq (md3radioFieldList slots) "" tid

    let r = (,,,) <$> assignmentR <*> staffR <*> serviceR <*> slotsR
    let w = [whamlet|#{extra} ^{fvInput assignmentV} ^{fvInput staffV} ^{fvInput serviceV} ^{fvInput slotsV}|]

    return (r,w)

  where
      pairs xs = (\x -> (pack $ show x, x)) <$> xs

      md3radioFieldList :: [LocalTime] -> Field (HandlerFor App) LocalTime
      md3radioFieldList items = (radioField (optionsPairs . pairs $ items))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs . pairs $ items)

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                [whamlet|
<md-list ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    <md-list-item type=button onclick="this.querySelector('md-radio').click()">
      $with dt <- optionDisplay opt
        <time.time-slot slot=headline datetime=#{dt}>
          #{optionDisplay opt}
      <div slot=end>
        <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
          :sel x opt:checked touch-target=wrapper>
    <md-divider>
|]
              }


groupByKey :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupByKey f = M.fromListWith (<>) . fmap (\x -> (f x,[x]))


formSearchAssignments :: Text -> Form ( Maybe [BusinessId]
                                      , Maybe [WorkspaceId]
                                      , Maybe [ServiceId]
                                      , Maybe [Text]
                                      )
formSearchAssignments idFormSearch extra = do

    msgr <- getMessageRender

    businesses <- liftHandler $ runDB $ select $ do
        x <- from $ table @Business
        orderBy [asc (x ^. BusinessName)]
        return x

    let businessItem (Entity bid (Business _ name)) = (name,bid)

    (bR,bV) <- mopt (chipsFieldList idFormSearch MsgBusiness (businessItem <$> businesses))
        FieldSettings { fsLabel = SomeMessage MsgBusiness
                      , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
                      , fsAttrs = [("label", msgr MsgBusiness),("hidden","hidden")]
                      } Nothing

    workspaces <- liftHandler $ runDB $ select $ do
        x <- from $ table @Workspace
        unless (null businesses) $ where_ $ x ^. WorkspaceBusiness `in_` valList (entityKey <$> businesses)
        when (null businesses) $ where_ $ val False
        case bR of
          FormSuccess (Just bids@(_:_)) -> where_ $ x ^. WorkspaceBusiness `in_` valList bids
          _otherwise -> where_ $ val False
        orderBy [asc (x ^. WorkspaceName)]
        return x

    let workspaceItem (Entity wid (Workspace _ name _ _ _)) = (name,wid)

    (wR,wV) <- mopt (chipsFieldList idFormSearch MsgWorkspace (workspaceItem <$> workspaces))
        FieldSettings { fsLabel = SomeMessage MsgWorkspace
                      , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
                      , fsAttrs = [("label", msgr MsgWorkspace),("hidden","hidden")]
                      } Nothing

    services <- liftHandler $ runDB $ select $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceAvailable ==. val True
        unless (null workspaces) $ where_ $ x ^. ServiceWorkspace `in_` valList (entityKey <$> workspaces)
        when (null workspaces) $ where_ $ val False
        case wR of
          FormSuccess (Just wids@(_:_)) -> where_ $ x ^. ServiceWorkspace `in_` valList wids
          _otherwise -> where_ $ val False
        orderBy [asc (x ^. ServiceName)]
        return x

    let serviceItem (Entity sid (Service _ name _ _ _ _)) = (name,sid)

    (sR,sV) <- mopt (chipsFieldList idFormSearch MsgService (serviceItem <$> services))
        FieldSettings { fsLabel = SomeMessage MsgService
                      , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
                      , fsAttrs = [("label", msgr MsgService),("hidden","hidden")]
                      } Nothing

    roles <- liftHandler $ (unValue <$>) <$> runDB ( select $ distinct $ do
        x <- from $ table @Assignment
        unless (null services) $ where_ $ x ^. AssignmentService `in_` valList (entityKey <$> services)
        when (null services) $ where_ $ val False
        case sR of
          FormSuccess (Just sids@(_:_)) -> where_ $ x ^. AssignmentService `in_` valList sids
          _otherwise -> where_ $ val False
        orderBy [asc (x ^. AssignmentRole)]
        return $ x ^. AssignmentRole )

    (rR,rV) <- mopt (chipsFieldList idFormSearch MsgRole ((\x -> (x,x)) <$> roles))
        FieldSettings { fsLabel = SomeMessage MsgRole
                      , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
                      , fsAttrs = [("label", msgr MsgRole),("hidden","hidden")]
                      } Nothing

    let r = (,,,) <$> bR <*> wR <*> sR <*> rR
    let w = do
            idFilterChips <- newIdent
            toWidget [cassius|
                             ##{idFilterChips}
                                 display: flex
                                 flex-direction: column

                                 details
                                     summary
                                         position: relative
                                         padding: 1rem
                                         display: flex
                                         align-items: center
                                         justify-content: flex-start

                                     summary::before
                                         margin-right: 0.5rem
                                         content: 'filter_alt'
                                         font-family: 'Material Symbols Outlined'
                                         font-size: 24px

                                     summary::after
                                         margin-left: auto
                                         content: 'arrow_drop_down'
                                         font-family: 'Material Symbols Outlined'
                                         font-size: 32px
                                         transition: 0.2s

                                     p
                                         padding: 0 1rem

                                 details[open] > summary::after
                                     transform: rotate(180deg)

            |]
            [whamlet|
                    <div ##{idFilterChips}>
                      #{extra}
                      ^{fvInput bV}
                      $case bR
                        $of FormSuccess (Just ((:) _ _))
                          <md-divider>
                          ^{fvInput wV}
                        $of _
                      $case wR
                        $of FormSuccess (Just ((:) _ _))
                          <md-divider>
                          ^{fvInput sV}
                        $of _
                      $case sR
                        $of FormSuccess (Just ((:) _ _))
                          <md-divider>
                          ^{fvInput rV}
                        $of _
            |]

    return (r,w)
  where
      chipsFieldList :: (Eq a, RenderMessage App msg) => Text -> AppMessage -> [(msg,a)] -> Field Handler [a]
      chipsFieldList idForm label items = (checkboxesFieldList items)
          { fieldView = \theId name attrs x _isReq -> do
                opts <- olOptions <$> handlerToWidget (optionsPairs items)
                let sele (Left _) _ = False
                    sele (Right vals) opt = optionInternalValue opt `elem` vals
                [whamlet|
                  <details :any (sele x) opts:open>
                    <summary.md-typescale-label-large #summary#{theId}>
                      <md-ripple for=summary#{theId}>
                      _{label}
                    <p>
                      <md-chip-set ##{theId}>
                        $forall opt <- opts
                          <md-filter-chip label=#{optionDisplay opt} :sele x opt:selected
                            onclick="this.querySelector('input').click();document.getElementById('#{idForm}').submit()">
                            <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :sele x opt:checked>
                |]
          }


postAppointmentTimingR :: Month -> Handler Html
postAppointmentTimingR month = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    ((fr,fw),et) <- runFormPost $ formTiming aid eid sid tid

    case fr of
      FormSuccess (aid',eid',sid',tid') -> redirect ( AppointmentPaymentR
                                                    , [ ( "aid", pack $ show $ fromSqlKey aid' )
                                                      , ( "eid", pack $ show $ fromSqlKey eid' )
                                                      , ( "sid", pack $ show $ fromSqlKey sid' )
                                                      , ( "tid", pack $ show tid' )
                                                      ]
                                                    )
      FormFailure errs -> do

          let start = weekFirstDay Monday (periodFirstDay month)
          let end = addDays 41 start
          let page = [start .. end]
          let next = addMonths 1 month
          let prev = addMonths (-1) month

          today <- utctDay <$> liftIO getCurrentTime

          slots <- groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$> runDB ( select $ do
              x :& a <- from $ table @Schedule
                  `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
              case aid of
                Just assignment -> where_ $ a ^. AssignmentId ==. val assignment
                Nothing -> where_ $ val False
              where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
              return x )

          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormTiming <- newIdent
              idCalendarPage <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/timing/timing")
              
      FormMissing -> do

          let start = weekFirstDay Monday (periodFirstDay month)
          let end = addDays 41 start
          let page = [start .. end]
          let next = addMonths 1 month
          let prev = addMonths (-1) month

          today <- utctDay <$> liftIO getCurrentTime

          slots <- groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$> runDB ( select $ do
              x :& a <- from $ table @Schedule
                  `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
              case aid of
                Just assignment -> where_ $ a ^. AssignmentId ==. val assignment
                Nothing -> where_ $ val False
              where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
              return x )

          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              idFormTiming <- newIdent
              idCalendarPage <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/timing/timing")


getAppointmentTimingR :: Month -> Handler Html
getAppointmentTimingR month = do
    stati <- reqGetParams <$> getRequest
    eid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "eid" )
    sid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "sid" )
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )
    tid <- runInputGet ( iopt datetimeLocalField "tid" )

    (fw,et) <- generateFormPost $ formTiming aid eid sid tid

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    today <- utctDay <$> liftIO getCurrentTime

    slots <- groupByKey (\(Entity _ (Schedule _ day _ _)) -> day) <$> runDB ( select $ do
        x :& a <- from $ table @Schedule
            `innerJoin` table @Assignment `on` (\(x :& a) -> x ^. ScheduleAssignment ==. a ^. AssignmentId)
        case aid of
          Just assignment -> where_ $ a ^. AssignmentId ==. val assignment
          Nothing -> where_ $ val False
        where_ $ x ^. ScheduleDay `between` (val today, val $ periodLastDay month)
        return x )

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        idFormTiming <- newIdent
        idCalendarPage <- newIdent
        idFabNext <- newIdent
        $(widgetFile "appointments/timing/timing")


formTiming :: Maybe AssignmentId -> Maybe StaffId -> Maybe ServiceId -> Maybe LocalTime
           -> Form (AssignmentId, StaffId, ServiceId,LocalTime)
formTiming aid eid sid time extra = do

    msgr <- getMessageRender

    (staffR,staffV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgEmployee),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> eid)

    (serviceR,serviceV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgService),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> sid)

    (assignmentR,assignmentV) <- first (toSqlKey . fromIntegral @Integer <$>) <$> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgServiceAssignment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgServiceAssignment),("hidden","hidden")]
        } (fromIntegral . fromSqlKey <$> aid)

    (timeR,timeV) <- mreq datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAppointmentTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgAppointmentTime),("hidden","hidden")]
        } time

    let r = (,,,) <$> assignmentR <*> staffR <*> serviceR <*> timeR
    let w = [whamlet|#{extra} ^{fvInput staffV} ^{fvInput serviceV} ^{fvInput assignmentV} ^{fvInput timeV}|]

    return (r,w)


postAppointmentStaffR :: Handler Html
postAppointmentStaffR = do
    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )

    idFormSearch <- newIdent

    ((fr0,fw0),et0) <- runFormGet $ formSearchAssignments idFormSearch

    let (bids,wids,sids,roles) = case fr0 of
          FormSuccess r -> r
          _otherwise -> (Just [],Just [],Just [],Just [])

    ((fr,fw),et) <- runFormPost $ formAssignments bids wids sids roles aid

    case fr of
      FormSuccess (aid',(eid',sid')) -> do
          today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
          month <- fromMaybe today . (readMaybe . unpack =<<) <$> runInputGet ( iopt textField "month" )
          redirect (AppointmentTimingR month, [ ("aid",pack $ show $ fromSqlKey aid')
                                              , ("eid",pack $ show $ fromSqlKey eid')
                                              , ("sid",pack $ show $ fromSqlKey sid')
                                              ])
      FormFailure errs -> do
          forM_ errs $ \err -> addMessageI statusError err
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormAssignment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/assignments")
      FormMissing -> do
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgServices
              idFormAssignment <- newIdent
              idFabNext <- newIdent
              $(widgetFile "appointments/assignments")


getAppointmentStaffR :: Handler Html
getAppointmentStaffR = do

    stati <- reqGetParams <$> getRequest
    aid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "aid" )

    idFormSearch <- newIdent

    ((fr0,fw0),et0) <- runFormGet $ formSearchAssignments idFormSearch

    let (bids,wids,sids,roles) = case fr0 of
          FormSuccess r -> r
          _otherwise -> (Just [],Just [],Just [],Just [])

    (fw,et) <- generateFormPost $ formAssignments bids wids sids roles aid

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        idFormAssignment <- newIdent
        idFabNext <- newIdent
        $(widgetFile "appointments/assignments")


formAssignments :: Maybe [BusinessId] -> Maybe [WorkspaceId] -> Maybe [ServiceId] -> Maybe [Text]
                -> Maybe AssignmentId -> Form (AssignmentId,(StaffId,ServiceId))
formAssignments bids wids sids roles aid extra = do

    assignments <- liftHandler $ runDB $ select $ do
        x :& e :& s :& w :& b <- from $ table @Assignment
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. AssignmentStaff ==. e ^. StaffId)
            `innerJoin` table @Service `on` (\(x :& _ :& s) -> x ^. AssignmentService ==. s ^. ServiceId)
            `innerJoin` table @Workspace `on` (\(_ :& _ :& s :& w) -> s ^. ServiceWorkspace ==. w ^. WorkspaceId)
            `innerJoin` table @Business `on` (\(_ :& _ :& _ :& w :& b) -> w ^. WorkspaceBusiness ==. b ^. BusinessId)

        where_ $ s ^. ServiceAvailable ==. val True
        
        case roles of
          Just xs@(_:_) -> where_ $ x ^. AssignmentRole `in_` valList xs
          _otherwise -> where_ $ val True
        case sids of
          Just xs@(_:_) -> where_ $ s ^. ServiceId `in_` valList xs
          _otherwise -> where_ $ val True
        case wids of
          Just xs@(_:_) -> where_ $ w ^. WorkspaceId `in_` valList xs
          _otherwise -> where_ $ val True
        case bids of
          Just xs@(_:_) -> where_ $ b ^. BusinessId `in_` valList xs
          _otherwise -> where_ $ val True
        orderBy [asc (e ^. StaffName), asc (e ^. StaffId)]
        return (x,(e,(s,(w,b))))

    let getDefault :: Maybe AssignmentId
                   -> [(Entity Assignment,(Entity Staff,(Entity Service,(Entity Workspace,Entity Business))))]
                   -> Maybe (AssignmentId,(StaffId,ServiceId))
        getDefault (Just aid') assignments' =
            ((\(Entity aid'' _,(Entity eid'' _,(Entity sid'' _,_))) -> (aid'',(eid'',sid''))) <$>)
               . find (\(Entity aid'' _,(_,_)) -> aid'' == aid') $ assignments'
        getDefault Nothing _ = Nothing

    (assigmentR,assigmentV) <- mreq (md3radioFieldList assignments) "" (getDefault aid assignments)

    let r = assigmentR
    let w = [whamlet|#{extra} ^{fvInput assigmentV}|]

    return (r,w)

  where
      
      pairs assignments = (\(Entity aid' _,(Entity eid' (Staff name _ _ _),(Entity sid' _,_))) -> (name, (aid',(eid',sid'))))
          <$> assignments

      md3radioFieldList :: [(Entity Assignment,(Entity Staff,(Entity Service,(Entity Workspace,Entity Business))))]
                        -> Field (HandlerFor App) (AssignmentId,(StaffId,ServiceId))
      md3radioFieldList assignments = (radioField (optionsPairs (pairs assignments)))
          { fieldView = \theId name attrs x isReq -> do

                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs assignments))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findAssigment opt = find (\(Entity aid' _,(_,_)) -> aid' == fst (optionInternalValue opt))

                [whamlet|
$if null opts
    <figure style="text-align:center">
      <span.on-secondary style="font-size:4rem">&varnothing;
      <figcaption.md-typescale-body-large>
        _{MsgNoEmployeesAvailableNow}.
$else
  <md-list ##{theId} *{attrs}>
    $forall (i,opt) <- opts
      $maybe (assignment,(Entity _ (Staff ename _ _ _),(service,(workspace,business)))) <- findAssigment opt assignments
        <md-list-item type=text onclick="this.querySelector('md-radio').click()">
          <div slot=headline>
            #{ename}
          <div slot=supporting-text>
            $with Entity _ (Assignment _ _ role _ _ _) <- assignment
              #{role}
            $with (Entity _ (Business _ bname),Entity _ (Workspace _ wname _ _ _)) <- (business,workspace)
              \ (#{bname} - #{wname})
          <div slot=supporting-text>
            $with (Entity _ (Workspace _ _ _ _ currency),Entity _ (Service _ name _ price _ _)) <- (workspace,service)
              #{name} (
              <span.currency data-value=#{price} data-currency=#{currency}>
                #{show price} #{currency}
              )
          <div slot=end>
            <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
              :sel x opt:checked touch-target=wrapper>
      <md-divider>
|]
              }
