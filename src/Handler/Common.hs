{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Common
  ( getFaviconR, getRobotsR, getSitemapR, getWebAppManifestR
  ) where

import Control.Monad ( Monad(return) )

import Data.Aeson (object, (.=), Value (String))
import Data.Conduit (yield)
import Data.FileEmbed (embedFile)
import Data.Function (($))
import Data.Maybe (Maybe (Nothing, Just))

import Foundation
    ( Handler
    , Route (HomeR, DocsR, StaticR)
    , AppMessage
      ( MsgAppName, MsgMetaDescription, MsgFeatures, MsgHome, MsgServiceCatalog
      , MsgService
      )
    )

import Prelude ((*))
    
import Settings.StaticFiles
    ( img_manifest_icon_144x144_svg
    , img_screenshot_1_narrow_png, img_screenshot_1_wide_png
    , img_screenshot_2_narrow_png, img_screenshot_2_wide_png
    , img_screenshot_3_narrow_png, img_screenshot_3_wide_png
    , img_screenshot_4_narrow_png, img_screenshot_4_wide_png
    )

import Yesod.Core.Content
    ( TypedContent (TypedContent), typePlain, ToContent (toContent)
    )
import Yesod.Core.Handler
    ( cacheSeconds, getUrlRender, getMessageRender, selectRep
    )
import Yesod.Core.Json (provideJson, array)
import Yesod.Sitemap
    ( sitemap, SitemapUrl (SitemapUrl), SitemapChangeFreq (Monthly)
    )


getWebAppManifestR :: Handler TypedContent
getWebAppManifestR = do
    rndr <- getUrlRender
    msgr <- getMessageRender
    selectRep $ provideJson $ object
        [ "name" .= msgr MsgAppName
        , "short_name" .= msgr MsgAppName
        , "description" .= msgr MsgMetaDescription
        , "categories" .= array [String "business"]
        , "start_url" .= rndr HomeR
        , "theme_color" .= String "#FFFFFF"
        , "background_color" .= String "#FFFFFF"
        , "display" .= String "standalone"
        , "icons" .= array [ object [ "src" .= rndr (StaticR img_manifest_icon_144x144_svg)
                                    , "type" .= String "image/svg+xml"
                                    , "sizes" .= String "144x144"
                                    , "purpose" .= String "any"
                                    ]
                           , object [ "src" .= rndr (StaticR img_manifest_icon_144x144_svg)
                                    , "type" .= String "image/svg+xml"
                                    , "sizes" .= String "144x144"
                                    , "purpose" .= String "maskable"
                                    ]
                           ]         
        , "screenshots" .= array [ object [ "src" .= rndr (StaticR img_screenshot_1_narrow_png)
                                          , "sizes" .= String "440x783"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "narrow"
                                          , "label" .= msgr MsgFeatures
                                          ]
                                 , object [ "src" .= rndr (StaticR img_screenshot_1_wide_png)
                                          , "sizes" .= String "1920x922"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "wide"
                                          , "label" .= msgr MsgFeatures
                                          ]
                                 , object [ "src" .= rndr (StaticR img_screenshot_2_narrow_png)
                                          , "sizes" .= String "440x783"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "narrow"
                                          , "label" .= msgr MsgHome
                                          ]
                                 , object [ "src" .= rndr (StaticR img_screenshot_2_wide_png)
                                          , "sizes" .= String "1920x922"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "wide"
                                          , "label" .= msgr MsgHome
                                          ]
                                 , object [ "src" .= rndr (StaticR img_screenshot_3_narrow_png)
                                          , "sizes" .= String "440x783"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "narrow"
                                          , "label" .= msgr MsgServiceCatalog
                                          ]
                                 , object [ "src" .= rndr (StaticR img_screenshot_3_wide_png)
                                          , "sizes" .= String "1920x922"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "wide"
                                          , "label" .= msgr MsgServiceCatalog
                                          ]
                                 , object [ "src" .= rndr (StaticR img_screenshot_4_narrow_png)
                                          , "sizes" .= String "440x783"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "narrow"
                                          , "label" .= msgr MsgService
                                          ]
                                 , object [ "src" .= rndr (StaticR img_screenshot_4_wide_png)
                                          , "sizes" .= String "1920x922"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "wide"
                                          , "label" .= msgr MsgService
                                          ]
                                 ]
        ]

getSitemapR :: Handler TypedContent
getSitemapR = sitemap $ do
    yield $ SitemapUrl HomeR Nothing (Just Monthly) (Just 1.0)
    yield $ SitemapUrl DocsR Nothing (Just Monthly) (Just 1.0)

    
getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")


getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
