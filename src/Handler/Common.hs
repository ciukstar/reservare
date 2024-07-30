{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Common (getFaviconR, getRobotsR, getSitemapR) where

import Control.Monad ( Monad(return) )

import Data.Conduit (yield)
import Data.FileEmbed (embedFile)
import Data.Function (($))
import Data.Maybe (Maybe (Nothing, Just))

import Prelude ((*))

import Foundation
    ( Handler
    , Route (HomeR, DocsR)
    )

import Yesod.Core.Content
    ( TypedContent (TypedContent), typePlain, ToContent (toContent) )
import Yesod.Core.Handler ( cacheSeconds )
import Yesod.Sitemap
    (sitemap, SitemapUrl (SitemapUrl), SitemapChangeFreq (Monthly))


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
