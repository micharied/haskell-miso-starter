{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Common (
    ServerRoutes,
    Page (..),
    haskellMisoComponent,
    uri404,
    uriHome,
    uriSiteCounterExample,
    uriSiteOne,
    uriSiteTwo,
 )
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Types hiding (Header)
import Network.Wai (responseLBS)
import Network.Wai.Application.Static (defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipFiles (..), def, gzip, gzipFiles)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import qualified System.IO as IO
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

import Miso hiding (run)
import Miso.String

main :: IO ()
main = do
    staticDir <- fromMaybe "./static" <$> lookupEnv "STATIC_DIR"
    IO.hPutStrLn IO.stderr $ "Running on port 3002... serving static files from: " ++ staticDir
    run 3002 $ logStdout (compress (app staticDir))
  where
    compress = gzip def{gzipFiles = GzipCompress}

app :: String -> Application
app staticDir = serve (Proxy @API) (website staticDir)
  where
    website staticDirPath =
        serveDirectoryWith (defaultWebAppSettings staticDirPath)
            :<|> serverHandlers
            :<|> pure misoManifest
            :<|> pure robotsTxt
            :<|> Tagged handle404

robotsTxt :: Text
robotsTxt =
    T.unlines
        [ "# www.robotstxt.org/"
        , ""
        , "# Allow crawling of all content"
        , "User-agent: *"
        , "Disallow:"
        ]

-- | robots.txt
type RobotsTXT = "robots.txt" :> Get '[PlainText] Text

-- | API type
type API =
    ("static" :> Raw)
        :<|> ServerRoutes
        :<|> ("manifest.json" :> Get '[JSON] Manifest)
        :<|> RobotsTXT
        :<|> Raw

data Manifest = Manifest
    { name :: MisoString
    , short_name :: MisoString
    , start_url :: MisoString
    , display :: MisoString
    , theme_color :: MisoString
    , description :: MisoString
    }
    deriving (Show, Eq, Generic)

instance ToJSON Manifest

misoManifest :: Manifest
misoManifest =
    Manifest
        { name = "Haskell Miso Starter"
        , short_name = "Starter"
        , start_url = "."
        , display = "standalone"
        , theme_color = "#00d1b2"
        , description = "A tasty starter for the Haskell Miso front-end web framework"
        }

handle404 :: Application
handle404 _ respond =
    respond $
        responseLBS status404 [("Content-Type", "text/html")] $
            toHtml $
                Page (haskellMisoComponent uri404)

instance ToHtml Page where
    toHtml (Page x) =
        toHtml
            [ doctype_
            , html_
                [ lang_ "en"
                ]
                [ head_
                    [ title_ "A tasty starter for the Haskell Miso front-end web framework"
                    ]
                    [ link_
                        [ rel_ "stylesheet"
                        , href_ "https://cdnjs.cloudflare.com/ajax/libs/github-fork-ribbon-css/0.2.2/gh-fork-ribbon.min.css"
                        ]
                    , link_
                        [ rel_ "manifest"
                        , href_ "/manifest.json"
                        ]
                    , link_
                        [ rel_ "icon"
                        , href_ "static/favicon.ico"
                        , type_ "image/x-icon"
                        ]
                    , meta_
                        [ charset_ "utf-8"
                        ]
                    , meta_
                        [ name_ "theme-color"
                        , content_ "#00d1b2"
                        ]
                    , meta_
                        [ httpEquiv_ "X-UA-Compatible"
                        , content_ "IE=edge"
                        ]
                    , meta_
                        [ name_ "viewport"
                        , content_ "width=device-width, initial-scale=1"
                        ]
                    , meta_
                        [ name_ "description"
                        , content_ "Miso is a small isomorphic Haskell front-end framework featuring a virtual-dom, diffing / patching algorithm, event delegation, event batching, SVG, Server-sent events, Websockets, type-safe servant-style routing and an extensible Subscription-based subsystem. Inspired by Elm and React. Miso is pure by default, but side effects can be introduced into the system via the Effect data type. Miso makes heavy use of the GHC FFI and therefore has minimal dependencies."
                        ]
                    , style_ [] ".github-fork-ribbon:before { background-color: \"#e59751\" !important; } "
                    , cssRef animateRef
                    , cssRef bulmaRef
                    , cssRef fontAwesomeRef
                    , jsRef "https://buttons.github.io/buttons.js"
                    , jsRef "static/all.js"
                    , body_ [] [toView x]
                    ]
                ]
            ]
      where
        jsRef href =
            script_
                [ src_ href
                , async_ "true"
                , defer_ "true"
                ]
                ""
        cssRef href =
            link_
                [ rel_ "stylesheet"
                , type_ "text/css"
                , href_ href
                ]

fontAwesomeRef :: MisoString
fontAwesomeRef = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"

animateRef :: MisoString
animateRef = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css"

bulmaRef :: MisoString
bulmaRef = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"


-- | Server handlers
serverHandlers :: Server ServerRoutes
serverHandlers =
    mkPage uriSiteCounterExample
        :<|> mkPage uriSiteTwo
        :<|> mkPage uriSiteOne
        :<|> mkPage uriHome
        :<|> mkPage uri404
  where
    mkPage :: URI -> Handler Page
    mkPage uri = pure $ Page (haskellMisoComponent uri)
