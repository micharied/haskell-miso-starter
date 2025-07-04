{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Common where

import           Control.Monad.State
import           Data.Bool
import           Data.Proxy
import           Servant.API
import           Servant.Links

import           Miso
import           Miso.String
import qualified Miso.Style as CSS

{- | We can pretty much share everything

model, action, view, router, links, events map
decoders are all shareable
-}

-- | Model
data Model = Model
    { uri :: URI
    , navMenuOpen :: Bool
    }
    deriving (Show, Eq)

-- | Event Actions
data Action
    = ChangeURI URI
    | HandleURI URI
    | ToggleNavMenu
    deriving (Show, Eq)

-- | Routes (server / client agnostic)
type Home a = a

type SiteOne a = "site-one" :> a
type SiteTwo a = "site-two" :> a
type SiteCounterExample a = "site-counter-example" :> a
type The404 a = "404" :> a

-- | Routes skeleton
type Routes a =
    SiteCounterExample a
        :<|> SiteTwo a
        :<|> SiteOne a
        :<|> Home a
        :<|> The404 a

-- | Client routing
type ClientRoutes = Routes (View Action)

-- | Server routing
type ServerRoutes = Routes (Get '[HTML] Page)

-- | Component synonym
type HaskellMisoComponent = Component "app" Model Action

-- | Links
uriHome, uriSiteOne, uriSiteTwo, uriSiteCounterExample, uri404 :: URI
uriSiteCounterExample
    :<|> uriSiteTwo
    :<|> uriSiteOne
    :<|> uriHome
    :<|> uri404 = allLinks' linkURI (Proxy @ClientRoutes)

-- | Page for setting HTML doctype and header
newtype Page = Page HaskellMisoComponent

-- | Client Handlers
clientHandlers ::
    (Model -> View Action)
        :<|> (Model -> View Action)
        :<|> (Model -> View Action)
        :<|> (Model -> View Action)
        :<|> (Model -> View Action)
clientHandlers =
    siteCounterExample
        :<|> siteTwo
        :<|> siteOne
        :<|> home
        :<|> the404

secs :: Int -> Int
secs = (*1000000)

haskellMisoComponent ::
    URI ->
    HaskellMisoComponent
haskellMisoComponent uri
  = (app uri)
  { subs = [ uriSub HandleURI ]
  , logLevel = DebugAll
  }
  
app :: URI -> Component name Model Action
app currentUri = defaultComponent emptyModel updateModel viewModel
  where
    emptyModel = Model currentUri False
    viewModel m =
        case route (Proxy :: Proxy ClientRoutes) clientHandlers uri m of
          Left _ -> the404 m
          Right v -> v

updateModel :: Action -> Effect Model Action
updateModel = \case
  HandleURI u ->
    modify $ \m -> m { uri = u }
  ChangeURI u -> do
    modify $ \m -> m { navMenuOpen = False }
    io_ (pushURI u)
  ToggleNavMenu -> do
    m@Model{..} <- get
    put m { navMenuOpen = not navMenuOpen }

-- | Views
siteOne :: Model -> View Action
siteOne = template v
  where
    v =
        div_
            [class_ "animated fadeIn"]
            [
             h1_
                [ class_ "title animated pulse"
                , CSS.style_
                    [ CSS.fontSize "82px"
                    , CSS.fontWeight "100"
                    ]
                ]
                [text "siteOne"]
            ]

siteTwo :: Model -> View Action
siteTwo = template v
  where
    v =
        div_
            [class_ "animated fadeIn"]
            [ 
             h1_
                [ class_ "title animated pulse"
                , CSS.style_
                    [ CSS.fontSize "82px"
                    , CSS.fontWeight "100"
                    ]
                ]
                [text "siteTwo"]
            ]

siteCounterExample :: Model -> View Action
siteCounterExample = template v
  where
    v =
        div_
            [class_ "animated fadeIn"]
            [ h1_
                [ class_ "title animated pulse"
                , CSS.style_
                    [ CSS.fontSize "82px"
                    , CSS.fontWeight "100"
                    ]
                ]
                [text "siteCounterExample - cooming soon!"]
            ]

home :: Model -> View Action
home = template v
  where
    v =
        div_
            [class_ "animated fadeIn"]
            [
             h1_
                [ class_ "title animated pulse"
                , CSS.style_
                    [ CSS.fontSize "82px"
                    , CSS.fontWeight "100"
                    ]
                ]
                [text "home"]
            ]

template :: View Action -> Model -> View Action
template content Model{..} =
    div_
        []
        [ 
         hero content uri navMenuOpen
        ]


cols :: View action
cols =
    section_
        []
        [ div_
            [class_ "container"]
            [ div_
                [class_ "columns"]
                [ div_
                    [class_ "column"]
                    [ h1_
                        [class_ "title"]
                        [ span_ [class_ "icon is-large"] [i_ [class_ "fa fa-flash"] []]
                        , text "Fast"
                        ]
                    , h2_
                        [class_ "subtitle"]
                        [ text "Mutable virtual dom implementation"
                        ]
                    ]
                , div_
                    [class_ "column"]
                    [ text "Second column"
                    ]
                , div_
                    [class_ "column"]
                    [ text "Third column"
                    ]
                , div_
                    [class_ "column"]
                    [ text "Fourth column"
                    ]
                ]
            ]
        ]

the404 :: Model -> View Action
the404 = template v
  where
    v =
        div_
            []
            [ 
             h1_
                [ class_ "title"
                , CSS.style_
                    [ CSS.fontSize "82px"
                    , CSS.fontWeight "100"
                    ]
                ]
                [text "404"]
            ]

-- | Hero
hero :: View Action -> URI -> Bool -> View Action
hero content uri' navMenuOpen' =
    section_
        [class_ "hero is-medium is-primary is-bold has-text-centered"]
        [ div_
            [class_ "hero-head"]
            [ header_
                [class_ "nav"]
                [ div_
                    [class_ "container"]
                    [ div_
                        [class_ "nav-left"]
                        [ a_ [class_ "nav-item"] []
                        ]
                    , span_
                        [ class_ $ "nav-toggle " <> bool mempty "is-active" navMenuOpen'
                        , onClick ToggleNavMenu
                        ]
                        [ span_ [] []
                        , span_ [] []
                        , span_ [] []
                        ]
                    , div_
                        [class_ $ "nav-right nav-menu " <> bool mempty "is-active" navMenuOpen']
                        [ div_
                            [ classList_
                                [ ("nav-item", True)
                                ]
                            ]
                            [ a_
                                [ href_ $ ms (uriPath uriHome)
                                , onPreventClick (ChangeURI uriHome)
                                , classList_
                                    [ ("is-active", uriPath uri' == "")
                                    ]
                                ]
                                [ text "Home"
                                ]
                            ]
                        , div_
                            [ classList_
                                [ ("nav-item", True)
                                ]
                            ]
                            [ a_
                                [ href_ $ ms (uriPath uriSiteOne)
                                , onPreventClick (ChangeURI uriSiteOne)
                                , classList_ [("is-active", uriPath uri' == uriPath uriSiteOne)]
                                ]
                                [text "Site One"]
                            ]
                        , div_
                            [ classList_
                                [ ("nav-item", True)
                                ]
                            ]
                            [ a_
                                [ href_ $ ms (uriPath uriSiteTwo)
                                , onPreventClick (ChangeURI uriSiteTwo)
                                , classList_
                                    [ ("is-active", uriPath uri' == uriPath uriSiteTwo)
                                    ]
                                ]
                                [text "Site Two"]
                            ]
                        , div_
                            [ classList_
                                [ ("nav-item", True)
                                ]
                            ]
                            [ a_
                                [ href_ $ ms (uriPath uriSiteCounterExample)
                                , onPreventClick (ChangeURI uriSiteCounterExample)
                                , classList_
                                    [ ("is-active", uriPath uri' == uriPath uriSiteCounterExample)
                                    ]
                                ]
                                [text "Site Counter Example"]
                            ]
                        ]
                    ]
                ]
            ]
        , div_
            [class_ "hero-body"]
            [ div_
                [class_ "container"]
                [ content
                ]
            ]
        ]

onPreventClick :: Action -> Attribute Action
onPreventClick action =
    onWithOptions
        defaultOptions{preventDefault = True}
        "click"
        emptyDecoder
        (\() -> const action)
