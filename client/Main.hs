----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import Miso
import Miso.String
import Miso.Lens
----------------------------------------------------------------------------
-- | Component model state
newtype Model = Model
  { _counter :: Int
  } deriving (Show, Eq)
----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record { _counter = field }
----------------------------------------------------------------------------
-- | Sum type for Component events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run $ startComponent app
  { events = pointerEvents
  }
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.



----------------------------------------------------------------------------

-- | Component definition (uses 'defaultComponent' smart constructor)
app :: Component name Model Action
app = defaultComponent (Model 0) updateModel viewModel
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = Model 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect Model Action
updateModel = \case
  AddOne        -> counter += 1
  SubtractOne   -> counter -= 1
  SayHelloWorld -> io_ $ do
    consoleLog "Hello World"
    alert "Hello World"
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ []
  [ button_ [ onClick AddOne ] [ text "+" ]
  , text $ ms (x ^. counter)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , button_ [ onClick SayHelloWorld ] [ text "Alert Hello World!" ]
  ]
----------------------------------------------------------------------------