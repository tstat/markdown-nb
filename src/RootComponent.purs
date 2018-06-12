module RootComponent where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import DOM (DOM)
import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State
  = { switch :: Boolean }

data Query a
  = Noop a
  | Toggle a

component
  :: forall m eff. MonadEff ( console :: CONSOLE, dom :: DOM | eff ) m
  => H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState = { switch: true }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_
      [ HH.h1_
          [ HH.text ("OUR COOL THING " <> if st.switch
                                         then "IS ALIVE"
                                         else "IS DEAD")]
      , HH.button
        [HE.onClick (HE.input_ Toggle)]
        [HH.text "click me"]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (Noop next) = pure next
  eval (Toggle next) = do
    H.modify (\st -> st { switch = not st.switch })
    pure next
