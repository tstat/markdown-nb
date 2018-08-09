module Renderer where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe)
import Data.Ord (abs)
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (error)
import Foreign.Object (Object)
import Halogen (Component, ComponentSlot)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Pretty (prettyPrintMd)

type State = { text :: String }

data QueryF a
  = SetContent String (Boolean -> a)

type Input
  = Unit

data Message = Void

data Slot = Slot
derive instance eqRendererSlot :: Eq Slot
derive instance ordRendererSort :: Ord Slot

ui :: Component HTML QueryF Input Message Aff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Unit -> State
initialState _ = { text: "" }

render
  :: State
  -> HTML Void (QueryF Unit)
render st =
  HH.p_ [ HH.text st.text ]

eval :: QueryF ~> H.HalogenM State QueryF (Const Void) Void Message Aff
eval = case _ of
  SetContent str resp -> do
    H.modify_ (_ { text = str })
    pure $ resp true
