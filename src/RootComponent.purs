module RootComponent where

import Prelude

import Ace.AceComponent (AceOutput(..), AceQuery(..), aceComponent)
import Ace.Types (DocumentEvent)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Halogen (Component, ComponentSlot)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

-- | The application state, which in this case just stores the current text in
-- | the editor.
type NbState = { text :: String }

-- | The query algebra for the app.
data NbQuery a
  = HandleAceOutput AceOutput a
  | HandleServerOutput DocumentEvent a

type NbInput
  = Unit

type NbOutput
  = DocumentEvent

-- | The slot address type for the Ace component.
data AceSlot = AceSlot
derive instance eqAceSlot :: Eq AceSlot
derive instance ordAceSlot :: Ord AceSlot

-- | The main UI component definition.
ui :: Component HTML NbQuery Unit NbOutput Aff
ui =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Unit -> NbState
initialState _ = { text: "" }

render
  :: NbState
  -> HTML (ComponentSlot HTML AceQuery Aff AceSlot (NbQuery Unit)) (NbQuery Unit)
render { text: text } =
  HH.div_
    [ HH.h1_
        [ HH.text "ace editor" ]
    , HH.div_
        [ HH.slot AceSlot aceComponent unit handleAceOuput ]
    , HH.p_
        [ HH.text ("Current text: " <> text) ]
    ]

  where
    handleAceOuput :: AceOutput -> Maybe (NbQuery Unit)
    handleAceOuput output =
      Just (HandleAceOutput output unit)

eval :: NbQuery ~> H.ParentDSL NbState NbQuery AceQuery AceSlot NbOutput Aff
eval = case _ of
  HandleAceOutput output next -> do
    evalHandleAceOutput output
    pure next
  HandleServerOutput output next -> do
    evalHandleServerOutput output
    pure next

evalHandleAceOutput
  :: AceOutput
  -> H.ParentDSL NbState NbQuery AceQuery AceSlot NbOutput Aff Unit
evalHandleAceOutput =
  case _ of
    Change change ->
      H.raise change

evalHandleServerOutput
  :: DocumentEvent
  -> H.ParentDSL NbState NbQuery AceQuery AceSlot NbOutput Aff Unit
evalHandleServerOutput change = do
  _ <- H.query AceSlot (ApplyChange change unit)
  pure unit
