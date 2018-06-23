module RootComponent where

import Prelude

import Ace.AceComponent (AceOutput(..), AceQuery(..), AceChange, aceComponent)
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
type State = { text :: String }

-- | The query algebra for the app.
data Query a
  = ClearText a
  | HandleAceOutput AceOutput a

-- | The slot address type for the Ace component.
data AceSlot = AceSlot
derive instance eqAceSlot :: Eq AceSlot
derive instance ordAceSlot :: Ord AceSlot

-- | The main UI component definition.
ui :: Component HTML Query Unit Void Aff
ui =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Unit -> State
initialState _ = { text: "" }

render
  :: State
  -> HTML (ComponentSlot HTML AceQuery Aff AceSlot (Query Unit)) (Query Unit)
render { text: text } =
  HH.div_
    [ HH.h1_
        [ HH.text "ace editor" ]
    , HH.div_
        [ HH.p_
            [ HH.button
                [ HE.onClick (HE.input_ ClearText) ]
                [ HH.text "Clear" ]
            ]
        ]
    , HH.div_
        [ HH.slot AceSlot aceComponent unit handleAceOuput ]
    , HH.p_
        [ HH.text ("Current text: " <> text) ]
    ]

  where
    handleAceOuput :: AceOutput -> Maybe (Query Unit)
    handleAceOuput output =
      Just (HandleAceOutput output unit)


eval :: Query ~> H.ParentDSL State Query AceQuery AceSlot Void Aff
eval (ClearText next) = do
  -- _ <- H.query AceSlot $ H.action (ChangeText "")
  pure next
eval (HandleAceOutput output next) = do
  -- H.modify_ (_ { text = text })
  logShow output
  pure next
