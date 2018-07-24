module RootComponent where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Halogen (ParentHTML, Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Editor as Editor

-- | The application state, which in this case just stores the current text in
-- | the editor.
type NbState = { text :: String }

-- | The query algebra for the app.
data NbQuery a
  = HandleTextChange a
  | HandleServerOutput a

type NbInput
  = Unit

type NbOutput
  = Void

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
  -> ParentHTML NbQuery Editor.QueryF Editor.Slot Aff
render { text: text } =
  HH.div_
    [ HH.h1_ [ HH.text "Neckbeards" ]
    , HH.div_
        [ HH.slot Editor.Slot Editor.ui unit (const Nothing) ]
    , HH.p_
        [ HH.text ("Current text: " <> text) ]
    ]

eval :: NbQuery ~> H.HalogenM NbState NbQuery Editor.QueryF Editor.Slot NbOutput Aff
eval = case _ of
  HandleServerOutput next -> do
    pure next
  HandleTextChange next -> do
    pure next
