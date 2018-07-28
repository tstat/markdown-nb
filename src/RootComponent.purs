module RootComponent where

import Prelude

import Data.Argonaut (class DecodeJson)
import Data.Const (Const)
import Data.Either
import Data.Maybe (Maybe(..))
import Data.String as String
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
  = EditorTextChange Editor.DocumentChange a
  | HandleServerOutput ServerOutput a

-- | The type of blob that is sent to the server.
type ServerInput
  = Editor.Message

-- | The type of blob that comes from the server.
data ServerOutput
  = ServerOutputContents String
    -- ^ The entire document.
  | ServerOutputDelta Editor.DocumentChange
    -- ^ A document delta.

-- TODO decodeJsonServerOutput
instance decodeJsonServerOutput :: DecodeJson ServerOutput where
  decodeJson _ = Left "TODO"

type NbInput
  = Unit

type NbOutput
  = Editor.Message

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
        [ HH.slot Editor.Slot Editor.ui "" handleEditorMessage ]
    , HH.p_
        [ HH.text ("Current text: " <> text) ]
    ]

eval :: NbQuery ~> H.HalogenM NbState NbQuery Editor.QueryF Editor.Slot NbOutput Aff
eval = case _ of
  HandleServerOutput (ServerOutputContents str) next -> do
    H.put { text: str }
    pure next
  HandleServerOutput (ServerOutputDelta dc) next -> do
    st <- H.get
    let newStr = calcNewString st.text dc
    H.modify_ (_ { text = newStr })
    _ <- H.query Editor.Slot (Editor.SetText newStr unit)
    pure next
  EditorTextChange dc next -> do
    H.raise (Editor.DocumentUpdate dc)
    pure next

calcNewString :: String -> Editor.DocumentChange -> String
calcNewString orig (Editor.Insertion i str) = let { before, after } = String.splitAt i orig
                                              in before <> str <> after
calcNewString orig (Editor.Deletion i k) = let { before, after } = String.splitAt i orig
                                           in before <> String.drop k after

handleEditorMessage :: Editor.Message -> Maybe (NbQuery Unit)
handleEditorMessage (Editor.DocumentUpdate dc) = HE.input EditorTextChange dc
