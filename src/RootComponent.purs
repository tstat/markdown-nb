module RootComponent where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson
                     , caseJsonObject, caseJsonString, fromString, (.?)
                     , (~>), jsonEmptyObject, (:=), encodeJson
                     )
import Data.Const (Const)
import Data.Either
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Foreign.Object (Object)
import Halogen (ParentHTML, Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.Component.ChildPath (cp1, cp2)

import Editor as Editor
import Renderer as Renderer

type ChildQuery = Coproduct2 Editor.QueryF Renderer.QueryF
type ChildSlot = Either2 Editor.Slot Renderer.Slot

-- | The application state, which in this case just stores the current text in
-- | the editor.
type NbState = Unit

-- | The query algebra for the app.
data NbQuery a
  = EditorTextChange Editor.DocumentChange a
  | HandleServerOutput ServerOutput a

-- | The type of blob that is sent to the server.
type ServerInput
  = Editor.DocumentChange

-- | The type of blob that comes from the server.
data ServerOutput
  = ServerOutputContents String
    -- ^ The entire document.
  | ServerOutputDelta Editor.DocumentChange
    -- ^ A document delta.

instance decodeJsonServerOutput :: DecodeJson ServerOutput where
  decodeJson =
    caseJsonObject (Left "Expected an Object") $ \o ->
      o .? "type" >>= case _ of
        "contents" -> ServerOutputContents <$> parseContents o
        "insertion" -> ServerOutputDelta <$> parseInsertion o
        "deletion" -> ServerOutputDelta <$> parseDeletion o
        b -> Left $ "Could not recognize type: " <> b
      where
        parseContents :: Object Json -> Either String String
        parseContents o =
          o .? "contents"

        parseInsertion :: Object Json -> Either String Editor.DocumentChange
        parseInsertion o = Editor.Insertion
          <$> o .? "pos"
          <*> o .? "content"

        parseDeletion :: Object Json -> Either String Editor.DocumentChange
        parseDeletion o = Editor.Deletion
          <$> o .? "pos"
          <*> o .? "len"

type NbInput
  = Unit

type NbOutput
  = Editor.DocumentChange

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
initialState _ = unit

render
  :: NbState
  -> ParentHTML NbQuery ChildQuery ChildSlot Aff
render _ =
  HH.div [ HP.class_ (HH.ClassName "wrap") ]
    [ HH.header_
      [ HH.h4_ [ HH.text "markdown-nb" ]
      ]
    , HH.div [ HP.class_ (HH.ClassName "main") ]
        [ HH.slot' cp1 Editor.Slot Editor.ui "" handleEditorMessage
        , HH.slot' cp2 Renderer.Slot Renderer.ui unit (const Nothing)
        ]
    ]

eval :: NbQuery ~> H.HalogenM NbState NbQuery ChildQuery ChildSlot NbOutput Aff
eval = case _ of
  HandleServerOutput so next -> do
    case so of
      ServerOutputContents str -> do
         _ <- H.query' cp1 Editor.Slot (Editor.SetContents str unit)
         b <- H.query' cp2 Renderer.Slot (Renderer.SetContent str identity)
         pure unit
      ServerOutputDelta dc -> do
        mstr <- H.query' cp1 Editor.Slot (Editor.ApplyChange dc identity)
        case mstr of
          Just str -> do
            _ <- H.query' cp2 Renderer.Slot (Renderer.SetContent str identity)
            pure unit
          Nothing -> pure unit
    pure next
  EditorTextChange dc next -> do
    H.raise dc
    pure next

handleEditorMessage :: Editor.Message -> Maybe (NbQuery Unit)
handleEditorMessage (Editor.DocumentUpdate dc) = HE.input EditorTextChange dc
