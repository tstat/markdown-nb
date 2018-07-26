module Editor where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson
                     , caseJsonObject, caseJsonString, fromString, (.?)
                     , (~>), jsonEmptyObject, (:=), encodeJson
                     )
import Foreign.Object (Object)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe)
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (error)
import Halogen (Component, ComponentSlot)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Clipboard.ClipboardEvent (ClipboardEvent)
import Web.Clipboard.ClipboardEvent as Clipboard
import Web.DOM.Node (fromEventTarget)
import Web.Event.Event (preventDefault, Event, currentTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLTextAreaElement (HTMLTextAreaElement)
import Web.HTML.HTMLTextAreaElement as TextArea
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as Keyboard

type State = { text :: String }

data QueryF a
  = HandleKeyDown KeyboardEvent a
  | HandlePaste ClipboardEvent a
  | SetText String a

type Input
  = String

data DocumentChange
  = Insertion Int String
  | Deletion Int Int

data Message
  = DocumentUpdate DocumentChange

instance decodeDocumentChange :: DecodeJson DocumentChange where
  decodeJson = caseJsonObject (Left "Expected an Object") $ \o ->
    o .? "type" >>= case _ of
      "insertion" -> parseInsertion o
      "deletion" -> parseDeletion o
      b -> Left $ "Could not recognize type: " <> b
    where
      parseInsertion :: Object Json -> Either String DocumentChange
      parseInsertion o = Insertion
        <$> o .? "pos"
        <*> o .? "content"

      parseDeletion :: Object Json -> Either String DocumentChange
      parseDeletion o = Deletion
        <$> o .? "pos"
        <*> o .? "length"

instance encodeDocumentChange :: EncodeJson DocumentChange where
  encodeJson (Deletion k n) =
       "type"   := "deletion"
    ~> "pos"    := k
    ~> "length" := n
    ~> jsonEmptyObject
  encodeJson (Insertion k str) =
       "type"    := "insertion"
    ~> "pos"     := k
    ~> "content" := str
    ~> jsonEmptyObject

instance decodeMessage :: DecodeJson Message where
  decodeJson = map DocumentUpdate <<< decodeJson

instance encodeMessage :: EncodeJson Message where
  encodeJson (DocumentUpdate x) = encodeJson x

data Slot = Slot
derive instance eqEditorSlot :: Eq Slot
derive instance ordEditSort :: Ord Slot

ui :: Component HTML QueryF String Message Aff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input SetText
    }

initialState :: String -> State
initialState txt = { text: txt }

render
  :: State
  -> HTML Void (QueryF Unit)
render st =
  HH.textarea
  [ HE.onKeyDown (HE.input HandleKeyDown)
  , HE.onPaste (HE.input HandlePaste)
  , HP.ref textAreaRefLabel
  , HP.value st.text
  ]

eval :: QueryF ~> H.HalogenM State QueryF (Const Void) Void Message Aff
eval = case _ of
  HandleKeyDown kev next -> do
    if shouldInsert kev
      then runKeyDown kev
      else log ("ignoring key: " <> Keyboard.key kev) *> pure unit
    pure next
  HandlePaste cbev next -> do
    liftEffect $ preventDefault $ Clipboard.toEvent cbev
    log "paste not implemented yet"
    pure next
  SetText str next -> do
    H.modify_ (_ { text = str })
    pure next

runKeyDown
  :: KeyboardEvent
  -> H.HalogenM State QueryF (Const Void) Void Message Aff Unit
runKeyDown kev = do
  liftEffect $ preventDefault $ Keyboard.toEvent kev
  i <- lift <<< justOrError =<< runMaybeT cursorPosition
  st <- H.get
  H.raise $ DocumentUpdate $ docUpdate i (Keyboard.key kev)
  pure unit

cursorPosition :: MaybeT (H.HalogenM State QueryF (Const Void) Void Message Aff) Int
cursorPosition = MaybeT <<< liftEffect <<< cursorPos
  =<< MaybeT (H.getHTMLElementRef textAreaRefLabel)
  where
    cursorPos :: HTMLElement -> Effect (Maybe Int)
    cursorPos = traverse TextArea.selectionStart
      <<< TextArea.fromHTMLElement

justOrError :: forall a. Maybe a -> Aff a
justOrError Nothing = throwError $ error "WHAT KIND OF WITCHCRAFT IS THIS?"
justOrError (Just a) = pure a

-- TODO: do something that makes more sense here
shouldInsert :: KeyboardEvent -> Boolean
shouldInsert kev = str == "Backspace"
  || str == "Delete"
  || String.length str == 1
  where
    str :: String
    str = Keyboard.key kev

docUpdate :: Int -> String -> DocumentChange
docUpdate i = case _ of
  "Backspace" -> Deletion (i-1) 1
  "Delete"    -> Deletion i 1
  k           -> Insertion i k

textAreaRefLabel :: H.RefLabel
textAreaRefLabel = H.RefLabel "textarea"
