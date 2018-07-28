module Editor where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson
                     , caseJsonObject, caseJsonString, fromString, (.?)
                     , (~>), jsonEmptyObject, (:=), encodeJson
                     )
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
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
  | ApplyChange DocumentChange a
  | SetContents String a

type Input
  = String

data DocumentChange
  = Insertion Int String
  | Deletion Int Int

documentChangePos :: DocumentChange -> Int
documentChangePos (Insertion i _ ) = i
documentChangePos (Deletion i _ ) = i

documentChangeLen :: DocumentChange -> Int
documentChangeLen (Insertion _ s ) = String.length s
documentChangeLen (Deletion _ i ) = i

data Message
  = DocumentUpdate DocumentChange
  | NewContent String

instance encodeDocumentChange :: EncodeJson DocumentChange where
  encodeJson (Deletion k n) =
       "type"   := "deletion"
    ~> "pos"    := k
    ~> "len" := n
    ~> jsonEmptyObject
  encodeJson (Insertion k str) =
       "type"    := "insertion"
    ~> "pos"     := k
    ~> "content" := str
    ~> jsonEmptyObject

data Slot = Slot
derive instance eqEditorSlot :: Eq Slot
derive instance ordEditSort :: Ord Slot

ui :: Component HTML QueryF String Message Aff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
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
  ApplyChange dc next -> do
    applyChange dc
    pure next
  SetContents str next -> do
    H.put { text: str }
    pure next

applyChange
  :: DocumentChange
  -> H.HalogenM State QueryF (Const Void) Void Message Aff Unit
applyChange dc = do
  sel <- getSelection
  st <- H.modify (\st -> st { text = calcNewString st.text dc })
  setSelection (newSelection sel)
  H.raise $ NewContent st.text
  where
    newSelection :: Selection -> Selection
    newSelection sel = { start: newStart, end: newEnd }
      where
        newStart =
          case dc of
            (Insertion i str) ->
              if sel.start >= i
              then i + String.length str
              else i
            (Deletion i k) ->
              if sel.start >= i + k
              then sel.start - k
              else sel.start
        newEnd = newStart

runKeyDown
  :: KeyboardEvent
  -> H.HalogenM State QueryF (Const Void) Void Message Aff Unit
runKeyDown kev = do
  liftEffect $ preventDefault $ Keyboard.toEvent kev
  sel <- getSelection
  st <- H.get
  if sel.start /= sel.end
    then H.raise $ DocumentUpdate $ Deletion (min sel.start sel.end) (abs $ sel.start - sel.end)
    else pure unit
  H.raise $ DocumentUpdate $ docUpdate sel.start (Keyboard.key kev)
  pure unit

type Selection = { start :: Int, end :: Int }

getSelection :: H.HalogenM State QueryF (Const Void) Void Message Aff Selection
getSelection = lift <<< justOrError =<< runMaybeT getSelection'

getSelection' :: MaybeT (H.HalogenM State QueryF (Const Void) Void Message Aff) Selection
getSelection' =
  MaybeT <<< toSel =<< MaybeT getTextArea
  where
    toSel ta = map Just $ { start: _, end: _ }
      <$> liftEffect (TextArea.selectionStart ta)
      <*> liftEffect (TextArea.selectionEnd ta)

getTextArea
  :: H.HalogenM State QueryF (Const Void) Void Message Aff (Maybe TextArea.HTMLTextAreaElement)
getTextArea = toTA <$> H.getHTMLElementRef textAreaRefLabel
   where
     toTA = case _ of
       Nothing -> Nothing
       Just el -> TextArea.fromHTMLElement el

setSelection :: Selection -> H.HalogenM State QueryF (Const Void) Void Message Aff Unit
setSelection sel =
  maybe (log "can't set cursor position") g =<< getTextArea
  where
    g ta = liftEffect do
      TextArea.setSelectionStart sel.start ta
      TextArea.setSelectionEnd sel.end ta

justOrError :: forall a. Maybe a -> Aff a
justOrError Nothing = throwError $ error "WHAT KIND OF WITCHCRAFT IS THIS?"
justOrError (Just a) = pure a

-- TODO: do something that makes more sense here
shouldInsert :: KeyboardEvent -> Boolean
shouldInsert kev = str == "Backspace"
  || str == "Delete"
  || str == "Enter"
  || String.length str == 1
  where
    str :: String
    str = Keyboard.key kev

docUpdate :: Int -> String -> DocumentChange
docUpdate i = case _ of
  "Backspace" -> Deletion (i-1) 1
  "Delete"    -> Deletion i 1
  "Enter"     -> Insertion i "\n"
  k           -> Insertion i k

textAreaRefLabel :: H.RefLabel
textAreaRefLabel = H.RefLabel "textarea"

calcNewString :: String -> DocumentChange -> String
calcNewString orig (Insertion i str) =
  let { before, after } = String.splitAt i orig
  in before <> str <> after
calcNewString orig (Deletion i k) =
  let { before, after } = String.splitAt i orig
  in before <> String.drop k after
