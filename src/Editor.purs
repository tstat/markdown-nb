module Editor where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
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
import Web.Clipboard.ClipboardEvent (ClipboardEvent)
import Web.Clipboard.ClipboardEvent as Clipboard
import Web.DOM.Node (fromEventTarget)
import Web.Event.Event (preventDefault, Event, currentTarget)
import Web.HTML.HTMLTextAreaElement (HTMLTextAreaElement)
import Web.HTML.HTMLTextAreaElement as TextArea
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as Keyboard

type State = { text :: String }

data QueryF a
  = HandleKeyDown KeyboardEvent a
  | HandlePaste ClipboardEvent a

type Input
  = Unit

type Message
  = Unit

data Slot = Slot
derive instance eqEditorSlot :: Eq Slot
derive instance ordEditSort :: Ord Slot

ui :: Component HTML QueryF Unit Message Aff
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
render { text: text } =
  HH.textarea
  [ HE.onKeyDown (HE.input HandleKeyDown)
  , HE.onPaste (HE.input HandlePaste)
  ]

eval :: QueryF ~> H.HalogenM State QueryF (Const Void) Void Message Aff
eval = case _ of
  HandleKeyDown kev next -> do
    let ev = Keyboard.toEvent kev
    liftEffect $ preventDefault ev
    log $ Keyboard.key kev
    liftEffect (cursorPos ev) >>= case _ of
      Nothing -> log "WHAT KIND OF WITCHCRAFT IS THIS?"
      Just i -> log (show i)
    pure next
  HandlePaste cbev next -> do
    liftEffect $ preventDefault $ Clipboard.toEvent cbev
    log "paste not implemented yet"
    pure next

cursorPos :: Event -> Effect (Maybe Int)
cursorPos ev = traverse TextArea.selectionStart
  $ TextArea.fromNode
  =<< fromEventTarget
  =<< currentTarget ev
