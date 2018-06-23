module Ace.AceComponent
  ( AceQuery(..)
  , AceOutput(..)
  , aceComponent
  ) where

import Prelude

import Ace as Ace
import Ace.Document as Document
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Range as Range
import Ace.Types
  (DocumentEvent(..), DocumentEventType(..), Editor, getColumn, getRow)
import Data.Const (Const)
import Data.Generic.Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | The state for the ace component - we only need a reference to the editor,
-- | as Ace editor has its own internal state that we can query instead of
-- | replicating it within Halogen.
type AceState
  = { editor :: Maybe Editor }

-- | A basic query algebra for the Ace component.
data AceQuery a
  = Initialize a
  | Finalize a
  | ApplyChange DocumentEvent a
  | OnChange DocumentEvent (H.SubscribeStatus -> a)

type AceInput
    = Unit

data AceOutput
  = Change DocumentEvent

-- | The Ace component definition.
aceComponent :: H.Component HH.HTML AceQuery AceInput AceOutput Aff
aceComponent =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , initializer
    , finalizer
    , receiver
    }

initialState :: AceInput -> AceState
initialState _ =
  { editor: Nothing }

-- As we're embedding a 3rd party component we only need to create a placeholder
-- div here and attach the ref property which will let us reference the element
-- in eval.
render :: AceState -> HTML Void (AceQuery Unit)
render _ =
  HH.div [ HP.ref (H.RefLabel "ace"), HP.class_ (HH.ClassName "ace-editor") ] []

-- The query algebra for the component handles the initialization of the Ace
-- editor as well as responding to the `ChangeText` action that allows us to
-- alter the editor's state.
eval :: AceQuery ~> HalogenM AceState AceQuery (Const Void) Void AceOutput Aff
eval = case _ of
  Initialize next -> do
    evalInitialize
    pure next
  Finalize next ->
    evalFinalize next
  ApplyChange change next -> do
    evalApplyChange change
    pure next
  OnChange change reply ->
    reply <$> evalOnChange change

evalInitialize
  :: HalogenM AceState AceQuery (Const Void) Void AceOutput Aff Unit
evalInitialize = do
  H.getHTMLElementRef (H.RefLabel "ace") >>= case _ of
    Nothing ->
      pure unit
    Just el' -> do
      editor <- H.liftEffect $ Ace.editNode el' Ace.ace
      session <- H.liftEffect $ Editor.getSession editor
      H.modify_ (_ { editor = Just editor })
      H.subscribe $
        H.eventSource
          (Session.onChange session)
          (\event -> Just (OnChange event identity))

evalFinalize
  :: forall x.
     x
  -> HalogenM AceState AceQuery (Const Void) Void AceOutput Aff x
evalFinalize next = do
  -- Release the reference to the editor and do any other cleanup that a
  -- real world component might need.
  H.modify_ (_ { editor = Nothing })
  pure next

evalApplyChange
  :: DocumentEvent
  -> HalogenM AceState AceQuery (Const Void) Void AceOutput Aff Unit
evalApplyChange (DocumentEvent event) = do
  maybeEditor <- H.gets _.editor
  case maybeEditor of
    Nothing ->
      pure unit
    Just editor -> H.liftEffect do
      session <- Editor.getSession editor
      document <- Session.getDocument session
      case event.action of
        Insert ->
          Document.insertLines (getRow event.start) event.lines document
        Remove -> do
          range <-
            (Range.create
              (getRow event.start)
              (getColumn event.start)
              (getRow event.end)
              (getColumn event.end))
          Document.remove range document

evalOnChange
  :: forall x.
     DocumentEvent
  -> HalogenM AceState AceQuery (Const Void) Void AceOutput Aff H.SubscribeStatus
evalOnChange event = do
  H.raise (Change event)
  pure H.Listening

initializer :: Maybe (AceQuery Unit)
initializer =
  Just (H.action Initialize)

finalizer :: Maybe (AceQuery Unit)
finalizer =
  Just (H.action Finalize)

receiver :: AceInput -> Maybe (AceQuery Unit)
receiver _ =
  Nothing
