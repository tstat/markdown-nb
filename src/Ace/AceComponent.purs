module Ace.AceComponent (AceQuery(..), AceOutput(..), aceComponent) where

import Prelude

import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Types (Editor)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
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
  | ChangeText String a
  | HandleChange (H.SubscribeStatus -> a)

type AceInput
    = Unit

data AceOutput
  = TextChanged String

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
render :: AceState -> HH.HTML Void (AceQuery Unit)
render _ =
  HH.div [ HP.ref (H.RefLabel "ace"), HP.class_ (HH.ClassName "ace-editor") ] []

-- The query algebra for the component handles the initialization of the Ace
-- editor as well as responding to the `ChangeText` action that allows us to
-- alter the editor's state.
eval :: AceQuery ~> H.ComponentDSL AceState AceQuery AceOutput Aff
eval = case _ of
  Initialize next ->
    evalInitialize next
  Finalize next ->
     evalFinalize next
  ChangeText text next ->
    evalChangeText text next
  HandleChange reply ->
    evalHandleChange reply

evalInitialize
  :: forall x.
     x
  -> H.HalogenM AceState AceQuery (Const Void) Void AceOutput Aff x
evalInitialize next = do
  H.getHTMLElementRef (H.RefLabel "ace") >>= case _ of
    Nothing -> pure unit
    Just el' -> do
      editor <- H.liftEffect $ Ace.editNode el' Ace.ace
      session <- H.liftEffect $ Editor.getSession editor
      H.modify_ (_ { editor = Just editor })
      H.subscribe $ H.eventSource_ (Session.onChange session) (H.request HandleChange)
  pure next

evalFinalize
  :: forall x.
     x
  -> H.HalogenM AceState AceQuery (Const Void) Void AceOutput Aff x
evalFinalize next = do
  -- Release the reference to the editor and do any other cleanup that a
  -- real world component might need.
  H.modify_ (_ { editor = Nothing })
  pure next

evalChangeText
  :: forall x.
     String
  -> x
  -> H.HalogenM AceState AceQuery (Const Void) Void AceOutput Aff x
evalChangeText text next = do
  maybeEditor <- H.gets _.editor
  case maybeEditor of
    Nothing -> pure unit
    Just editor -> do
      current <- H.liftEffect $ Editor.getValue editor
      when (text /= current) do
        void $ H.liftEffect $ Editor.setValue text Nothing editor
  H.raise $ TextChanged text
  pure next

evalHandleChange
  :: forall x.
     (H.SubscribeStatus -> x)
  -> H.HalogenM AceState AceQuery (Const Void) Void AceOutput Aff x
evalHandleChange reply = do
  maybeEditor <- H.gets _.editor
  case maybeEditor of
    Nothing -> pure unit
    Just editor -> do
      text <- H.liftEffect (Editor.getValue editor)
      H.raise $ TextChanged text
  pure (reply H.Listening)

initializer :: Maybe (AceQuery Unit)
initializer =
  Just (H.action Initialize)

finalizer :: Maybe (AceQuery Unit)
finalizer =
  Just (H.action Finalize)

receiver :: AceInput -> Maybe (AceQuery Unit)
receiver _ =
  Nothing
