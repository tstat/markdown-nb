module Renderer where

import Prelude

import Data.Array as A
import Data.List as L
import Data.String as S
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldMap, traverse_)
import Data.Const (Const)
import Data.Either (Either(..))
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
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Pretty (prettyPrintMd)
import Unsafe.Coerce (unsafeCoerce)

type State = { content :: HTML Void (QueryF Unit) }

data QueryF a
  = SetContent String (Boolean -> a)

type Input
  = Unit

data Message = Void

data Slot = Slot
derive instance eqRendererSlot :: Eq Slot
derive instance ordRendererSort :: Ord Slot

ui :: Component HTML QueryF Input Message Aff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Unit -> State
initialState _ = { content: HH.div_ [] }

render
  :: State
  -> HTML Void (QueryF Unit)
render st = HH.div [ HP.class_ (HH.ClassName "renderer") ] [st.content]

eval :: QueryF ~> H.HalogenM State QueryF (Const Void) Void Message Aff
eval = case _ of
  SetContent str resp -> do
    case parseMd str of
      Left _ -> pure $ resp false
      Right sd -> do
        H.modify_ (_ { content = renderSlamDown sd })
        pure $ resp true

renderSlamDown
  ∷ ∀ v m. SD.SlamDown -> HTML v m
renderSlamDown (SD.SlamDown bs) = HH.div_ $ renderBlocks bs
  where
    h_ ∷ Int → Array (HTML v m) → HTML v m
    h_ 1 = HH.h1_
    h_ 2 = HH.h2_
    h_ 3 = HH.h3_
    h_ 4 = HH.h4_
    h_ 5 = HH.h5_
    h_ _ = HH.h6_

    el_ ∷ SD.ListType → Array (HTML v m) → HTML v m
    el_ (SD.Bullet _)  = HH.ul_
    el_ (SD.Ordered _) = HH.ol_

    renderInline ∷ SD.Inline String -> HTML v m
    renderInline i =
      case i of
        SD.Str s → HH.text s
        SD.Entity s → HH.text s
        SD.Space → HH.text " "
        SD.SoftBreak → HH.text "\n"
        SD.LineBreak → HH.br_
        SD.Emph is → HH.em_ $ map renderInline (A.fromFoldable is)
        SD.Strong is → HH.strong_ $ map renderInline (A.fromFoldable is)
        SD.Code _ c → HH.code_ [ HH.text c ]
        SD.Link body tgt → do
          let
            href (SD.InlineLink url) = url
            href (SD.ReferenceLink tgt') = maybe "" ("#" <> _) tgt'
          HH.a [ HP.href $ href tgt ] $ map renderInline (A.fromFoldable body)
        SD.Image body url →
          HH.img
            [ HP.src url
            , HP.alt $ foldMap stripInline body
            ]
        SD.FormField label required field → unsafeCoerce unit

    stripInline ∷ SD.Inline String → String
    stripInline i =
      case i of
        SD.Str s → s
        SD.Entity s → s
        SD.Space → " "
        SD.SoftBreak → "\n"
        SD.LineBreak → "\n"
        SD.Emph is → foldMap stripInline is
        SD.Strong is → foldMap stripInline is
        SD.Code _ c → c
        SD.Link body _ → foldMap stripInline body
        _ → ""

    renderBlocks ∷ L.List (SD.Block String) → Array (HTML v m)
    renderBlocks = foldMap (\x -> A.singleton $ renderBlock x)

    renderBlock ∷ SD.Block String -> HTML v m
    renderBlock b =
      case b of
        SD.Paragraph is →
          HH.p_ $ map renderInline (A.fromFoldable is)
        SD.Header lvl is →
          h_ lvl $ map renderInline (A.fromFoldable is)
        SD.Blockquote bs →
          HH.blockquote_ $ renderBlocks bs
        SD.Lst lt bss →
          let
            item ∷ L.List (SD.Block String) -> HTML v m
            item bs = HH.li_ $ renderBlocks bs
          in el_ lt $ map item (A.fromFoldable bss)
        SD.CodeBlock _ ss →
          HH.pre_ [ HH.code_ [ HH.text (S.joinWith "\n" $ A.fromFoldable ss) ] ]
        SD.LinkReference l url →
          HH.p_
            [ HH.text (l <> ": ")
            , HH.a [ HP.id_ l, HP.href url ] [ HH.text url ]
            ]
        SD.Rule →
          HH.hr_
