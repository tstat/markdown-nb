module Main where

import Prelude

import Control.Bind (bind)
import Control.Coroutine
  (Consumer, Producer, Transformer, ($$), (~$), await, consumer, transform,
    transformConsumer)
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff (Emitter, produce) as Co
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (stringify, encodeJson, decodeJson, Json, jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Editor as Editor
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign (F, Foreign, unsafeToForeign, readString)
import Foreign.Generic.Types as Generic
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.EventTarget as Event
import Web.Socket.Event.EventTypes (onMessage) as WebSocket
import Web.Socket.Event.MessageEvent (MessageEvent) as WebSocket
import Web.Socket.Event.MessageEvent as WebSocket.MessageEvent
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket (create, sendString, toEventTarget) as WebSocket

import RootComponent (NbOutput, NbQuery(..), ServerInput, ServerOutput, ui)

main :: Effect Unit
main = do
  connection <-
    WebSocket.create "ws://localhost:8600" []

  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI ui unit body

    -- Forward all output from the root component to the server.
    io.subscribe (wsConsumer connection)

    -- Forward all input from the server to the root component.
    CR.runProcess (wsProducer connection $$ sink io.query)

-- A consumer coroutine that takes output messages from our component IO and
-- sends them to the server.
wsConsumer :: WebSocket -> Consumer ServerInput Aff Unit
wsConsumer socket =
  forever do
    s <- await
    liftEffect $ WebSocket.sendString socket $ stringify $ encodeJson s

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: forall r. WebSocket -> Producer ServerOutput Aff r
wsProducer socket =
  Co.produce produce
  where
    produce :: Co.Emitter Effect ServerOutput r -> Effect Unit
    produce emitter = do
      listener :: Event.EventListener <-
        Event.eventListener listen

      Event.addEventListener
        WebSocket.onMessage
        listener
        false
        (WebSocket.toEventTarget socket)
      where
        listen :: Event -> Effect Unit
        listen event =
          for_ (WebSocket.MessageEvent.fromEvent event) listen'

        listen' :: WebSocket.MessageEvent -> Effect Unit
        listen' event = do
          json <- foreignToJson (WebSocket.MessageEvent.data_ event)
          emit emitter json

-- | Consume the server's output by forwarding it to the root component.
sink :: forall r. (NbQuery ~> Aff) -> Consumer ServerOutput Aff r
sink query =
  forever do
    output <- await
    lift (query (HandleServerOutput output unit))

-- | Read a foreign value as server input. If it doesn't parse, blow up.
foreignToJson :: Foreign -> Effect ServerOutput
foreignToJson x =
  fromRight do
    string <- lmap (const "") (runExcept (readString x))
    json <- jsonParser string
    decodeJson json

fromRight :: forall a b. Either a b -> Effect b
fromRight =
  either (const (throw "fromRight: Left")) pure
