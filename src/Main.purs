module Main where

import Prelude

import Control.Bind (bind)
import Control.Coroutine
  (Consumer, Producer, Transformer, ($$), (~$), await, consumer, transform,
    transformConsumer)
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (stringify, encodeJson, decodeJson, Json, jsonParser)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Editor as Editor
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign (F, Foreign, unsafeToForeign, readString)
import Foreign.Generic.Types as Generic
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS

import RootComponent (NbOutput, NbQuery(..), ui)

main :: Effect Unit
main = do
  connection <-
    WS.create "ws://0.0.0.0:8600" []

  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI ui unit body

    -- Forward all output from the root component to the server.
    io.subscribe (wsConsumer connection)

    CR.runProcess
      (wsProducer connection $$
        forever do
          s <- await
          case decodeJson s of
            Left err ->
              unsafeThrow $ "Bad server json: " <> err
            Right change ->
              lift (io.query (HandleServerOutput change unit)))

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: WebSocket -> Producer Json Aff Unit
wsProducer socket = CRA.produce \emitter -> do
  listener <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (readHelper (map (eitherToMaybe <<< jsonParser) <<< readString) (ME.data_ msgEvent)) \msg ->
        emit emitter msg
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)
  where
    readHelper :: forall a b. (Foreign -> F (Maybe a)) -> b -> Maybe a
    readHelper read =
      either (const Nothing) identity <<< runExcept <<< read <<< unsafeToForeign

eitherToMaybe :: forall a b. Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

{-
-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from the
-- producer.
wsConsumer :: (Log.Query ~> Aff) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  query $ H.action $ Log.AddMessage msg
  pure Nothing
-}

-- A consumer coroutine that takes output messages from our component IO and
-- sends them using the websocket
wsConsumer :: WebSocket -> Consumer Editor.DocumentChange Aff Unit
wsConsumer socket =
  forever do
    s <- await
    liftEffect $ WS.sendString socket $ stringify $ encodeJson s
