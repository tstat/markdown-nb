module Main
  ( main
  ) where

import Mitchell

import Concurrency (killThread)
import Exception (finally)
import FRP
import Json.Decode (FromJSON)
import Json.Encode (Value) -- TODO: Export Value from Json.Decode as well
import Network.WebSockets (Connection, PendingConnection)
import Text (pack)

import qualified Json.Decode
import qualified Network.WebSockets as WebSockets
import qualified Text.Partial

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  --
  -- Create network inputs
  --
  (newClientAddHandler, fireNewClient) <-
    newAddHandler

  --
  -- Define the event network
  --
  network :: EventNetwork <-
    compile $ do
      eNewClient :: Event (PendingConnection, ThreadId) <-
        fromAddHandler newClientAddHandler

      moment eNewClient

  --
  -- Actuate the event network
  --
  actuate network

  --
  -- Accept clients forever
  --
  putStrLn "Running on 0.0.0.0:8600"
  WebSockets.runServer "0.0.0.0" 8600 $ \pconn -> do
    tid :: ThreadId <-
      myThreadId
    fireNewClient (pconn, tid)
    forever (threadDelay maxBound) -- Wait to die

--------------------------------------------------------------------------------
-- Data types and pure functions
--------------------------------------------------------------------------------

-- | A text document.
-- type Document
--   = Text

-- | An edit to a text document.
data Delta
  = Delta Value -- Not parsed yet

instance FromJSON Delta where
  parseJSON :: Value -> Json.Decode.Parser Delta
  parseJSON value =
    pure (Delta value)

--------------------------------------------------------------------------------
-- Main event network logic
--------------------------------------------------------------------------------

moment :: Event (PendingConnection, ThreadId) -> MomentIO ()
moment eNewClient = do
  -- The "new input" event. Every time a client connects, this event fires,
  -- carrying that client's event of messages sent to the server.
  eNewInput :: Event (Event Delta) <-
    execute (handleNewClient <$> eNewClient)

  -- The input events of all of the connected clients. This event fires with the
  -- most up-to-date list of connected clients' input events.
  eInputs :: Event [Event Delta] <-
    accumE []
      (unions
        [ (:) <$> eNewInput
        ])

  -- The input sent to the server from all connected clients. Each time
  -- 'eInputs' fires, we switch to the event that unions all of the carried
  -- events together.
  eInput :: Event Delta <-
    switchE (foldr (unionWith const) never <$> eInputs)

  -- Debug: print when a client connects.
  reactimate (putStrLn "Client connected" <$ eNewInput)

  -- Debug: print the messages sent to the server.
  reactimate ((\(Delta val) -> putStrLn (pack (show val))) <$> eInput)

handleNewClient :: (PendingConnection, ThreadId) -> MomentIO (Event Delta)
handleNewClient (pconn, tid) = do
  -- Always accept every client
  conn :: Connection <-
    liftIO (WebSockets.acceptRequest pconn)

  -- Create a new Event that corresponds to this client's input sent to the
  -- server.
  (eDelta, fireDelta) :: (Event Delta, Delta -> IO ()) <-
    newEvent

  -- In a background thread, forward all input from the connected client to its
  -- Event. If receiving fails, kill the server thread.
  --
  -- TODO: Get disconnect event into the network somehow.
  (liftIO . void . forkIO)
    (do
      let
        loop :: IO ()
        loop = do
          bytes :: ByteString <-
            WebSockets.receiveData conn
          case Json.Decode.decodeStrict bytes of
            Nothing -> do
              putStrLn
                ("Failed to decode payload: " <>
                  Text.Partial.decodeUtf8 bytes)
            Just delta -> do
              fireDelta delta
              loop

      loop `finally` killThread tid)

  pure eDelta
