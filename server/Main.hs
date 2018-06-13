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
      ePendingClient :: Event (PendingConnection, ThreadId) <-
        fromAddHandler newClientAddHandler

      moment ePendingClient

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

type ClientId
  = Int

data Client = Client
  { clientId :: !ClientId
  , clientInput :: Event Delta
  }

deleteClient :: ClientId -> [Client] -> [Client]
deleteClient x = \case
  [] ->
    []
  y:ys | x == clientId y ->
    ys
  y:ys ->
    y : deleteClient x ys

-- | A text document.
type Document
  = Text

-- | Apply a 'Delta' to a 'Document'.
applyDelta :: Delta -> Document -> Document
applyDelta _ document =
  document

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
moment ePendingClient = mdo
  -- The "new client" event. Every time a pending client connects, this event
  -- fires, carrying that client's event of messages sent to the server.
  eConnect :: Event Client <- do
    bNextClientId :: Behavior ClientId <-
      accumB 0 ((+1) <$ ePendingClient)
    execute
      (handleNewClient fireDisconnect
        <$> bNextClientId
        <@> ePendingClient)

  -- Disconnect event; fires whenever a client disconnects (which we discover
  -- when we get an IO exception when attempting to send/recv to/from their
  -- websocket).
  (eDisconnect, fireDisconnect) :: (Event ClientId, ClientId -> IO ()) <-
    newEvent

  -- The connected clients. This event fires with the most up-to-date list of
  -- connected clients whenever it changes (connect/disconnect).
  eClients :: Event [Client] <-
    accumE []
      (unions
        [ (:) <$> eConnect
        , deleteClient <$> eDisconnect
        ])

  -- The input sent to the server from all connected clients. Each time
  -- 'eClients' fires, we switch to the event that unions all of the clients'
  -- input events together.
  eInput :: Event Delta <-
    switchE (foldr (unionWith const . clientInput) never <$> eClients)

  -- Debug: print when a client connects.
  reactimate
    ((\client ->
      putStrLn ("Client " <> pack (show (clientId client)) <> " connected")) <$>
        eConnect)

  -- Debug: print when a client connects.
  reactimate
    ((\cid ->
      putStrLn ("Client " <> pack (show cid) <> " disconnected")) <$>
        eDisconnect)

  -- Debug: print the messages sent to the server.
  reactimate ((\(Delta val) -> putStrLn (pack (show val))) <$> eInput)

handleNewClient
  :: (ClientId -> IO ())
  -> ClientId
  -> (PendingConnection, ThreadId)
  -> MomentIO Client
handleNewClient fireDisconnect cid (pconn, tid) = do
  -- Always accept every client
  conn :: Connection <-
    liftIO (WebSockets.acceptRequest pconn)

  -- Create a new Event that corresponds to this client's input sent to the
  -- server.
  (eDelta, fireDelta) :: (Event Delta, Delta -> IO ()) <-
    newEvent

  -- In a background thread, forward all input from the connected client to its
  -- Event. If receiving fails, fire a disconnect into the network and kill the
  -- server thread.
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

      loop `finally` do
        fireDisconnect cid
        killThread tid)

  pure Client
    { clientId = cid
    , clientInput = eDelta
    }
