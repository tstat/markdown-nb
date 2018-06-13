module Main
  ( main
  ) where

import Mitchell

import Concurrency (killThread)
import Exception (finally, onException)
import FRP
import Json.Decode (FromJSON, (.:), parseJSON, withObject)
import Json.Encode (ToJSON, Value, (.=), toJSON)
import Network.WebSockets (Connection, PendingConnection)
import Text (pack)

import qualified ByteString.Lazy as Lazy (ByteString)
import qualified Json.Decode as Json (Parser, decodeStrict)
import qualified Json.Encode
import qualified Json.Encode as Json (encode)
import qualified Network.WebSockets as WebSockets
import qualified Text
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
  , clientSend :: Lazy.ByteString -> IO ()
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
applyDelta delta document =
  case delta of
    Add off str ->
      case Text.splitAt off document of
        (xs, ys) ->
          xs <> str <> ys
    Del off len ->
      case Text.splitAt off document of
        (xs, ys) ->
          xs <> Text.drop len ys

-- | An edit to a text document.
data Delta
  = Add !Int !Text -- offset, text to add
  | Del !Int !Int  -- offset, number of chars to remove
  deriving Show

instance FromJSON Delta where
  parseJSON :: Value -> Json.Parser Delta
  parseJSON =
    withObject "Delta"
      (\o ->
        asum
          [ o .: "add" >>=
              withObject "Add"
                (\p ->
                  Add
                    <$> p .: "off"
                    <*> p .: "txt")
          , o .: "del" >>=
              withObject "Del"
                (\p ->
                  Del
                    <$> p .: "off"
                    <*> p .: "len")
          ])

instance ToJSON Delta where
  toJSON :: Delta -> Value
  toJSON = \case
    Add off text ->
      Json.Encode.object
        [ "add" .= Json.Encode.object
            [ "off" .= toJSON off
            , "txt" .= toJSON text
            ]
        ]
    Del off len ->
      Json.Encode.object
        [ "del" .= Json.Encode.object
            [ "off" .= toJSON off
            , "len" .= toJSON len
            ]
        ]

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
      ((\cid document pending ->
        handleNewClient (fireDisconnect cid) cid document pending)
        <$> bNextClientId
        <*> bDocument
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
  bClients :: Behavior [Client] <-
    stepper [] eClients

  -- The input sent to the server from all connected clients. Each time
  -- 'eClients' fires, we switch to the event that unions all of the clients'
  -- input events together.
  eInput :: Event Delta <-
    switchE (foldr (unionWith const . clientInput) never <$> eClients)

  -- The document.
  bDocument :: Behavior Document <-
    accumB "" (applyDelta <$> eInput)

  -- Forward delta to every connected client
  reactimate
    ((\clients delta ->
      traverse_
        (\client ->
          clientSend client (Json.encode delta)
            `onException` fireDisconnect (clientId client))
        clients)
      <$> bClients
      <@> eInput)

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
  reactimate (print <$> eInput)

handleNewClient
  :: IO ()
  -> ClientId
  -> Document
  -> (PendingConnection, ThreadId)
  -> MomentIO Client
handleNewClient fireDisconnect cid document (pconn, tid) = do
  -- Always accept every client
  conn :: Connection <-
    liftIO (WebSockets.acceptRequest pconn)

  -- Send the client the current document
  liftIO
    (WebSockets.sendTextData conn document
      `onException` fireDisconnect)

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
          case Json.decodeStrict bytes of
            Nothing -> do
              putStrLn
                ("Failed to decode payload: " <>
                  Text.Partial.decodeUtf8 bytes)
            Just delta -> do
              fireDelta delta
              loop

      loop `finally` do
        fireDisconnect
        killThread tid)

  pure Client
    { clientId = cid
    , clientInput = eDelta
    , clientSend = WebSockets.sendTextData conn
    }
