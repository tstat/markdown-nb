module Main
  ( main
  ) where

import Mitchell

import Document (Document)

import qualified Document

import Concurrency (killThread)
import Environment (getArgs)
import Exception (ExitCode(..), exitFailure, finally, onException)
import File (FilePath)
import File.Text (readFile)
import FRP
import Json.Decode (FromJSON, (.:), parseJSON, withObject)
import Json.Encode (ToJSON, Value, (.=), toJSON)
import Network.WebSockets (Connection, PendingConnection)
import Process (exitImmediately)
import Text (pack)

import qualified ByteString.Lazy as Lazy (ByteString)
import qualified Json.Decode as Json (Parser, decodeStrict)
import qualified Json.Encode
import qualified Json.Encode as Json (encode)
import qualified Network.WebSockets as WebSockets
import qualified Text.Partial

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  --
  -- Parse command-line arguments
  --

  file :: FilePath <-
    getArgs >>= \case
      [file] ->
        pure file
      _ -> do
        hPutStrLn stderr "Usage: markdown-nb-server FILE"
        exitFailure

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

      moment file ePendingClient

  --
  -- Actuate the event network
  --

  actuate network

  --
  -- Accept clients forever
  --

  putStrLn ("Serving " <> pack file <> " on 0.0.0.0:8600")

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

-- | Apply a 'Delta' to a 'Document'.
applyDelta :: Delta -> Document -> Document
applyDelta delta document =
  case delta of
    Add off str ->
      case Document.splitAt off document of
        (xs, ys) ->
          xs <> Document.fromText str <> ys

    Del off len ->
      case Document.splitAt off document of
        (xs, ys) ->
          xs <> Document.drop len ys

--------------------------------------------------------------------------------
-- Main event network logic
--------------------------------------------------------------------------------

moment :: FilePath -> Event (PendingConnection, ThreadId) -> MomentIO ()
moment file ePendingClient = mdo
  -- Create a "write" event that fires every few seconds. When it fires, write
  -- the file to disk. If writing fails, the server dies.
  do
    (eWrite, fireWrite) <-
      newEvent

    (liftIO . void . forkIO . forever) $ do
      threadDelay (3*1000*1000)
      fireWrite () `onException`
        exitImmediately (ExitFailure 1)

    on eWrite $ \_ -> do
      document :: Document <-
        valueB bDocument
      pure (Document.write file document)

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
  bDocument :: Behavior Document <- do
    document0 :: Document <-
      liftIO (Document.fromText <$> readFile file)

    accumB document0 (applyDelta <$> eInput)

  -- Forward delta to every connected client
  on eInput $ \delta -> do
    clients :: [Client] <-
      valueB bClients
    pure $
      traverse_
        (\client ->
          clientSend client (Json.encode delta)
            `onException` fireDisconnect (clientId client))
        clients

  -- Debug: print when a client connects.
  on eConnect $ \client ->
    pure (putStrLn ("Client " <> pack (show (clientId client)) <> " connected"))

  -- Debug: print when a client connects.
  on eDisconnect $ \cid ->
    pure (putStrLn ("Client " <> pack (show cid) <> " disconnected"))

  -- Debug: print the messages sent to the server.
  on eInput $ \msg ->
    pure (print msg)

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
    (WebSockets.sendTextData conn (Document.toLazyText document)
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

on :: Event a -> (a -> Moment (IO ())) -> MomentIO ()
on event callback =
  reactimate (observeE (callback <$> event))
