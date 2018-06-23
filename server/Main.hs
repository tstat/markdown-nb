module Main
  ( main
  ) where

import Mitchell

import Delta
import Document (Document)

import qualified Document

import Concurrency (killThread)
import Environment (getArgs)
import Exception (ExitCode(..), exitFailure, finally, onException)
import File (FilePath)
import FRP
import Network.WebSockets (Connection, PendingConnection)
import Process (exitImmediately)
import Text (pack)

import qualified ByteString.Lazy as Lazy (ByteString)
import qualified Json.Decode as Json
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
  -- Create the event network
  --

  (newClientAddHandler, fireNewClient) <-
    newAddHandler

  network :: EventNetwork <-
    compile $ do
      ePendingClient :: Event (PendingConnection, ThreadId) <-
        fromAddHandler newClientAddHandler

      moment file ePendingClient

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
      pure (Document.writeFile file document)

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
      Document.readFile file

    accumB document0 (Document.applyDelta <$> eInput)

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
  {-
  liftIO
    (WebSockets.sendTextData conn (Document.toText document)
      `onException` fireDisconnect)
  -}

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
          case Json.eitherDecodeStrict bytes of
            Left err -> do
              putStrLn
                ("Failed to decode payload: " <> pack err <> ": " <>
                  Text.Partial.decodeUtf8 bytes)
            Right delta -> do
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
