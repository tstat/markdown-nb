-- | Debug client for testing. Forwards messages typed at the console to the
-- server.
--
-- To run:
--
--   runghc debug-client.hs
--
--   (requires .ghc.environment file shat out by cabal new-build)

{-# language NoImplicitPrelude   #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}

import Mitchell

import Concurrency
import File.Text (getLine)

import qualified Network.WebSockets as WebSockets

main :: IO ()
main = do
  putStrLn "Connecting to server at 127.0.0.1:8600/"
  WebSockets.runClient "127.0.0.1" 8600 "" $ \conn ->
    race_
      (forever $ do
        msg :: Text <-
          WebSockets.receiveData conn
        putStrLn ("RECV: " <> msg))
      (forever (getLine >>= WebSockets.sendTextData conn))
