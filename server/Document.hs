-- | Ace editor 'Document'.

module Document
  ( Document
  , toText
  , applyDelta
    -- ** I/O
  , readFile
  , writeFile
  ) where

import Mitchell

import Delta

import Coerce (coerce)
import Control.Lens (over) -- TODO: export from mitchell-stdlib
import Control.Lens.Tuple -- TODO: export from mitchell-stdlib
import File (FilePath, IOMode(ReadMode))
import File.Text (hSetEncoding, utf8, withFile)
import Yi.Rope (YiString)

import qualified Yi.Rope as Yi

newtype Document
  = Document YiString
  deriving newtype (Monoid, Semigroup)

toText :: Document -> Text
toText =
  coerce Yi.toText

-- | Apply a 'Delta' to a 'Document'.
applyDelta :: Delta -> Document -> Document
applyDelta =
  coerce applyDelta_

applyDelta_ :: Delta -> YiString -> YiString
applyDelta_ delta rope =
  case delta of
    DeltaInsert Insertion{pos, content} ->
      let
        (xs, ys) =
          Yi.splitAt pos rope
      in
        mconcat
          [ xs
          , Yi.fromText content
          , ys
          ]

    DeltaRemove Deletion{pos, len}->
      let
        (xs, (_, ys)) =
          Yi.splitAt pos rope
            & over _2 (Yi.splitAt len)
      in
        xs <> ys

readFile :: MonadIO m => FilePath -> m Document
readFile path =
  liftIO $ do
    withFile path ReadMode $ \handle -> do
      hSetEncoding handle utf8
      Document . Yi.fromText <$> hGetContents handle

writeFile :: FilePath -> Document -> IO ()
writeFile =
  coerce Yi.writeFile
