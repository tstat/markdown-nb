-- | Ace editor 'Document'.

module Document
  ( Document
  , fromText
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

import qualified Vector
import qualified Yi.Rope as Yi

newtype Document
  = Document YiString
  deriving newtype (Monoid, Semigroup)

fromText :: Text -> Document
fromText =
  undefined
  -- Document . Seq.fromList . Text.chunksOf 1024

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
    DeltaInsert Insert{start = Loc{row, column}, lines} ->
      let
        (xs, (ys, zs)) =
          Yi.splitAtLine row rope
            & over _2 (Yi.splitAt column)
      in
        mconcat
          [ xs
          , ys
          , mconcat (((`Yi.snoc` '\n') . Yi.fromText) <$> Vector.toList lines)
          , zs
          ]

    DeltaRemove _ ->
      error "TODO: handle removes"

readFile :: MonadIO m => FilePath -> m Document
readFile path =
  liftIO $ do
    withFile path ReadMode $ \handle -> do
      hSetEncoding handle utf8
      Document . Yi.fromText <$> hGetContents handle

writeFile :: FilePath -> Document -> IO ()
writeFile =
  coerce Yi.writeFile
