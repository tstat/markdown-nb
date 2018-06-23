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
import Cons (_head, _last)
import Control.Lens (over, view) -- TODO: export from mitchell-stdlib
import Control.Lens.Tuple -- TODO: export from mitchell-stdlib
import File (FilePath, IOMode(ReadMode))
import File.Text (hSetEncoding, utf8, withFile)
import Yi.Rope (YiString)

import qualified Vector
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
    DeltaInsert Insert{start = Loc{row, column}, lines} ->
      let
        (xs, (ys, zs)) =
          Yi.splitAtLine row rope
            & over _2 (Yi.splitAt column)
      in
        mconcat
          [ xs
          , ys
          , Yi.unlines (Yi.fromText <$> Vector.toList lines)
          , zs
          ]

    DeltaRemove Insert{start, end}->
      let
        (xs, (ys, zs)) =
          Yi.splitAtLine (row start) rope
            & over _2 (Yi.splitAtLine (row end - row start + 1))
            & over (_2._1) (mconcat . f . Yi.lines')

        f :: [YiString] -> [YiString]
        f = \case
          [] ->
            error "zero rows"
          [leg] ->
            leg
              & Yi.splitAt (column start)
              & over _2 (Yi.splitAt (column end - column start))
              & view (_1 <> _2._2)
              & pure
          legs ->
            legs
              & over _last (Yi.drop (column end))
              & over _head (Yi.take (column start))
      in
        xs <> ys <> zs

readFile :: MonadIO m => FilePath -> m Document
readFile path =
  liftIO $ do
    withFile path ReadMode $ \handle -> do
      hSetEncoding handle utf8
      Document . Yi.fromText <$> hGetContents handle

writeFile :: FilePath -> Document -> IO ()
writeFile =
  coerce Yi.writeFile
