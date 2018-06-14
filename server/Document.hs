module Document
  ( Document
  , fromText
  , toLazyText
  , drop
  , splitAt
  , write
  ) where

import Mitchell

import Bifunctor (first)
import Coerce (coerce)
import File (FilePath)
import Sequence (pattern (:<|))

import qualified Data.Text.Lazy.IO as Text.Lazy
import qualified Sequence as Seq
import qualified Text
import qualified Text.Lazy as Lazy (Text)
import qualified Text.Lazy

newtype Document
  = Document (Seq Text)
  deriving (Monoid, Semigroup)

fromText :: Text -> Document
fromText =
  Document . Seq.fromList . Text.chunksOf 1024

toLazyText :: Document -> Lazy.Text
toLazyText =
  coerce toLazyText_

toLazyText_ :: Seq Text -> Lazy.Text
toLazyText_ =
  Text.Lazy.fromChunks . toList

drop :: Int -> Document -> Document
drop =
  coerce drop_

drop_ :: Int -> Seq Text -> Seq Text
drop_ n = \case
  x :<| xs ->
    case compare n (Text.length x) of
      LT ->
        Text.drop n x :<| xs
      EQ ->
        xs
      GT ->
        drop_ (n - Text.length x) xs
  x ->
    x

splitAt :: Int -> Document -> (Document, Document)
splitAt =
  coerce splitAt_

splitAt_ :: Int -> Seq Text -> (Seq Text, Seq Text)
splitAt_ n = \case
  x :<| xs ->
    if n <= Text.length x
      then
        case Text.splitAt n x of
          (ys, zs) ->
            (pure ys, zs :<| xs)
      else
        first (x :<|) (splitAt_ (n - Text.length x) xs)
  x ->
    (x, x)

write :: FilePath -> Document -> IO ()
write file =
  Text.Lazy.writeFile file . toLazyText
