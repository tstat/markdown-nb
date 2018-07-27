-- | Ace editor 'Delta'.

module Delta
  ( Delta(..)
  , Insertion(..)
  , Deletion(..)
  ) where

import Mitchell

import Json.Decode
import Json.Encode
import Monad.Fail (fail)
import Text (unpack)

import qualified Map.Hash

--------------------------------------------------------------------------------
-- Delta
--------------------------------------------------------------------------------

data Delta
  = DeltaInsert Insertion
  | DeltaRemove Deletion
  deriving (Show)

instance FromJSON Delta where
  parseJSON :: Value -> Parser Delta
  parseJSON =
    withObject "delta" $ \o ->
      o .: "type" >>=
        withText "type" (\case
          "insertion" ->
            DeltaInsert <$> parseJSON (Object o)
          "deletion" ->
            DeltaRemove <$> parseJSON (Object o)
          s ->
            fail ("unknown action: " ++ unpack s))

-- TODO: use Encoding
instance ToJSON Delta where
  toJSON :: Delta -> Value
  toJSON = \case
    DeltaInsert x ->
      go "insertion" x
    DeltaRemove x ->
      go "deletion" x
   where
    go :: ToJSON a => Value -> a -> Value
    go s x =
      case toJSON x of
        Object o ->
          Object (Map.Hash.insert "type" s o)
        _ ->
          error "impossible"

--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

data Insertion = Insertion
  { pos :: !Int
  , content :: !Text
  } deriving anyclass (FromJSON, ToJSON)
    deriving stock (Show, Generic)

--------------------------------------------------------------------------------
-- Deletion
--------------------------------------------------------------------------------

data Deletion = Deletion
  { pos :: !Int
  , len :: !Int
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
