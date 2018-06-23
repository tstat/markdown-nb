-- | Ace editor 'Delta'.

module Delta
  ( Delta(..)
  , Insert(..)
  , Remove
  , Loc(..)
  ) where

import Mitchell

import Json.Decode
import Json.Encode
import Monad.Fail (fail)
import Text (unpack)
import Vector (Vector)

import qualified Map.Hash

--------------------------------------------------------------------------------
-- Delta
--------------------------------------------------------------------------------

data Delta
  = DeltaInsert Insert
  | DeltaRemove Remove
  deriving (Show)

instance FromJSON Delta where
  parseJSON :: Value -> Parser Delta
  parseJSON =
    withObject "delta" $ \o ->
      o .: "action" >>=
        withText "action" (\case
          "insert" ->
            DeltaInsert <$> parseJSON (Object o)
          "remove" ->
            DeltaRemove <$> parseJSON (Object o)
          s ->
            fail ("unknown action: " ++ unpack s))

-- TODO: use Encoding
instance ToJSON Delta where
  toJSON :: Delta -> Value
  toJSON = \case
    DeltaInsert x ->
      go "insert" x
    DeltaRemove x ->
      go "remove" x
   where
    go :: ToJSON a => Value -> a -> Value
    go s x =
      case toJSON x of
        Object o ->
          Object (Map.Hash.insert "action" s o)
        _ ->
          error "impossible"

--------------------------------------------------------------------------------
-- Insert
--------------------------------------------------------------------------------

data Insert = Insert
  { start :: !Loc
  , end   :: !Loc
  , lines :: !(Vector Text)
  } deriving anyclass (FromJSON, ToJSON)
    deriving stock (Show, Generic)

--------------------------------------------------------------------------------
-- Remove
--------------------------------------------------------------------------------

type Remove
  = Insert

--------------------------------------------------------------------------------
-- Loc
--------------------------------------------------------------------------------

data Loc = Loc
  { row    :: !Int
  , column :: !Int
  } deriving anyclass (FromJSON, ToJSON)
    deriving stock (Generic, Show)
