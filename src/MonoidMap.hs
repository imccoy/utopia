module MonoidMap where

import Data.Map (Map)
import qualified Data.Map as Map

newtype MonoidMap k v = MonoidMap { unMonoidMap :: Map k v }
  deriving (Eq, Ord, Show)

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = empty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap (Map.unionWith mappend a b)

empty :: MonoidMap k v
empty = MonoidMap Map.empty

singleton :: k -> v -> MonoidMap k v
singleton k v = MonoidMap (Map.singleton k v)
