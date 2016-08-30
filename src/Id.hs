{-# LANGUAGE FlexibleContexts, GADTs #-}
module Id where

import Prelude hiding (id)

import Data.Functor.Foldable.Extended

data WithId i f v = WithId { _id :: i, _value :: f v }
  deriving (Show, Functor, Prelude.Foldable, Traversable)

instance (Eq i) => Eq (WithId i f v) where
  w1 == w2 = _id w1 == _id w2

instance (Ord i) => Ord (WithId i f v) where
  w1 `compare` w2 = _id w1 `compare` _id w2

mapId :: (i1 -> i2) -> (WithId i1 f v) -> (WithId i2 f v)
mapId f (WithId id v) = WithId (f id) v

withIdEq :: Eq i => WithId i f v1 -> WithId i f v2 -> Bool
withIdEq v1 v2 = _id v1 == _id v2
