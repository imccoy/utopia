{-# LANGUAGE FlexibleContexts, GADTs #-}
module Id where

import Prelude hiding (id)

import Data.Functor.Foldable.Extended

data WithId i v = WithId { _id :: i, _value :: v }
  deriving (Show, Functor, Prelude.Foldable, Traversable)

mapId :: (i1 -> i2) -> (WithId i1 v) -> (WithId i2 v)
mapId f (WithId id v) = WithId (f id) v
