{-# LANGUAGE FlexibleContexts, GADTs #-}
module Id where

import Prelude hiding (id)

import Data.Functor.Foldable.Extended

data WithId i v = WithId { _id :: i, _value :: v }
  deriving (Show, Functor, Prelude.Foldable, Traversable)

mapId :: (i1 -> i2) -> (WithId i1 v) -> (WithId i2 v)
mapId f (WithId id v) = WithId (f id) v

withIdEq :: Eq i => WithId i v1 -> WithId i v2 -> Bool
withIdEq v1 v2 = _id v1 == _id v2
