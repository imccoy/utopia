{-# LANGUAGE FlexibleContexts #-}

module Id where

import Prelude hiding (Foldable, id)

import Control.Monad
import Data.Functor.Foldable
import Data.Traversable as T

data WithId i v = WithId { _id :: i, _value :: v }
  deriving (Show, Functor)
data WithIdF i f v = WithIdF (WithId i (f v))
  deriving (Show, Functor)
type WithIdR i v = Fix (WithIdF i v)

withIdR :: i -> v (Fix (WithIdF i v)) -> Fix (WithIdF i v)
withIdR i v = Fix (WithIdF (WithId i v))

-- from https://github.com/ekmett/recursion-schemes/issues/3
cataM
  :: (Foldable t, T.Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< T.mapM c <=< (return . project)

withIdM :: (Monad m, Traversable v) => m i -> Fix v -> m (WithIdR i v)
withIdM gen = cataM $ \v -> do
  id <- gen
  pure $ withIdR id v

mapIds :: (Functor v) => (i1 -> i2) -> WithIdR i1 v -> WithIdR i2 v
mapIds f = cata $ \(WithIdF (WithId id v)) -> withIdR (f id) v
