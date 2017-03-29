{-# LANGUAGE FlexibleContexts #-}

module Data.Functor.Foldable.Extended 
  ( module Data.Functor.Foldable
  , cataM
  , paraM
  ) where

import Prelude

import Control.Monad
import Data.Functor.Foldable
import Data.Traversable as T

-- from https://github.com/ekmett/recursion-schemes/issues/3
cataM
  :: (Recursive t, T.Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< T.mapM c <=< (return . project)

-- from http://jtobin.ca/monadic-recursion-schemes
paraM
  :: (Monad m, T.Traversable (Base t), Recursive t)
  => (Base t (t, a) -> m a) -> t -> m a
paraM alg = p where
  p   = alg <=< traverse f . project
  f t = liftM2 (,) (return t) (p t)
