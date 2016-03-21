{-# LANGUAGE FlexibleContexts #-}

module Data.Functor.Foldable.Extended 
  ( module Data.Functor.Foldable
  , cataM
  ) where

import Prelude hiding (Foldable)

import Control.Monad
import Data.Functor.Foldable
import Data.Traversable as T

-- from https://github.com/ekmett/recursion-schemes/issues/3
cataM
  :: (Foldable t, T.Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< T.mapM c <=< (return . project)

