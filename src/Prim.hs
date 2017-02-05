module Prim where

import qualified Data.Text as T
import qualified Data.Map as Map

data Prim = Text T.Text
          | Number Integer
  deriving (Eq, Ord, Show)

