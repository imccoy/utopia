module Prim where

import qualified Data.Text as T

data Prim = Text T.Text
          | Number Integer
  deriving (Eq, Ord, Show)

