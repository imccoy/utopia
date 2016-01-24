module CodeTree where

import Control.Lens
import Data.Text (Text)

data CodeTree = CodeTree { _label :: Text, _name :: Maybe Text, _children :: [CodeTree] }
makeLenses ''CodeTree

 
