module DiffTree (DiffTree(..), label, name, SrcNode(..), DstNode(..), diffTree, SrcNodeId, DstNodeId, srcNodeId, dstNodeId, srcNodeChildren, dstNodeChildren) where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

data DiffTree = DiffTree { _diffTreeId :: Text, _label :: Text, _name :: Maybe Text, _diffTreeChildren :: [DiffTree] }
  deriving (Eq, Show)

newtype SrcNode = SrcNode { _srcNodeDiffTree :: DiffTree }
newtype DstNode = DstNode { _dstNodeDiffTree :: DiffTree }

makeLenses ''DiffTree
makeFields ''SrcNode
makeFields ''DstNode

newtype SrcNodeId = SrcNodeId Text
  deriving (Eq)

instance Show SrcNodeId where
  show (SrcNodeId id) = T.unpack id


newtype DstNodeId = DstNodeId Text
  deriving (Eq)

instance Show DstNodeId where
  show (DstNodeId id) = T.unpack id

srcNodeId :: Getter SrcNode SrcNodeId
srcNodeId = diffTree . diffTreeId . (to SrcNodeId)

dstNodeId :: Getter DstNode DstNodeId
dstNodeId = diffTree . diffTreeId . to DstNodeId


srcNodeChildren :: Getter SrcNode [SrcNode]
srcNodeChildren = diffTree . diffTreeChildren . (to (map SrcNode))

dstNodeChildren :: Getter DstNode [DstNode]
dstNodeChildren = diffTree . diffTreeChildren . (to (map DstNode))

