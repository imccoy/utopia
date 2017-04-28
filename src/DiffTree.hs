module DiffTree (DiffTree(..), label, name, SrcNode(..), DstNode(..), diffTree, SrcNodeId, DstNodeId, srcNodeId, srcNodeIdText, dstNodeId, dstNodeIdText, srcNodeChildren, dstNodeChildren, humanReadableIds) where

import Prelude hiding (id)

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

import CodeTree (CodeTree)
import qualified CodeTree

data DiffTree = DiffTree { _diffTreeId :: Text, _label :: Text, _name :: Maybe Text, _diffTreeChildren :: [DiffTree] }
  deriving (Eq, Show)

newtype SrcNode = SrcNode { _srcNodeDiffTree :: DiffTree }
newtype DstNode = DstNode { _dstNodeDiffTree :: DiffTree }

makeLenses ''DiffTree
makeFields ''SrcNode
makeFields ''DstNode

newtype SrcNodeId = SrcNodeId Text
  deriving (Eq, Ord, Show)

srcNodeIdText :: SrcNodeId -> Text
srcNodeIdText (SrcNodeId t) = t

newtype DstNodeId = DstNodeId Text
  deriving (Eq, Ord, Show)

dstNodeIdText :: DstNodeId -> Text
dstNodeIdText (DstNodeId t) = t

srcNodeId :: Getter SrcNode SrcNodeId
srcNodeId = diffTree . diffTreeId . (to SrcNodeId)

dstNodeId :: Getter DstNode DstNodeId
dstNodeId = diffTree . diffTreeId . to DstNodeId


srcNodeChildren :: Getter SrcNode [SrcNode]
srcNodeChildren = diffTree . diffTreeChildren . (to (map SrcNode))

dstNodeChildren :: Getter DstNode [DstNode]
dstNodeChildren = diffTree . diffTreeChildren . (to (map DstNode))

humanReadableIds :: [CodeTree] -> [DiffTree]
humanReadableIds nodes = [ go (T.pack $ show n) node
                         | (n, node) <- zip ([(0 :: Int)..]) nodes
                         ]
  where go prefix codeTree = let id = prefix `T.append` "." `T.append` (codeTree ^. CodeTree.label)
                              in DiffTree id
                                          (codeTree ^. CodeTree.label)
                                          (codeTree ^. CodeTree.name)
                                          [ go (T.concat [id, "[", T.pack (show n), "]"]) child
                                          | (n, child) <- zip [(0 :: Int)..] (codeTree ^. CodeTree.children)
                                          ]

