module CodeDb where

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Diff
import qualified DiffTree

newtype CodeDbId = CodeDbId Text
  deriving (Ord, Eq, Show)
codeDbIdText (CodeDbId id) = id

dstNodeIdCodeDbId dstNodeId = CodeDbId (T.pack $ show dstNodeId)
mappingCodeDbId :: Getter Diff.Mapping CodeDbId
mappingCodeDbId = Diff.mappingDst . DiffTree.dstNodeId . to dstNodeIdCodeDbId

newtype CodeDbType = CodeDbType Text
  deriving (Ord, Eq, Show)
codeDbTypeText (CodeDbType ty) = ty

data CodeDbGraphEntry = CodeDbGraphEntry CodeDbType [CodeDbId]
type CodeDbGraph = Map CodeDbId CodeDbGraphEntry
data CodeDb = CodeDb { _codeDbGraph :: CodeDbGraph, _codeDbNames :: Map CodeDbId Text }

makeLenses ''CodeDb


