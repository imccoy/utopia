module Main where

import Debug.Trace

import Prelude hiding (putStrLn)

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Lens ((^.))
import Control.Monad.Reader
import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import Data.Text.Lazy.Builder
import qualified Data.UUID as UUID
import qualified Data.UUID.V1
import qualified Data.UUID.V4
import Safe

import qualified Lam
import qualified Code
import qualified CodeTree
import qualified Diff
import qualified DiffTree

newtype CodeDbId = CodeDbId Text
  deriving (Ord, Eq, Show)
codeDbIdText (CodeDbId id) = id

newtype CodeDbType = CodeDbType Text
  deriving (Ord, Eq, Show)
codeDbTypeText (CodeDbType ty) = ty

data CodeDbTreeEntry = CodeDbTreeEntry CodeDbType [CodeDbId]
type CodeDbTree = Map CodeDbId CodeDbTreeEntry
data CodeDb = CodeDb { codeDbTree :: CodeDbTree, codeDbNames :: Map CodeDbId Text }

data ProjectionCode = ProjectionCode CodeDbId CodeDbType [ProjectionCode]
projectionCodeId (ProjectionCode id _ _) = id

data Projection = Projection { projectionCode :: [ProjectionCode], projectionNames :: Map CodeDbId Text }

data SrcMapping = Delete | SrcChange CodeDbId | SrcMove CodeDbId
  deriving (Show)
data DstMapping = Add | DstChange CodeDbId | DstMove CodeDbId
  deriving (Show)

type Mappings = (Map CodeDbId SrcMapping, Map CodeDbId DstMapping)

type CodeDbIdGen a = ReaderT Text IO a

runCodeDbIdGen f = do
  baseUUID <- getBaseUUID
  runReaderT f (UUID.toText baseUUID)

getBaseUUID = do
  baseUUID <- Data.UUID.V1.nextUUID
  case baseUUID of
    Just u -> return u
    Nothing -> do
      Data.UUID.V4.nextRandom

nextCodeDbId :: CodeDbIdGen CodeDbId
nextCodeDbId = do
  baseUUID <- ask
  tailUUID <- liftIO Data.UUID.V4.nextRandom
  return $ CodeDbId $ baseUUID `T.append` "-" `T.append` UUID.toText tailUUID

withIds :: CodeTree.CodeTree -> CodeDbIdGen DiffTree.DiffTree
withIds codeTree = do
  id <- nextCodeDbId
  children <- mapM withIds (codeTree ^. CodeTree.children)
  pure $ DiffTree.DiffTree (codeDbIdText id) (codeTree ^. CodeTree.label) (codeTree ^. CodeTree.name) children

main = do
  let v1 = DiffTree.humanReadableIds $ Lam.codeTree Code.v1
  let v2 = DiffTree.humanReadableIds $ Lam.codeTree Code.v2
  let diffResult = Diff.diff (DiffTree.DiffTree "M1" "Module" Nothing v1)
                             (DiffTree.DiffTree "M2" "Module" Nothing v2)
  putStrLn $ T.pack $ show diffResult

  v1projection <- runCodeDbIdGen $ codeTreeProjection $ Lam.codeTree Code.v1
  let initialDb = initFromProjection v1projection
  printProjection v1projection

  v2projection <- runCodeDbIdGen $ codeTreeProjection $ Lam.codeTree Code.v2
  printProjection v2projection

  let diffResult = diffProjection initialDb v2projection
  putStrLn $ T.pack $ show diffResult

projectionCodeDbIds :: ProjectionCode -> Set CodeDbId
projectionCodeDbIds (ProjectionCode id _ children) = id `Set.insert` (Set.unions $ map projectionCodeDbIds children)

codeTreeProjection :: [CodeTree.CodeTree] -> CodeDbIdGen Projection
codeTreeProjection codeTrees = do
  (projectionsCode, projectionsNames) <- mapM codeTreeProjection' codeTrees >>= return . unzip
  return $ Projection projectionsCode (Map.unions projectionsNames)

codeTreeProjection' :: CodeTree.CodeTree -> CodeDbIdGen (ProjectionCode, Map CodeDbId Text)
codeTreeProjection' (CodeTree.CodeTree label name children) = do
  id <- nextCodeDbId
  (childrenProjectionCode, childNameMaps) <- mapM codeTreeProjection' children >>= return . unzip
  let projectionCode = ProjectionCode id (CodeDbType label) childrenProjectionCode

  let childNamesMap = Map.unions childNameMaps
  let projectionNames = maybe childNamesMap (\name -> Map.insert id name childNamesMap) name
  return (projectionCode, projectionNames)

initDb :: [CodeTree.CodeTree] -> CodeDbIdGen ([CodeDbId], CodeDb)
initDb codeTree = codeTreeProjection codeTree >>= return . initFromProjection

initFromProjection :: Projection -> ([CodeDbId], CodeDb)
initFromProjection Projection{..} = (id, CodeDb tree projectionNames)
  where (id, tree) = codeDbTreeFromProjectionCodeList projectionCode

codeDbTreeFromProjectionCodeList :: [ProjectionCode] -> ([CodeDbId], CodeDbTree)
codeDbTreeFromProjectionCodeList projectionCodeList = (ids, Map.unions trees)
  where (ids, trees) = unzip $ map codeDbTreeFromProjectionCode $ projectionCodeList

codeDbTreeFromProjectionCode :: ProjectionCode -> (CodeDbId, CodeDbTree)
codeDbTreeFromProjectionCode (ProjectionCode id ty children) = (id, Map.insert id entry childrenCodeDbTree)
  where (childrenCodeDbIds, childrenCodeDbTrees) = unzip $ map codeDbTreeFromProjectionCode children
        childrenCodeDbTree = Map.unions childrenCodeDbTrees
        entry = CodeDbTreeEntry ty childrenCodeDbIds

printProjection :: Projection -> IO ()
printProjection Projection{..} = mapM_ (printProjection' " ") projectionCode where
  printProjection' prefix (ProjectionCode id ty children) = do
    let name = Map.lookup id projectionNames
    putStrLn $ codeDbIdText id `T.append` prefix `T.append` codeDbTypeText ty `T.append` " " `T.append` fromJustDef "" name
    forM_ children (printProjection' (prefix `T.append` "  "))

diffProjection :: ([CodeDbId], CodeDb) -> Projection -> Diff.Mapping
diffProjection codeDb projection = Diff.diff (codeDbDiffTree codeDb) (projectionDiffTree projection)

projectionDiffTree :: Projection -> DiffTree.DiffTree
projectionDiffTree Projection{..} = DiffTree.DiffTree "M.Projection" "Module" Nothing $ map go projectionCode
  where go (ProjectionCode id ty children) = DiffTree.DiffTree (codeDbIdText id) (codeDbTypeText ty) (Map.lookup id projectionNames) (map go children)

codeDbProjection :: ([CodeDbId], CodeDb) -> Projection
codeDbProjection (codeDbIds, CodeDb{..}) = Projection (map project codeDbIds) codeDbNames
  where roots = let parentedNodes = Set.unions [ Set.fromList children
                                                          | (CodeDbTreeEntry _ children) <- Map.elems codeDbTree]
                            in Map.filterWithKey (\k a -> k `Set.member` parentedNodes) codeDbTree
        project nodeId = let (CodeDbTreeEntry ty children) = fromJustNote "EEEK" $ Map.lookup nodeId codeDbTree
                          in ProjectionCode nodeId ty $ map project children

codeDbDiffTree :: ([CodeDbId], CodeDb) -> DiffTree.DiffTree
codeDbDiffTree = projectionDiffTree . codeDbProjection 

--alterCodeDb :: Projection -> Mappings -> CodeDb -> CodeDb
--alterCodeDb Projection{..} (srcMappings, dstMappings) codeDb = 
