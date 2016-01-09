module Main where

import Debug.Trace

import Prelude hiding (putStrLn)

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Lens ((^.), to)
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
import System.IO (openTempFile, hClose)
import System.Process (spawnCommand)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Lam
import qualified Code
import qualified CodeTree
import qualified Diff
import qualified DiffTree

import qualified Html

newtype CodeDbId = CodeDbId Text
  deriving (Ord, Eq, Show)
codeDbIdText (CodeDbId id) = id

dstNodeIdCodeDbId dstNodeId = CodeDbId (T.pack $ show dstNodeId)
mappingCodeDbId = Diff.mappingDst . DiffTree.dstNodeId . to dstNodeIdCodeDbId

newtype CodeDbType = CodeDbType Text
  deriving (Ord, Eq, Show)
codeDbTypeText (CodeDbType ty) = ty

data CodeDbGraphEntry = CodeDbGraphEntry CodeDbType [CodeDbId]
type CodeDbGraph = Map CodeDbId CodeDbGraphEntry
data CodeDb = CodeDb { codeDbGraph :: CodeDbGraph, codeDbNames :: Map CodeDbId Text }

data ProjectionCode = ProjectionCode CodeDbId CodeDbType [ProjectionCode]
projectionCodeId (ProjectionCode id _ _) = id

data Projection = Projection { projectionCode :: [ProjectionCode], projectionNames :: Map CodeDbId Text }

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

  v1projection <- runCodeDbIdGen $ codeTreeListProjection $ Lam.codeTree Code.v1
  let initialDb = initFromProjection v1projection
  printProjection v1projection

  v2projection <- runCodeDbIdGen $ codeTreeListProjection $ Lam.codeTree Code.v2
  printProjection v2projection

  let (reversedDiffResult, diffResult) = diffProjection initialDb v2projection
  putStrLn $ T.pack $ show diffResult

  let html = Html.mappingHtml reversedDiffResult diffResult
  (filePath, handle) <- openTempFile "." ".html"
  LB.hPut handle $ renderHtml html
  hClose handle
  putStrLn $ T.pack filePath
  spawnCommand $ "open " ++ filePath
  
  

projectionCodeDbIds :: ProjectionCode -> Set CodeDbId
projectionCodeDbIds (ProjectionCode id _ children) = id `Set.insert` (Set.unions $ map projectionCodeDbIds children)

codeTreeListProjection :: [CodeTree.CodeTree] -> CodeDbIdGen Projection
codeTreeListProjection codeTrees = do
  (projectionsCode, projectionsNames) <- mapM codeTreeProjection codeTrees >>= return . unzip
  return $ Projection projectionsCode (Map.unions projectionsNames)

codeTreeProjection :: CodeTree.CodeTree -> CodeDbIdGen (ProjectionCode, Map CodeDbId Text)
codeTreeProjection (CodeTree.CodeTree label name children) = do
  id <- nextCodeDbId
  (childrenProjectionCode, childNameMaps) <- mapM codeTreeProjection children >>= return . unzip
  let projectionCode = ProjectionCode id (CodeDbType label) childrenProjectionCode

  let childNamesMap = Map.unions childNameMaps
  let projectionNames = maybe childNamesMap (\name -> Map.insert id name childNamesMap) name
  return (projectionCode, projectionNames)

initDb :: [CodeTree.CodeTree] -> CodeDbIdGen ([CodeDbId], CodeDb)
initDb codeTree = codeTreeListProjection codeTree >>= return . initFromProjection

initFromProjection :: Projection -> ([CodeDbId], CodeDb)
initFromProjection Projection{..} = (id, CodeDb tree projectionNames)
  where (id, tree) = codeDbGraphFromProjectionCodeList projectionCode

codeDbGraphFromProjectionCodeList :: [ProjectionCode] -> ([CodeDbId], CodeDbGraph)
codeDbGraphFromProjectionCodeList projectionCodeList = (ids, Map.unions trees)
  where (ids, trees) = unzip $ map codeDbGraphFromProjectionCode $ projectionCodeList

codeDbGraphFromProjectionCode :: ProjectionCode -> (CodeDbId, CodeDbGraph)
codeDbGraphFromProjectionCode (ProjectionCode id ty children) = (id, Map.insert id entry childrenCodeDbGraph)
  where (childrenCodeDbIds, childrenCodeDbGraphs) = unzip $ map codeDbGraphFromProjectionCode children
        childrenCodeDbGraph = Map.unions childrenCodeDbGraphs
        entry = CodeDbGraphEntry ty childrenCodeDbIds

printProjection :: Projection -> IO ()
printProjection Projection{..} = mapM_ (printProjection' " ") projectionCode where
  printProjection' prefix (ProjectionCode id ty children) = do
    let name = Map.lookup id projectionNames
    putStrLn $ codeDbIdText id `T.append` prefix `T.append` codeDbTypeText ty `T.append` " " `T.append` fromJustDef "" name
    forM_ children (printProjection' (prefix `T.append` "  "))

diffProjection :: ([CodeDbId], CodeDb) -> Projection -> (Diff.ReverseMapping, Diff.Mapping)
diffProjection codeDb projection = let srcDiffTree = codeDbDiffTree codeDb
                                       mapping = Diff.diff (codeDbDiffTree codeDb) (projectionDiffTree projection)
                                    in (Diff.reverseMapping mapping (DiffTree.SrcNode srcDiffTree), mapping)

projectionDiffTree :: Projection -> DiffTree.DiffTree
projectionDiffTree Projection{..} = DiffTree.DiffTree "M.Projection" "Module" Nothing $ map go projectionCode
  where go (ProjectionCode id ty children) = DiffTree.DiffTree (codeDbIdText id) (codeDbTypeText ty) (Map.lookup id projectionNames) (map go children)

codeDbProjection :: ([CodeDbId], CodeDb) -> Projection
codeDbProjection (codeDbIds, CodeDb{..}) = Projection (map project codeDbIds) codeDbNames
  where project nodeId = let (CodeDbGraphEntry ty children) = fromJustNote "EEEK" $ Map.lookup nodeId codeDbGraph
                          in ProjectionCode nodeId ty $ map project children

codeDbDiffTree :: ([CodeDbId], CodeDb) -> DiffTree.DiffTree
codeDbDiffTree = projectionDiffTree . codeDbProjection 

type ChangedNodeInUse = (CodeDbId, DiffTree.SrcNodeId, DiffTree.DstNodeId)
type DeletedNodeInUse = (CodeDbId, DiffTree.SrcNodeId)

alterCodeDbGraph :: [Diff.Mapping] -> CodeDbGraph -> CodeDbGraph
alterCodeDbGraph [] codeDb = codeDb 
alterCodeDbGraph (mapping:mappings) codeDb
  | Map.member (mapping ^. mappingCodeDbId) codeDb = alterCodeDbGraph mappings codeDb
  | otherwise = alterCodeDbGraph mappings
                  $ Map.insert (mapping ^. mappingCodeDbId)
                               (CodeDbGraphEntry (CodeDbType $ mapping ^. Diff.mappingDst . DiffTree.diffTree . DiffTree.label)
                                                 [ child ^. mappingCodeDbId
                                                 | child <- mapping ^. Diff.mappingChildren
                                                 ])
                  $ alterCodeDbGraph (mapping ^. Diff.mappingChildren)
                  $ codeDb
