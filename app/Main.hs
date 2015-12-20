module Main where

import Debug.Trace

import Prelude hiding (putStrLn)

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Lens ((^.))
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
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
import qualified Diff
import qualified DiffTree

data CodeTree = CodeTree (Maybe Text) Text [CodeTree]

class ToCodeTree a where
  toCodeTree :: a -> CodeTree

instance ToCodeTree Lam.Module where
  toCodeTree (Lam.Module bindings) = CodeTree Nothing "Module" $ map toCodeTree bindings

instance ToCodeTree Lam.Binding where
  toCodeTree (Lam.Binding name exp) = CodeTree (Just name) "Binding" [toCodeTree exp]

instance ToCodeTree Lam.Exp where
  toCodeTree (Lam.Lit literal) = toCodeTree literal
  toCodeTree (Lam.Var name) = CodeTree (Just name) "Var" []
  toCodeTree (Lam.Lam args exp) = CodeTree Nothing "Lam" $ (map arg args) ++ [CodeTree Nothing "LamBody" [toCodeTree exp]]
    where arg name = CodeTree (Just name) "LamArg" []
  toCodeTree (Lam.App body args) = CodeTree Nothing "App" $ (CodeTree Nothing "AppBody" [toCodeTree body]):(map arg args)
    where arg exp = CodeTree Nothing "AppArg" [toCodeTree exp]

instance ToCodeTree Lam.Literal where
  toCodeTree (Lam.Number n) = CodeTree (Just $ T.pack $ show n) "LiteralNumber" []
  toCodeTree (Lam.Text t) = CodeTree (Just t) "LiteralText" []

newtype CodeDbId = CodeDbId Text
  deriving (Ord, Eq, Show)
codeDbIdText (CodeDbId id) = id

newtype CodeDbType = CodeDbType Text
  deriving (Ord, Eq, Show)
codeDbTypeText (CodeDbType ty) = ty

data CodeDbTreeEntry = CodeDbTreeEntry CodeDbType [CodeDbId]
data CodeDb = CodeDb { codeDbTree :: Map CodeDbId CodeDbTreeEntry, codeDbNames :: Map CodeDbId Text }

data ProjectionCode = ProjectionCode CodeDbId CodeDbType [ProjectionCode]
projectionCodeId (ProjectionCode id _ _) = id

data Projection = Projection { projectionCode :: ProjectionCode, projectionNames :: Map CodeDbId Text }

type CodeDbIdGen a = ReaderT Text IO a

data SrcMapping = Delete | SrcChange CodeDbId | SrcMove CodeDbId
  deriving (Show)
data DstMapping = Add | DstChange CodeDbId | DstMove CodeDbId
  deriving (Show)

type Mappings = (Map CodeDbId SrcMapping, Map CodeDbId DstMapping)

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

main = do
  let v1 = Lam.diffTree Code.v1
  let v2 = Lam.diffTree Code.v2
  let diffResult = Diff.diff v1 v2
  putStrLn $ T.pack $ show diffResult
--  v1 <- runCodeDbIdGen $ ingest $ toCodeTree Code.v1
--  printProjection v1
--  v2 <- runCodeDbIdGen $ ingest $ toCodeTree Code.v2
--  printProjection v2
--  diffResult <- diffProjections v1 v2
--  case diffResult of
--    (Left e) -> putStrLn . T.pack $ "Couldn't parse diff: " ++ e
--    (Right (srcMappings, dstMappings)) -> do
--      putStrLn $ T.pack $ unlines $ map show $ Map.assocs srcMappings
--      putStrLn $ T.pack $ unlines $ map show $ Map.assocs dstMappings

projectionIntKeys :: [Projection] -> Bimap CodeDbId Integer
projectionIntKeys projections = let allKeys = Set.unions $ map (projectionCodeDbIds . projectionCode) projections
                                    assocs = zip (Set.toList allKeys) [0..] 
                                 in Bimap.fromList assocs

projectionCodeDbIds :: ProjectionCode -> Set CodeDbId
projectionCodeDbIds (ProjectionCode id _ children) = id `Set.insert` (Set.unions $ map projectionCodeDbIds children)

ingest :: CodeTree -> CodeDbIdGen Projection
ingest (CodeTree name ty children) = do
  id <- nextCodeDbId
  childProjections <- mapM ingest children
  let childrenProjectionCode = map projectionCode childProjections
  let projectionCode = ProjectionCode id (CodeDbType ty) childrenProjectionCode

  let childNameMaps = map projectionNames childProjections
  let childNamesMap = Map.unions childNameMaps
  let projectionNames = maybe childNamesMap (\name -> Map.insert id name childNamesMap) name
  return $ Projection { projectionCode, projectionNames }

printProjection :: Projection -> IO ()
printProjection Projection{..} = printProjection' " " projectionCode where
  printProjection' prefix (ProjectionCode id ty children) = do
    let name = Map.lookup id projectionNames
    putStrLn $ codeDbIdText id `T.append` prefix `T.append` codeDbTypeText ty `T.append` " " `T.append` fromJustDef "" name
    forM_ children (printProjection' (prefix `T.append` "  "))

newCodeDb :: Projection -> CodeDb
newCodeDb Projection{..} = CodeDb { codeDbTree = treeOf projectionCode, codeDbNames = projectionNames }
  where treeOf (ProjectionCode id ty children) = let childTrees = Map.unions $ map treeOf children
                                                     childIds = map projectionCodeId children
                                                  in Map.insert id (CodeDbTreeEntry ty childIds) childTrees


--alterCodeDb :: Projection -> Mappings -> CodeDb -> CodeDb
--alterCodeDb Projection{..} (srcMappings, dstMappings) codeDb = 
