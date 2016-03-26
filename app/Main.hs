module Main where

import Prelude hiding (id, putStrLn)

import Control.Lens hiding (children, mapping)
import Control.Monad.Reader
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import qualified Data.ByteString.Lazy as LB
import Data.IORef (IORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import qualified Data.UUID as UUID
import qualified Data.UUID.V1
import qualified Data.UUID.V4
import Safe hiding (at)
import System.IO (openTempFile, hClose)
import System.Process (spawnCommand)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Code
import CodeDb
import qualified Diff
import qualified DiffTree
import qualified Eval
import qualified Id
import qualified Lam

import qualified Html

data ProjectionCode = ProjectionCode CodeDbId CodeDbType [ProjectionCode]
projectionCodeId :: ProjectionCode -> CodeDbId
projectionCodeId (ProjectionCode id _ _) = id

data Projection = Projection { projectionCode :: [ProjectionCode], projectionNames :: Map CodeDbId Text }

instance Monoid Projection where
  mempty = Projection [] Map.empty
  mappend (Projection code1 names1) (Projection code2 names2) = Projection (code1 ++ code2) (Map.union names1 names2)

type CodeDbIdGen a = ReaderT Text IO a

runCodeDbIdGen :: CodeDbIdGen a -> IO a
runCodeDbIdGen f = do
  baseUUID <- getBaseUUID
  runReaderT f (UUID.toText baseUUID)

getBaseUUID :: IO UUID.UUID
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

main :: IO ()
main = do
  v1bindingsWithIds <- runCodeDbIdGen $ mapM (Lam.bindingWithId nextCodeDbId) Code.v1
  let v1projection = bindingsWithIdsProjection v1bindingsWithIds
  let initialDb = initFromProjection v1projection
  printProjection v1projection
  run $ do
    m_bindingsWithIds <- newModBy (\bs1 bs2 -> (Set.fromList . map Id._id $ bs1) == (Set.fromList . map Id._id $ bs2)) $ inM $ pure v1bindingsWithIds
    resolved <- inCh $ Lam.resolveVars <$> readMod m_bindingsWithIds
    current_bindingsWithMod <- inCh (mapM Eval.bindingWithMod =<< readMod m_bindingsWithIds)
    toplevelEnv <- do assocs <- inCh $ forM current_bindingsWithMod $  \binding -> do (id, name, exp) <- Eval.bindingExp binding
                                                                                      pure $ (id, Map.singleton name (Eval.ValExp exp))
                      pure $ Map.fromList assocs
    program <- inCh $ do let [Just valCh] = [ Map.lookup "main" frame | frame <- Map.elems toplevelEnv, Map.member "main" frame ]
                         let (Eval.ValExp program) = valCh
                         pure program
    eval <- inCh $ Eval.eval current_bindingsWithMod resolved program toplevelEnv []
    inM . putStrLn . T.pack . show $ eval
  

  v2bindingsWithIds <- runCodeDbIdGen $ mapM (Lam.bindingWithId nextCodeDbId) Code.v2
  let v2projection = bindingsWithIdsProjection v2bindingsWithIds
  printProjection v2projection

  let (reversedDiffResult, diffResult) = diffProjection initialDb v2projection
  putStrLn $ T.pack $ show diffResult

  let html = Html.mappingHtml reversedDiffResult diffResult
  (filePath, handle) <- openTempFile "." ".html"
  LB.hPut handle $ renderHtml html
  hClose handle
  putStrLn $ T.pack filePath
  void $ spawnCommand $ "open " ++ filePath
  return ()


projectionCodeDbIds :: ProjectionCode -> Set CodeDbId
projectionCodeDbIds (ProjectionCode id _ children) = id `Set.insert` (Set.unions $ map projectionCodeDbIds children)

bindingsWithIdsProjection :: [Lam.BindingWithId CodeDbId] -> Projection
bindingsWithIdsProjection bindingsWithIds = mconcat $ diffTreeProjection <$> Lam.bindingDiffTrees (Lam.mapBindingId codeDbIdText <$> bindingsWithIds)

diffTreeProjection :: DiffTree.DiffTree -> Projection
diffTreeProjection (DiffTree.DiffTree id label name children) = 
  let Projection childrenProjectionCode childNamesMap = foldMap diffTreeProjection children
      projectionCode = ProjectionCode (CodeDbId id) (CodeDbType label) childrenProjectionCode

      projectionNames = maybe childNamesMap (\childName -> Map.insert (CodeDbId id) childName childNamesMap) name
   in Projection [projectionCode] projectionNames

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
codeDbProjection (codeDbIds, codeDb) = Projection (map project codeDbIds) (codeDb ^. codeDbNames)
  where project nodeId = let (CodeDbGraphEntry ty children) = fromJustNote "EEEK" $ Map.lookup nodeId (codeDb ^. codeDbGraph)
                          in ProjectionCode nodeId ty $ map project children

codeDbDiffTree :: ([CodeDbId], CodeDb) -> DiffTree.DiffTree
codeDbDiffTree = projectionDiffTree . codeDbProjection 

mappedCodeDb :: [Diff.Mapping] -> CodeDb -> CodeDb
mappedCodeDb [] = \codeDb -> codeDb
mappedCodeDb (mapping:mappings)
  | (mapping ^. Diff.mappingCost) == 0 = mappedCodeDb mappings
  | otherwise = mappedCodeDb mappings
                  . mappedCodeDb (mapping ^. Diff.mappingChildren)
                  . set (codeDbGraph . at (mapping ^. mappingCodeDbId)) (Just newGraphEntry)
                  . set (codeDbNames . at (mapping ^. mappingCodeDbId)) (mapping ^. Diff.mappingDst . DiffTree.diffTree . DiffTree.name)
  where newGraphEntry = CodeDbGraphEntry (CodeDbType $ mapping ^. Diff.mappingDst . DiffTree.diffTree . DiffTree.label)
                                         [ child ^. mappingCodeDbId
                                         | child <- mapping ^. Diff.mappingChildren
                                         ]
