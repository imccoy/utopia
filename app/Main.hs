module Main where

import Prelude hiding (id, putStrLn)

import Control.Lens hiding (children, mapping)
import Control.Monad.Reader
import Control.Monad.Adaptive
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.IO
import Safe hiding (at)
--import System.IO (openTempFile, hClose)
--import System.Process (spawnCommand)
--import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Code
import CodeDb
import qualified Diff
import qualified DiffTree
import qualified Eval
import Run (Projection(..), projectionCode, projectionNames, ProjectionCode(..), projectCode, runProjection)

--import qualified Html

main :: IO ()
main = do
--  (v1bindingsWithIds, v1projection) <- projectCode Code.v1
--  putStrLn "v1========="
--  let initialDb = initFromProjection v1projection
--  printProjection v1projection
--
--  putStrLn "v2========="
--  (v2bindingsWithIds, v2projection) <- projectCode Code.v2
--  printProjection v2projection

  putStrLn "oneshoty====="
  (oneshotBindingsWithIds, oneshotProjection) <- projectCode Code.oneshot
  printProjection oneshotProjection

--  let (reversedDiffResult, diffResult) = diffProjection initialDb v2projection
--  putStrLn $ T.pack $ show diffResult

--  run $ do
--    (m_bindingsWithIds, ch_evaluated) <- runProjection v1bindingsWithIds
--    inM . putStrLn . T.pack . show =<< ch_evaluated
--  
--    change m_bindingsWithIds v2bindingsWithIds
--    propagate
--    inM . putStrLn . T.pack . show =<< ch_evaluated
--
--  run $ do
--    m_bindingsWithIds <- newModBy (\bs1 bs2 -> (Set.fromList . map Id._id $ bs1) == (Set.fromList . map Id._id $ bs2)) $ inM $ pure v1bindingsWithIds
--    m_bindingsWithMod <- newMod (mapM Eval.bindingWithMod =<< readMod m_bindingsWithIds)
--    let ch_bindingsWithMod = readMod m_bindingsWithMod
--    let ch_evaluated = eval m_bindingsWithIds ch_bindingsWithMod
--
--    inM . putStrLn . T.pack . show =<< ch_evaluated
--  
--    m_v2bindingsWithIds <- newModBy (\bs1 bs2 -> (Set.fromList . map Id._id $ bs1) == (Set.fromList . map Id._id $ bs2)) $ inM $ pure v2bindingsWithIds
--    let zeroCostMappings = Map.map (CodeDbId . DiffTree.dstNodeIdText) $ Map.mapKeys (CodeDbId . DiffTree.srcNodeIdText) $ Diff.zeroCostMappings diffResult
--    let v2reuses = pure . Map.unions =<< mapM (\b -> Eval.bindingExp b >>= Eval.reuses zeroCostMappings) =<< ch_bindingsWithMod
--    let ch_v2bindingsWithMod = do reuses <- v2reuses
--                                  bindingsWithIds <- readMod m_v2bindingsWithIds
--                                  mapM (\binding -> Eval.bindingWithModReusing reuses binding) bindingsWithIds
-- 
--    change m_bindingsWithMod =<< inCh ch_v2bindingsWithMod
--    change m_bindingsWithIds v2bindingsWithIds
--    propagate
--    inM . putStrLn . T.pack . show =<< ch_evaluated

  run $ do
    (m_bindingsWithIds, ch_evaluated) <- runProjection oneshotBindingsWithIds
    inM . putStrLn . T.pack . (either show Eval.pprintVal) =<< ch_evaluated
    

--  let html = renderHtml $ Html.mappingHtml reversedDiffResult diffResult

--  (filePath, handle) <- openTempFile "." ".html"
--  LB.hPut handle html
--  hClose handle
--  putStrLn $ T.pack filePath
--  void $ spawnCommand $ "open " ++ filePath
  return ()


projectionCodeDbIds :: ProjectionCode -> Set CodeDbId
projectionCodeDbIds (ProjectionCode id _ children) = id `Set.insert` (Set.unions $ map projectionCodeDbIds children)

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
