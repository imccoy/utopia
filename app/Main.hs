module Main where

import Prelude hiding (id, putStrLn)

import Control.Lens hiding (children, mapping)
import Control.Monad.Reader
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import qualified Data.ByteString.Lazy as LB
import Data.Either.Combinators (eitherToError, mapLeft)
import Data.IORef (IORef)
import qualified Data.List as List
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

import qualified Builtins
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

data Error = ParseError (Lam.NameResolutionError CodeDbId) | RuntimeError (Eval.EvalError CodeDbId)
  deriving (Eq, Ord, Show)

evalWithTrail :: Modifiable IO IORef [Lam.BindingWithId CodeDbId]
              -> Changeable IO IORef [Eval.BindingWithMod IO IORef CodeDbId]
              -> Modifiable IO IORef (Eval.Trail IO IORef CodeDbId)
              -> Changeable IO IORef (Modifiable IO IORef (Either [Error] (Eval.Trailing IO IORef CodeDbId (Eval.Val IO IORef CodeDbId))))
evalWithTrail m_bindingsWithIds ch_bindingsWithMod m_trail = do
  m_either_resolved <- newMod $ Lam.resolveVars (Lam.GlobalNames Builtins.functionIds Builtins.allArgIds) <$> readMod m_bindingsWithIds
  let ch_mainExp = (pure . fmap (\(_, _, exp) -> exp) . List.find (\(id, name, exp) -> name == "main")) =<< mapM Eval.flattenBinding =<< ch_bindingsWithMod
  let ch_toplevelEnv = do bindingsWithMod <- ch_bindingsWithMod
                          assocs <- forM bindingsWithMod $ \binding -> do (\(id, name, exp) -> (id, Eval.Thunk Set.empty Map.empty $ Eval.ThunkExp exp)) <$> Eval.flattenBinding binding
                          pure $ Map.union (Map.fromList assocs) (Builtins.env)
  newMod $ do mainExp <- ch_mainExp
              either_resolved <- readMod m_either_resolved
              case (mainExp, either_resolved) of
                (Just exp, Right resolved)  -> do m_resolved <- newMod $ inM $ pure resolved
                                                  evaluated <- Eval.evalAndVal m_resolved ch_toplevelEnv m_trail exp
                                                  pure $ (_Left %~ map RuntimeError) $ evaluated
                (Nothing, _) -> pure . Left $ [RuntimeError $ Eval.UndefinedVar (CodeDbId "toplevel") "main"]
                (_, Left resolveErrors) -> pure . Left $ [ParseError resolveErrors]

eval :: Modifiable IO IORef [Lam.BindingWithId CodeDbId]
     -> Changeable IO IORef [Eval.BindingWithMod IO IORef CodeDbId]
     -> Adaptive IO IORef (Either [Error] (Eval.Val IO IORef CodeDbId))
eval m_bindingsWithIds ch_bindingsWithMod = do
  trail <- newMod (pure mempty)
  evalResult <- inCh (evalWithTrail m_bindingsWithIds ch_bindingsWithMod trail)
  iterateTrail trail evalResult

iterateTrail :: Modifiable IO IORef (Eval.Trail IO IORef CodeDbId)
             -> Modifiable IO IORef (Either [Error] (Eval.Trailing IO IORef CodeDbId (Eval.Val IO IORef CodeDbId)))
             -> Adaptive IO IORef (Either [Error] (Eval.Val IO IORef CodeDbId))
iterateTrail m_trail m_evalResult = do
  inCh (readMod m_evalResult) >>= \case
    Right (Eval.Trailing newTrail result) -> do trail1 <- inCh (readMod m_trail)
                                                if trail1 == newTrail
                                                  then do inM $ Eval.printTrail newTrail
                                                          pure $ Right result
                                                  else do change m_trail newTrail
                                                          propagate
                                                          iterateTrail m_trail m_evalResult
    Left e -> pure $ Left e

main :: IO ()
main = do
  v1bindingsWithIds <- runCodeDbIdGen $ mapM (Lam.bindingWithId nextCodeDbId) Code.v1
  v1projection <- eitherToError $ mapLeft (userError . show) $ bindingsWithIdsProjection v1bindingsWithIds
  let initialDb = initFromProjection v1projection
  printProjection v1projection

  v2bindingsWithIds <- runCodeDbIdGen $ mapM (Lam.bindingWithId nextCodeDbId) Code.v2
  v2projection <- eitherToError $ mapLeft (userError . show) $ bindingsWithIdsProjection v2bindingsWithIds
  printProjection v2projection

  let (reversedDiffResult, diffResult) = diffProjection initialDb v2projection
  putStrLn $ T.pack $ show diffResult

  run $ do
    m_bindingsWithIds <- newModBy (\bs1 bs2 -> (Set.fromList . map Id._id $ bs1) == (Set.fromList . map Id._id $ bs2)) $ inM $ pure v1bindingsWithIds
    let ch_bindingsWithMod = mapM Eval.bindingWithMod =<< readMod m_bindingsWithIds

    let ch_evaluated = eval m_bindingsWithIds ch_bindingsWithMod

    inM . putStrLn . T.pack . show =<< ch_evaluated
  
    change m_bindingsWithIds v2bindingsWithIds
    propagate
    inM . putStrLn . T.pack . show =<< ch_evaluated

  run $ do
    m_bindingsWithIds <- newModBy (\bs1 bs2 -> (Set.fromList . map Id._id $ bs1) == (Set.fromList . map Id._id $ bs2)) $ inM $ pure v1bindingsWithIds
    m_bindingsWithMod <- newMod (mapM Eval.bindingWithMod =<< readMod m_bindingsWithIds)
    let ch_bindingsWithMod = readMod m_bindingsWithMod
    let ch_evaluated = eval m_bindingsWithIds ch_bindingsWithMod

    inM . putStrLn . T.pack . show =<< ch_evaluated
  
    m_v2bindingsWithIds <- newModBy (\bs1 bs2 -> (Set.fromList . map Id._id $ bs1) == (Set.fromList . map Id._id $ bs2)) $ inM $ pure v2bindingsWithIds
    let zeroCostMappings = Map.map (CodeDbId . DiffTree.dstNodeIdText) $ Map.mapKeys (CodeDbId . DiffTree.srcNodeIdText) $ Diff.zeroCostMappings diffResult
    let v2reuses = pure . Map.unions =<< mapM (\b -> Eval.bindingExp b >>= Eval.reuses zeroCostMappings) =<< ch_bindingsWithMod
    let ch_v2bindingsWithMod = do reuses <- v2reuses
                                  bindingsWithIds <- readMod m_v2bindingsWithIds
                                  mapM (\binding -> Eval.bindingWithModReusing reuses binding) bindingsWithIds
 
    change m_bindingsWithMod =<< inCh ch_v2bindingsWithMod
    change m_bindingsWithIds v2bindingsWithIds
    propagate
    inM . putStrLn . T.pack . show =<< ch_evaluated

  

  let html = Html.mappingHtml reversedDiffResult diffResult
  (filePath, handle) <- openTempFile "." ".html"
  LB.hPut handle $ renderHtml html
  hClose handle
  putStrLn $ T.pack filePath
  void $ spawnCommand $ "open " ++ filePath
  return ()


projectionCodeDbIds :: ProjectionCode -> Set CodeDbId
projectionCodeDbIds (ProjectionCode id _ children) = id `Set.insert` (Set.unions $ map projectionCodeDbIds children)

bindingsWithIdsProjection :: [Lam.BindingWithId CodeDbId] -> Either (Lam.NameResolutionError Text) Projection
bindingsWithIdsProjection bindingsWithIds = do
  diffTrees <- Lam.bindingDiffTrees (Lam.GlobalNames (codeDbIdText <$> Builtins.functionIds) (codeDbIdText <$> Builtins.allArgIds)) (Lam.mapBindingId codeDbIdText <$> bindingsWithIds)
  pure $ mconcat $ diffTreeProjection <$> diffTrees

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
