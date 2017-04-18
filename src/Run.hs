module Run where

import Prelude hiding (id, putStrLn)

import Control.Lens hiding (children, mapping)
import Control.Monad
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import Data.Either.Combinators (eitherToError, mapLeft)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import Safe hiding (at)
import System.IO (openTempFile, hClose)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Builtins
import qualified Code
import CodeDb
import CodeDbIdGen
import qualified Diff
import qualified DiffTree
import qualified Eval
import qualified Id
import qualified Lam


data ProjectionCode = ProjectionCode CodeDbId CodeDbType [ProjectionCode]
projectionCodeId :: ProjectionCode -> CodeDbId
projectionCodeId (ProjectionCode id _ _) = id

data Projection = Projection { projectionCode :: [ProjectionCode], projectionNames :: Map CodeDbId Text }

instance Monoid Projection where
  mempty = Projection [] Map.empty
  mappend (Projection code1 names1) (Projection code2 names2) = Projection (code1 ++ code2) (Map.union names1 names2)



data Error = ParseError (Lam.NameResolutionError CodeDbId) | RuntimeError (Eval.EvalError CodeDbId)
  deriving (Eq, Ord, Show)

evalWithTrail :: Modifiable IO IORef [Lam.BindingWithId CodeDbId]
              -> Changeable IO IORef [Eval.BindingWithMod IO IORef CodeDbId]
              -> Changeable IO IORef (Map CodeDbId (Eval.Val IO IORef CodeDbId))
              -> Modifiable IO IORef (Eval.Trail IO IORef CodeDbId)
              -> Changeable IO IORef (Modifiable IO IORef (Either [Error] (Eval.Trailing IO IORef CodeDbId (Eval.Val IO IORef CodeDbId))))
evalWithTrail m_bindingsWithIds ch_bindingsWithMod ch_initialEnv m_trail = do
  m_either_resolved <- newMod $ Lam.resolveVars (Lam.GlobalNames Builtins.functionIds Builtins.allArgIds) <$> readMod m_bindingsWithIds
  let ch_mainExp = (pure . fmap (\(_, _, _, exp) -> exp) . List.find (\(bindingId, boundId, name, exp) -> name == "main")) =<< mapM Eval.flattenBinding =<< ch_bindingsWithMod
  let ch_toplevelEnv = do bindingsWithMod <- ch_bindingsWithMod
                          assocs <- forM bindingsWithMod $ \binding -> do (\(bindingId, boundId, name, exp) -> (boundId, Eval.Thunk Set.empty Map.empty $ Id.WithId bindingId $ Identity $ Eval.ThunkExp exp)) <$> Eval.flattenBinding binding
                          initialEnv <- ch_initialEnv
                          pure $ Map.unions [Map.fromList assocs, Builtins.env, initialEnv]
  newMod $ do mainExp <- ch_mainExp
              either_resolved <- readMod m_either_resolved
              case (mainExp, either_resolved) of
                (Just exp, Right resolved)  -> do m_resolved <- newMod $ inM $ pure resolved
                                                  ior_magicNumbers <- inM $ IORef.newIORef Map.empty

                                                  let magicNumber i env parentMagicNumbers = IORef.atomicModifyIORef ior_magicNumbers go
                                                        -- this is bad and I feel bad
                                                        where go map = case Map.lookup (i, env, parentMagicNumbers) map of
                                                                         Just x -> (map, x)
                                                                         Nothing -> let x = fromIntegral $ Map.size map
                                                                                     in (Map.insert (i, env, parentMagicNumbers) x map, x)

                                                  evaluated <- Eval.eval m_resolved magicNumber [] ch_toplevelEnv m_trail exp >>= readMod
                                                  pure $ (_Left %~ map RuntimeError) $ evaluated
                (Nothing, _) -> pure . Left $ [RuntimeError $ Eval.UndefinedVar (CodeDbId "toplevel") "main"]
                (_, Left resolveErrors) -> pure . Left $ [ParseError resolveErrors]

eval :: Modifiable IO IORef [Lam.BindingWithId CodeDbId]
     -> Changeable IO IORef [Eval.BindingWithMod IO IORef CodeDbId]
     -> Changeable IO IORef (Map CodeDbId (Eval.Val IO IORef CodeDbId))
     -> Adaptive IO IORef (Either [Error] (Eval.Val IO IORef CodeDbId))
eval m_bindingsWithIds ch_bindingsWithMod ch_env = do
  trail <- newMod (pure mempty)
  evalResult <- inCh (evalWithTrail m_bindingsWithIds ch_bindingsWithMod ch_env trail)
  iterateTrail trail evalResult



iterateTrail :: Modifiable IO IORef (Eval.Trail IO IORef CodeDbId)
             -> Modifiable IO IORef (Either [Error] (Eval.Trailing IO IORef CodeDbId (Eval.Val IO IORef CodeDbId)))
             -> Adaptive IO IORef (Either [Error] (Eval.Val IO IORef CodeDbId))
iterateTrail m_trail m_evalResult = do
  inCh (readMod m_evalResult) >>= 
    \case
      Right (Eval.Trailing newTrail result) -> do trail1 <- inCh (readMod m_trail)
                                                  let mergedTrails = Set.union trail1 newTrail
                                                  if trail1 == mergedTrails

                                                    then do -- inM $ putStrLn "ITERATE TRAIL DONE"
                                                            -- inM $ Eval.printTrail newTrail
                                                            pure $ Right result
                                                    else do -- inM $ putStrLn $ T.pack $ "ITERATE TRAIL" ++ show (Set.size trail1) ++ " " ++ show (Set.size newTrail) ++ " "
                                                            -- inM $ Eval.printTrail newTrail
                                                            change m_trail mergedTrails
                                                            propagate
                                                            iterateTrail m_trail m_evalResult
      Left e -> pure $ Left e

projectCode code = do
  bindingsWithIds <- runCodeDbIdGen $ mapM (Lam.bindingWithId nextCodeDbId) code
  projection <- eitherToError $ mapLeft (userError . show) $ bindingsWithIdsProjection bindingsWithIds
  pure (bindingsWithIds, projection)


runProjection :: [Lam.BindingWithId CodeDbId] -> Adaptive IO IORef (Modifiable IO IORef [Lam.BindingWithId CodeDbId], Adaptive IO IORef (Either [Error] (Eval.Val IO IORef CodeDbId)))
runProjection bindingsWithIds = do 
  m_bindingsWithIds <- newModBy (\bs1 bs2 -> (Set.fromList . map Id._id $ bs1) == (Set.fromList . map Id._id $ bs2)) $ inM $ pure bindingsWithIds
  let ch_bindingsWithMod = mapM Eval.bindingWithMod =<< readMod m_bindingsWithIds

  let ch_evaluated = eval m_bindingsWithIds ch_bindingsWithMod (pure Map.empty)

  pure (m_bindingsWithIds, ch_evaluated)
 
runProjectionWithEnv :: [Lam.BindingWithId CodeDbId]
                     -> Changeable IO IORef (Map CodeDbId (Eval.Val IO IORef CodeDbId))
                     -> Adaptive IO IORef (Modifiable IO IORef [Lam.BindingWithId CodeDbId], Adaptive IO IORef (Either [Error] (Eval.Val IO IORef CodeDbId)))
runProjectionWithEnv bindingsWithIds ch_initialEnv = do 
  m_bindingsWithIds <- newModBy (\bs1 bs2 -> (Set.fromList . map Id._id $ bs1) == (Set.fromList . map Id._id $ bs2)) $ inM $ pure bindingsWithIds
  let ch_bindingsWithMod = mapM Eval.bindingWithMod =<< readMod m_bindingsWithIds

  let ch_evaluated = eval m_bindingsWithIds ch_bindingsWithMod ch_initialEnv

  pure (m_bindingsWithIds, ch_evaluated)
 

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

