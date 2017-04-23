module Run where

import Prelude hiding (id, putStrLn)

import Control.Lens hiding (children, mapping)
import Control.Monad
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


import Debug.Trace

data ProjectionCode = ProjectionCode CodeDbId CodeDbType [ProjectionCode]
projectionCodeId :: ProjectionCode -> CodeDbId
projectionCodeId (ProjectionCode id _ _) = id

data Projection = Projection { projectionCode :: [ProjectionCode], projectionNames :: Map CodeDbId Text }

instance Monoid Projection where
  mempty = Projection [] Map.empty
  mappend (Projection code1 names1) (Projection code2 names2) = Projection (code1 ++ code2) (Map.union names1 names2)



data Error = ParseError (Lam.NameResolutionError CodeDbId) | RuntimeError (Eval.EvalError CodeDbId)
  deriving (Eq, Ord, Show)

evalWithTrail :: Lam.Resolved CodeDbId
              -> Eval.Env CodeDbId
              -> [Lam.BindingWithId CodeDbId]
              -> Map CodeDbId (Eval.Val CodeDbId)
              -> Lam.ExpWithId CodeDbId
              -> Eval.Trail CodeDbId
              -> Either [Error] (Eval.Trailing CodeDbId (Eval.Val CodeDbId))
evalWithTrail resolved toplevelEnv bindingsWithIds initialEnv mainExp trail =
  (_Left %~ map RuntimeError) $ Eval.eval resolved (Eval.GlobalEnv toplevelEnv) (Eval.Frame Nothing (CodeDbId "TOPFRAME") Map.empty) initialEnv trail mainExp

eval :: [Lam.BindingWithId CodeDbId]
     -> Map CodeDbId (Eval.Val CodeDbId)
     -> Either [Error] (Eval.Val CodeDbId)
eval bindingsWithIds initialEnv = do
  resolved <- (_Left %~ pure . ParseError) $ Lam.resolveVars (Lam.GlobalNames Builtins.functionIds Builtins.allArgIds) bindingsWithIds

  let maybeMainExp = fmap (\(_, _, _, exp) -> exp) . List.find (\(bindingId, boundId, name, exp) -> name == "main") $ (Eval.flattenBinding <$> bindingsWithIds)
  mainExp <- maybe (Left [RuntimeError $ Eval.UndefinedVar (CodeDbId "toplevel") "main"]) Right maybeMainExp

  let toplevelEnv = let assocs = (\(bindingId, boundId, name, exp) -> (boundId, Eval.Thunk Set.empty Map.empty $ Id.WithId bindingId $ Identity $ Eval.ThunkExp exp)) . Eval.flattenBinding <$> bindingsWithIds
                     in Map.unions [Map.fromList assocs, Builtins.env]

  iterateTrail mempty (evalWithTrail resolved toplevelEnv bindingsWithIds initialEnv mainExp)



iterateTrail :: Eval.Trail CodeDbId
             -> (Eval.Trail CodeDbId -> Either [Error] (Eval.Trailing CodeDbId (Eval.Val CodeDbId)))
             -> Either [Error] (Eval.Val CodeDbId)
iterateTrail trail1 next =
  do (Eval.Trailing newTrail result) <- next trail1
     let mergedTrails = Set.union trail1 newTrail
     if trail1 == mergedTrails
       then pure result
       else iterateTrail mergedTrails next

projectCode code = do
  bindingsWithIds <- runCodeDbIdGen $ mapM (Lam.bindingWithId nextCodeDbId) code
  projection <- eitherToError $ mapLeft (userError . show) $ bindingsWithIdsProjection bindingsWithIds
  pure (bindingsWithIds, projection)


runProjection :: [Lam.BindingWithId CodeDbId] -> (Either [Error] (Eval.Val CodeDbId))
runProjection bindingsWithIds = eval bindingsWithIds Map.empty
 
runProjectionWithEnv :: [Lam.BindingWithId CodeDbId]
                     -> Map CodeDbId (Eval.Val CodeDbId)
                     -> Either [Error] (Eval.Val CodeDbId)
runProjectionWithEnv bindingsWithIds initialEnv = 
  eval bindingsWithIds initialEnv

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

