{-# LANGUAGE ScopedTypeVariables #-}

-- THIS IS A BUSTED BROKE-DOWN INCOMPLETE HALF-BAKED SKETCH OF A THING
-- THAT MAY SOME DAY DO SOMETHING GOOD BUT TODAY IS NOT THAT DAY

module Types where

import           Control.Lens
import           Control.Monad (foldM)
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State.Lazy
import           Data.Functor.Foldable.Extended
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Text (Text)

import qualified Eval
import qualified Id
import qualified Lam

data TypeError i = AlreadyVisitedError i | UnresolvedId i Text | UndefinedVar i Text

data TyVar = TV Integer Text
  deriving (Ord, Eq, Show)

data Susp i = Set (Set i)
  deriving (Ord, Eq, Show)

data Type i = TyVar TyVar
            | TyFn [TyVar] (Type i)
            | Function (Susp i) [(Id.WithId i Identity Lam.Name, Type i)] (Type i)
            | Number
            | Text
            | Record [(Lam.Name, Type i)]
            | Union [(Lam.Name, Type i)]
  deriving (Ord, Eq, Show)

data LiteralType = LTNumber | LTText

type Annotations i = Map i (Type i)

type Env i = Map i (Type i)
type AppEnv i = Map i (Type i)

data HasConstraint i = HasConstraint TyVar (Constraint i)
data Constraint i = Annotated (Type i)
                  | IsFunction TyVar (Type i)
                  | IsApp [(i, Type i)] (Type i)
                  | IsLiteralType LiteralType
                  | IsRecord [(i, Type i)]
                  | Suspends TyVar


type TyVarGen i a = EitherT (TypeError i) (State (Integer, Set Text)) a

freeName :: Text -> Set Text -> Text
freeName name names = head . filter (not . (`Set.member` names)) $ name:[name `T.append` (T.pack . show $ n) | n <- [(0::Int)..]]

nextTyVar :: Text -> TyVarGen i TyVar
nextTyVar name = do (n, names) <- lift $ get
                    let n' = n + 1
                    let name' = freeName name names
                    n' `seq` lift $ put (n', Set.insert name' names)
                    pure $ TV n' name

constraints :: forall i. (Ord i) => (i -> Text) -> Lam.Resolved i -> Eval.GlobalEnv i -> Annotations i -> Map i TyVar -> Lam.ExpWithId i -> TyVarGen i (TyVar, [HasConstraint i])
constraints idAsText resolved globalEnv annotations env (Fix (Lam.ExpW (Id.WithId expId (Identity v))))
  | Just annotation <- Map.lookup expId annotations = do (tyVar,expCons) <- go v
                                                         pure (tyVar, (HasConstraint tyVar $ Annotated annotation):expCons)
  | otherwise = go v
  where --go :: Lam.ExpF (Lam.ExpWithId i) -> TyVarGen i (TyVar, [HasConstraint i])
        go :: Lam.ExpF (Id.WithId i Identity) (Fix (Lam.ExpW (Id.WithId i Identity))) -> TyVarGen i (TyVar, [HasConstraint i])
        go (Lam.AppF fun args)
          = do (appArgTys, argsCons) <- foldM (\(lastAppArgTys, lastArgCons) (Id.WithId argId (Identity argName), argExp) -> 
                                                 do (argTyVar, argCons) <- constraints idAsText resolved globalEnv annotations env argExp
                                                    resolvedId <- maybeToEither (UnresolvedId argId argName) $ Map.lookup argId resolved
                                                    pure ((resolvedId, TyVar $ argTyVar):lastAppArgTys, argCons:lastArgCons)
                                              ) 
                                              ([], [])
                                              args
               (bodyTyVar, bodyCons) <- constraints idAsText resolved globalEnv annotations env fun
               appTyVar <- nextTyVar (idAsText expId)
               pure (bodyTyVar, (HasConstraint appTyVar $ IsApp appArgTys (TyVar bodyTyVar)):(concat $ bodyCons:argsCons))
        go (Lam.LamF _ argNames body)
          = do envArgs <- mapM (\arg -> (arg ^. Id.id,) <$> nextTyVar (idAsText $ arg ^. Id.id)) argNames
               (bodyTyVar, bodyCons) <- constraints idAsText resolved globalEnv annotations (Map.union (Map.fromList envArgs) env) body
               lamTyVar <- nextTyVar (idAsText expId)
               argsTyVar <- nextTyVar (idAsText expId)
               let argsCon = HasConstraint argsTyVar $ IsRecord ((_2 %~ TyVar) <$> envArgs)
               pure (lamTyVar, (HasConstraint lamTyVar $ IsFunction argsTyVar (TyVar bodyTyVar)):argsCon:bodyCons)
        go (Lam.VarF name)
          = do resolvedId <- maybeToEither (UnresolvedId expId name) $ Map.lookup expId resolved
               ty <- maybeToEither (UndefinedVar resolvedId name) $ Map.lookup resolvedId env
               pure (ty, [])
        go (Lam.LitF lit)
          = do litTyVar <- nextTyVar (idAsText expId)
               let ty = case lit of
                          Lam.Number _ -> LTNumber
                          Lam.Text _ -> LTText
               pure (litTyVar, [HasConstraint litTyVar $ IsLiteralType ty])
        go _
          = error "I do not belong"


maybeToEither :: (Monad m) => a -> Maybe b -> EitherT a m b
maybeToEither e = maybe (left e) right

bindingTys :: Id.WithId t Identity (Lam.Binding (Id.WithId t1 Identity)) -> Map k a
bindingTys (Id.WithId _ (Identity (Lam.Binding _ b))) = case b of
  (Lam.BindingTypeish (Lam.Union _)) -> Map.empty
  (Lam.BindingExp (Fix (Lam.ExpW (Id.WithId _ (Identity (Lam.RecordF _)))))) -> Map.empty
  _ -> Map.empty

--types :: (Ord i) => Lam.Resolved i -> Eval.GlobalEnv i -> Annotations i -> Lam.BindingWithId i -> Either [TypeError i] (Typing i)
--types resolved globalEnv annotations bindings = let tys = bindingTys <$> bindings
--                                                    initialTyEnv = Map.unions tys
--                                                 in expTypes resolved globalEnv annotations initialTyEnv Map.empty Set.empty <$> bindingExps bindings
--
--
--
--expType :: (Ord i) => Lam.Resolved i -> Eval.GlobalEnv i -> Annotations i -> Env i -> AppEnv i -> Set i -> Lam.ExpWithId i -> Either [TypeError i] (Typing i)
--expType resolved globalEnv annotations tyEnv tyAppEnv visited (Fix (Lam.ExpW (Id.WithId expId (Identity v)))) = do (here, everywhere) <- go
--                                                                                                                   pure $ (here, Map.insert i here everywhere)
--  where visited' = Set.insert expId visited
--        go | Just annotation <- Map.lookup expId annotations
--               = Right (annotation, Map.empty)
--           | Set.member expId visited
--               = Left [AlreadyVisitedError expId]
--           | (Lam.AppF fun args) <- v 
--               = do funTy <- expType resolved globalEnv annotations tyEnv tyAppEnv visited' fun
--                    argTys <- eitherList $ flip map args $ \(arg, argExp) -> do
--                      ts <- expType resolved globalEnv annotations tyEnv tyAppEnv visited' argExp
--                      pure (arg ^. Id.id, expTy)
--                    appType funTy (Map.fromList argTys)
--        go (Lam.LamF args body) = do argTys <- eitherList $ flip map args $ \(Id.WithId argId (Identity argName)) -> do
--                                       argVal <- maybeToEither [ArgumentNotProvided argId argName] . Map.lookup argId $ tyAppEnv
--                                       pure (argId, argVal)
--                                       --resolvedId <- maybe (Left $ UnresolvedId id argName) Right . Map.lookup id $ resolved
--                                     typeOf resolved globalEnv annotations (Map.union (Map.fromList argTys) tyEnv) tyAppEnv visited' body
--        go _                = error "whats"
