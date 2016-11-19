{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving #-}
module Eval (Env, EvalError(..), Val(..), BindingWithMod, eval, evalVal, evalAndVal, bindingWithMod, reuses, bindingWithModReusing, flattenBinding, bindingExp, Thunk(..), dropTrail) where

import Prelude hiding (id, exp)
import Control.Lens hiding (reuses)
import Control.Monad
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import Data.Either (partitionEithers)
import qualified Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Data.Functor.Foldable.Extended
import qualified Id
import qualified Lam
import MonoidMap (MonoidMap(..))
import qualified MonoidMap

import Debug.Trace

newtype Modish m r w v = Modish (Modifiable m r (w v))
  deriving (Eq)

type ExpWithMod m r i = Fix (Lam.ExpW (Modish m r (Id.WithId i Identity)))

type BindingWithMod m r i = Modifiable m r (Id.WithId i Identity (Lam.Binding (ExpWithMod m r i)))

expWithMod :: (Ord i, Eq i, Monad m, Ref m r) => Lam.ExpWithId i -> Changeable m r (ExpWithMod m r i)
expWithMod = expWithModReusing Map.empty

expWithModReusing' :: (Ord i, Eq i, Monad m, Ref m r) => Map i (ExpWithMod m r i) -> Lam.ExpW (Id.WithId i Identity) (ExpWithMod m r i) -> Changeable m r (ExpWithMod m r i)
expWithModReusing' reuse (Lam.ExpW (Id.WithId i (Identity e))) = case Map.lookup i reuse of
                                                                     Nothing -> do e' <- Lam.expChangeWM (\p -> Modish <$> (newModBy Id.withIdEq . pure) p) e
                                                                                   v' <- newModBy Id.withIdEq $ inM $ pure $ Id.WithId i $ Identity e'
                                                                                   pure $ Fix $ Lam.ExpW $ Modish v'
                                                                     Just mod -> pure $ mod

expWithModReusing :: (Ord i, Eq i, Monad m, Ref m r) => Map i (ExpWithMod m r i) -> Lam.ExpWithId i -> Changeable m r (ExpWithMod m r i)
expWithModReusing reuse = cataM (expWithModReusing' reuse)

reuses :: (Ord srcNodeId, Ord dstNodeId, Ref m r) => Map srcNodeId dstNodeId -> ExpWithMod m r srcNodeId -> Changeable m r (Map dstNodeId (ExpWithMod m r srcNodeId))
reuses zeroCostMappings = go
  where go e@(Fix (Lam.ExpW (Modish mod))) = do (Id.WithId srcNodeId (Identity exp)) <- readMod mod
                                                children <- Data.Foldable.fold <$> (sequence $ go <$> exp)
                                                pure $ case Map.lookup srcNodeId zeroCostMappings of
                                                         Just dstNodeId -> Map.insert dstNodeId e children
                                                         Nothing -> children

flattenBinding :: (Ref m r) => BindingWithMod m r i -> Changeable m r (i, Lam.Name, ExpWithMod m r i)
flattenBinding expMod = do (Id.WithId _ (Identity (Lam.Binding n exp@(Fix (Lam.ExpW (Modish exp')))))) <- readMod expMod
                           Id.WithId id _ <- readMod exp'
                           pure (id, n, exp)

bindingWithMod :: (Ord i, Eq i, Monad m, Ref m r) => Lam.BindingWithId i -> Changeable m r (BindingWithMod m r i)
bindingWithMod = bindingWithModReusing Map.empty

bindingWithModReusing reuse (Id.WithId i (Identity (Lam.Binding n e))) = do e' <- expWithModReusing reuse e
                                                                            newModBy Id.withIdEq . inM . pure . (Id.WithId i) . Identity . Lam.Binding n $ e'

bindingExp :: (Ref m r) => BindingWithMod m r i -> Changeable m r (ExpWithMod m r i)
bindingExp binding = do (Id.WithId _ (Identity (Lam.Binding _ exp))) <- readMod binding
                        pure exp


data EvalError i = UndefinedVar i T.Text | TypeError i T.Text
  deriving (Eq, Ord, Show)


data Val m r i = Text T.Text
               | Number Integer
               | Thunk (Set i) (Env m r i) (Thunk m r i)
               | Suspension i (Map i (Val m r i))

data Thunk m r i = ThunkFn i (Map i (Val m r i) -> Either [EvalError i] (Val m r i))
                 | ThunkExp (ExpWithMod m r i)

type Env m r i = Map i (Val m r i)

instance (Eq i, EqRef r) => Eq (Thunk m r i) where
  ThunkFn f1 _ == ThunkFn f2 _ = f1 == f2
  ThunkExp e1 == ThunkExp e2   = e1 == e2

instance Show i => Show (Thunk m r i) where
  show (ThunkFn i _) = "thunkfn " ++ show i
  show (ThunkExp exp) = "thunkexp"

deriving instance (Eq i, EqRef r) => Eq (Val m r i)

instance (Show i) => Show (Val m r i) where
  show (Text text) = "Text " ++ T.unpack text
  show (Number num) = "Number " ++ show num
  show (Thunk _ _ thunk) = "Thunk " ++ show thunk

type TrailElement m r i = (Env m r i, Val m r i)
type Trail m r i = MonoidMap i [TrailElement m r i]

data Trailing m r i a = Trailing (Trail m r i) a

instance (Ref m r, Ord i, Monoid a) => Monoid (Trailing m r i a) where
  mempty = Trailing MonoidMap.empty mempty
  mappend (Trailing t1 v1) (Trailing t2 v2) = Trailing (mappend t1 t2) (mappend v1 v2)

instance (Ref m r, Eq a, Eq i) => Eq (Trailing m r i a) where
  Trailing t1 v1 == Trailing t2 v2 = (t1 == t2) && (v1 == v2)

instance (Show a) => Show (Trailing m r i a) where
  show (Trailing _ v1) = show v1


instance Functor (Trailing m r i) where
  fmap f (Trailing t v) = Trailing t (f v)

instance (Ord i) => Applicative (Trailing m r i) where
  pure v = noTrail v
  Trailing t1 f <*> Trailing t2 v = Trailing (mappend t1 t2) (f v)

noTrail v = Trailing mempty v

dropTrail (Trailing _ v) = v

withEitherTrail :: (Monad m', Ord i) => (Either e v1 -> m' (Either e (Trailing m r i v2))) -> Either e (Trailing m r i v1) -> m' (Either e (Trailing m r i v2))
withEitherTrail f (Left e) = pure $ Left e
withEitherTrail f (Right (Trailing t1 v1)) = do f (Right v1) >>= \case
                                                                   Left e -> pure $ Left e
                                                                   Right (Trailing t2 v2) -> pure $ Right $ Trailing (t1 `mappend` t2) v2

eval :: (Ref m r, Ord i, Show i)
  => Modifiable m r (Lam.Resolved i)
  -> Changeable m r (Map i (Val m r i))
  -> ExpWithMod m r i
  -> Changeable m r (Modifiable m r (Either [EvalError i] (Trailing m r i (Val m r i))))
eval m_resolved ch_env (Fix (Lam.ExpW (Modish expMod))) = newMod $ do
  (Id.WithId id (Identity v)) <- readMod expMod 
  exp' <- case v of
    Lam.LamF args exp -> do env <- ch_env
                            argIds <- forM args $ \(Modish argMod) -> do (Id.WithId argId (Identity argName)) <- readMod argMod
                                                                         pure argId
                            pure $ Right $ noTrail $ Thunk (Set.fromList argIds) env (ThunkExp exp)

    Lam.AppF exp args -> do argVals <- forM args $ \(Modish argNameMod, arg) -> do (Id.WithId argId (Identity argName)) <- readMod argNameMod
                                                                                   argResult <- eval m_resolved ch_env arg >>= readMod
                                                                                   resolved <- readMod m_resolved
                                                                                   case Map.lookup argId resolved of
                                                                                     Just resolvedArgId -> pure (fmap (resolvedArgId,) <$> argResult)
                                                                                     Nothing -> pure $ Left [UndefinedVar argId argName]
                            let (errors, successesList) = partitionEithers argVals
                            if errors == []
                              then do let successesMap = Map.fromList <$> sequenceA successesList
                                      env <- ch_env
                                      let Trailing argsTrail env' = ((`Map.union` env)) <$> successesMap
                                      fmap (\(Trailing expTrail val) -> Trailing (argsTrail `mappend` expTrail) val) <$> (readMod =<< eval m_resolved (pure env') exp)
                              else pure . Left . concat $ errors

    Lam.VarF var -> do lookupVar id var m_resolved ch_env >>= 
                         \case
                           Nothing -> pure $ Left [UndefinedVar id var]
                           Just val -> pure $ Right $ noTrail val
--    Lam.SuspendF var args -> do resolved <- readMod m_resolved
--                                case Map.lookup id resolved of
--                                  Nothing -> pure $ Left [UndefinedVar id var]
--                                  Just resolvedId -> do argsEnv <- evalArgs m_resolved ch_env args
--                                                        pure $ (Suspension resolvedId <$> argsEnv)
--
    Lam.LitF (Lam.Number n) -> pure $ Right $ noTrail $ Number n
    Lam.LitF (Lam.Text n) -> pure $ Right $ noTrail $ Text n
  --evalVal m_resolved ch_env exp' >>= \case
  --                                      Right (Trailing trail2 val) -> pure $ Right $ Trailing (trail1 `mappend` trail2) val
  --                                      Left l -> pure l
  --evalVal m_resolved ch_env <$> exp'
  case exp' of
    Right (Trailing trail1 exp'') -> evalVal m_resolved ch_env (Right exp'') >>= \case
                                                                                   Right (Trailing trail2 val) -> pure $ Right $ Trailing (trail1 `mappend` trail2) val
                                                                                   Left l -> pure $ Left l
    Left l -> pure $ Left l


evalVal :: (Ref m r, Ord i, Show i)
        => Modifiable m r (Lam.Resolved i)
        -> Changeable m r (Map i (Val m r i))
        -> Either [EvalError i] (Val m r i)
        -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i)))
evalVal m_resolved ch_env (Right (Thunk thunkArgs thunkEnv thunk)) 
  = do env <- ch_env
       let argsInEnv = Set.intersection thunkArgs (Map.keysSet env)
       let argsRemaining = Set.difference thunkArgs argsInEnv
       
       let argsEnv = Map.filterWithKey (\k _ -> k `Set.member` argsInEnv) env
       let thunkEnv' = Map.unions [env, argsEnv, thunkEnv]

       if Set.null argsRemaining
         then case thunk of
                ThunkFn _ fn -> evalVal m_resolved (pure thunkEnv') (fn thunkEnv')
                --ThunkExp exp -> eval m_resolved (pure thunkEnv') exp 
                --                  >>= readMod 
                --                  >>= \case
                --                        Left e -> pure $ Left e
                --                        Right (Trailing trail1 val) -> evalVal m_resolved (pure thunkEnv') (Right val)
                --                                                         >>= \case
                --                                                           Left e -> pure $ Left e
                --                                                           Right (Trailing trail2 result) -> pure $ Right $ Trailing (trail1 `mappend` trail2) result
                ThunkExp exp -> evalAndVal m_resolved (pure thunkEnv') exp
          else pure $ Right $ noTrail $ Thunk argsRemaining thunkEnv' thunk
       
evalVal m_resolved env v = pure $ noTrail <$> v

evalAndVal m_resolved ch_env exp = eval m_resolved ch_env exp 
                                     >>= readMod 
                                     >>= withEitherTrail (evalVal m_resolved ch_env)

                      
lookupVar :: (Ord i, Ref m r, Show i) => i -> T.Text -> Modifiable m r (Lam.Resolved i) -> Changeable m r (Map i (Val m r i)) -> Changeable m r (Maybe (Val m r i))
lookupVar id var m_resolved ch_env = do resolved <- readMod m_resolved
                                        env <- ch_env
                                        pure $ do varId <- Map.lookup id resolved
                                                  Map.lookup varId env
