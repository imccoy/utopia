{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving #-}
module Eval where

import Prelude hiding (id, exp)
import Control.Lens hiding (reuses)
import Control.Monad
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import Data.Either (partitionEithers)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
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
import qualified Prim

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


data Val m r i = Primitive Prim.Prim
               | Thunk (Set i) (Env m r i) (Thunk m r i)
               | Suspension i (Map i (Val m r i))
               | ValFrame i (Map i (Val m r i)) (Val m r i)
               | ValList [Val m r i]

data Thunk m r i = ThunkFn i (Map i (Val m r i) -> Either [EvalError i] (Val m r i))
                 | ThunkResolvedFn i (Integer -> Map i (Val m r i) -> Changeable m r (Lam.Resolved i) -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i))))
                 | ThunkEvalFn i (Map i (Val m r i) -> (Val m r i -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i)))) -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i))))
                 | ThunkTrailFn i (Trail m r i -> Map i (Val m r i) -> Either [EvalError i] (Val m r i))
                 | ThunkExp i (ExpWithMod m r i)

type Env m r i = Map i (Val m r i)

instance (Eq i) => Eq (Thunk m r i) where
  ThunkFn f1 _         == ThunkFn f2 _         = f1 == f2
  ThunkResolvedFn f1 _ == ThunkResolvedFn f2 _ = f1 == f2
  ThunkTrailFn f1 _    == ThunkTrailFn f2 _    = f1 == f2
  ThunkEvalFn f1 _     == ThunkEvalFn f2 _     = f1 == f2
  ThunkExp f1 _        == ThunkExp f2 _        = f1 == f2
  _                    == _                    = False


instance (Ord i, Show i) => Ord (Thunk m r i) where
  ThunkFn f1 _         `compare` ThunkFn f2 _         = f1 `compare` f2
  ThunkFn f1 _         `compare` _                    = LT
  ThunkResolvedFn f1 _ `compare` ThunkResolvedFn f2 _ = f1 `compare` f2
  ThunkResolvedFn _ _  `compare` _                    = LT
  ThunkTrailFn f1 _    `compare` ThunkTrailFn f2 _    = f1 `compare` f2
  ThunkTrailFn _ _     `compare` _                    = LT
  ThunkEvalFn f1 _     `compare` ThunkEvalFn f2 _     = f1 `compare` f2
  ThunkEvalFn _ _      `compare` _                    = LT
  ThunkExp f1 _        `compare` ThunkExp f2 _        = f1 `compare` f2
  t1                   `compare` t2                   = error $ "Ord instance for Thunks is not total (" ++ show t1 ++ ", " ++ show t2 ++ ")"



instance Show i => Show (Thunk m r i) where
  show (ThunkFn i _) = "thunkfn " ++ show i
  show (ThunkResolvedFn i _) = "thunkresolvedfn " ++ show i
  show (ThunkTrailFn i _) = "thunktrailfn " ++ show i
  show (ThunkEvalFn i _) = "thunkevalfn " ++ show i
  show (ThunkExp i exp) = "thunkexp " ++ show i

thunkId (ThunkFn i _) = i
thunkId (ThunkResolvedFn i _) = i
thunkId (ThunkTrailFn i _) = i
thunkId (ThunkEvalFn i _) = i
thunkId (ThunkExp i _) = i



deriving instance (Eq i) => Eq (Val m r i)
deriving instance (Ord i, Show i) => Ord (Val m r i)

deriving instance (Show i) => Show (Val m r i)

pprintVal :: Show i => Val m r i -> String
pprintVal = unlines . go 0
  where
    put indent s = [replicate indent ' ' ++ s]
    go indent (Primitive p) = put indent ("Primitive " ++ show p)
    go indent (Thunk needed _ t) = concat $ [ put indent ("Thunk " ++ show t) ] ++ neededArgsRows
      where neededArgsRows
              | Set.null needed  = []
              | otherwise        = [ put (indent + 2) "needs"
                                   , concat $ (put (indent + 4) . show) <$> Set.toList needed
                                   ]
    go indent (Suspension _ _) = put indent ("Suspension")
    go indent (ValFrame i env result) = concat [ put indent ("ValFrame " ++ show i)
                                               , go (indent + 2) result
                                               , put (indent + 2) "ValFrameEnv"
                                               , concat [ concat [ put (indent + 4) "ValFrameEnvElem"
                                                                 , put (indent + 6) (show k)
                                                                 , go (indent + 8) v
                                                                 ]
                                                        | (k, v) <- Map.assocs env
                                                        ]
                                               ]
    go indent (ValList elems) = concat [ put indent "ValList"
                                       , concat $ go (indent + 2) <$> elems
                                       ]


type Trail m r i = MonoidMap i (Map (Env m r i) (Val m r i))

printTrail :: (Show i) => Eval.Trail m r i -> IO ()
printTrail = mapM_ printTrailElems . Map.assocs . unMonoidMap 
  where printTrailElems (codeDbId, elems) = do putStrLn (show codeDbId)
                                               mapM_ printTrailElem . Map.assocs $ elems
        printTrailElem (env, val) = do putStrLn ("  " ++ show val)
                                       mapM_ (\(name, v) -> putStrLn ("    " ++ show name ++ " " ++ show v)) $ Map.assocs env 


data Trailing m r i a = Trailing (Trail m r i) a

instance (Ref m r, Ord i, Show i, Monoid a) => Monoid (Trailing m r i a) where
  mempty = Trailing MonoidMap.empty mempty
  mappend (Trailing t1 v1) (Trailing t2 v2) = Trailing (mappend t1 t2) (mappend v1 v2)

instance (Ref m r, Eq a, Eq i) => Eq (Trailing m r i a) where
  Trailing t1 v1 == Trailing t2 v2 = (t1 == t2) && (v1 == v2)

instance (Show a) => Show (Trailing m r i a) where
  show (Trailing _ v1) = show v1


instance Functor (Trailing m r i) where
  fmap f (Trailing t v) = Trailing t (f v)

instance (Ord i, Show i) => Applicative (Trailing m r i) where
  pure v = noTrail v
  Trailing t1 f <*> Trailing t2 v = Trailing (mappend t1 t2) (f v)

noTrail v = Trailing mempty v

dropTrail (Trailing _ v) = v

withEitherTrail :: (Monad m', Ord i, Show i) => (v1 -> m' (Either e (Trailing m r i v2))) -> Either e (Trailing m r i v1) -> m' (Either e (Trailing m r i v2))
withEitherTrail f (Left e) = pure $ Left e
withEitherTrail f (Right (Trailing t1 v1)) = do f  v1 >>= \case
                                                             Left e -> pure $ Left e
                                                             Right (Trailing t2 v2) -> pure $ Right $ Trailing (t1 `mappend` t2) v2

eval :: (Ref m r, Ord i, Show i)
  => Modifiable m r (Lam.Resolved i)
  -> MagicNumber m r i
  -> Changeable m r (Map i (Val m r i))
  -> Modifiable m r (Trail m r i)
  -> ExpWithMod m r i
  -> Changeable m r (Modifiable m r (Either [EvalError i] (Trailing m r i (Val m r i))))
eval m_resolved ior_magicNumbers ch_env m_trail (Fix (Lam.ExpW (Modish expMod))) = newMod $ do
  (Id.WithId id (Identity v)) <- readMod expMod 
  exp' <- case v of
    Lam.LamF args exp -> do env <- ch_env
                            argIds <- forM args $ \(Modish argMod) -> do (Id.WithId argId (Identity argName)) <- readMod argMod
                                                                         pure argId
                            pure $ Right $ noTrail $ Thunk (Set.fromList argIds) env (ThunkExp id exp)

    Lam.AppF exp args -> withEitherTrail (\argsEnv -> do env <- ch_env
                                                         readMod =<< eval m_resolved ior_magicNumbers (pure $ Map.union env argsEnv) m_trail exp)
                                         =<< (evalArgs m_resolved ior_magicNumbers ch_env m_trail args)

    Lam.LamArgIdF var -> do lookupVarId id m_resolved >>= 
                              \case
                                Nothing -> pure $ Left [UndefinedVar id var]
                                Just varId -> pure $ Right $ noTrail $ Suspension varId Map.empty

    Lam.VarF var -> do lookupVar id m_resolved ch_env >>= 
                         \case
                           Nothing -> pure $ Left [UndefinedVar id var]
                           Just val -> pure $ Right $ noTrail val

    Lam.SuspendF var args -> do resolved <- readMod m_resolved
                                case Map.lookup id resolved of
                                  Nothing -> pure $ Left [UndefinedVar id var]
                                  Just resolvedId -> do argsEnv <- evalArgs m_resolved ior_magicNumbers ch_env m_trail args
                                                        -- noTrail . dropTrail is pretty weird! But since we're evaluating things to specify a suspension, we're kind of not in the real world maybe? Perhaps these shouldn't be expressions in their own right, but references to expressions in the tree that are fully legit? That way there wouldn't be this weird case where expressions don't leave a trail. We don't have a good 'syntax' for referring to expressions like that though.
                                                        pure $ (noTrail . Suspension resolvedId . dropTrail <$> argsEnv)

    Lam.LitF (Lam.Number n) -> pure $ Right $ noTrail $ Primitive $ Prim.Number n
    Lam.LitF (Lam.Text n) -> pure $ Right $ noTrail $ Primitive $ Prim.Text n
  withEitherTrail (evalVal m_resolved ior_magicNumbers ch_env m_trail) exp'

evalArgs :: (Ref m r, Ord i, Show i)
         => Modifiable m r (Lam.Resolved i)
         -> MagicNumber m r i
         -> Changeable m r (Map i (Val m r i))
         -> Modifiable m r (Trail m r i)
         -> [(Modish m r (Id.WithId i Identity) Lam.Name, Fix (Lam.ExpW (Modish m r (Id.WithId i Identity))))]
         -> Changeable m r (Either [EvalError i] (Trailing m r i (Map i (Val m r i))))           
evalArgs m_resolved ior_magicNumbers ch_env m_trail args = 
  do argVals <- forM args $ \(Modish argNameMod, arg) -> do (Id.WithId argId (Identity argName)) <- readMod argNameMod
                                                            argResult <- eval m_resolved ior_magicNumbers ch_env m_trail arg >>= readMod
                                                            resolved <- readMod m_resolved
                                                            case Map.lookup argId resolved of
                                                              Just resolvedArgId -> pure (fmap (resolvedArgId,) <$> argResult)
                                                              Nothing -> pure $ Left [UndefinedVar argId argName]
     let (errors, successesList) = partitionEithers argVals
     if errors == []
       then pure . Right $ Map.fromList <$> sequenceA successesList
       else pure . Left . concat $ errors
         

evalVal :: (Ref m r, Ord i, Show i)
        => Modifiable m r (Lam.Resolved i)
        -> MagicNumber m r i
        -> Changeable m r (Map i (Val m r i))
        -> Modifiable m r (Trail m r i)
        -> Val m r i
        -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i)))
evalVal m_resolved ior_magicNumbers ch_env m_trail (Thunk thunkArgs thunkEnv thunk)
  = do env <- ch_env
       let argsInEnv = Set.intersection thunkArgs (Map.keysSet env)
       let argsRemaining = Set.difference thunkArgs argsInEnv
       
       let argsEnv = Map.filterWithKey (\k _ -> k `Set.member` argsInEnv) env
       let thunkEnv' = Map.unions [env, argsEnv, thunkEnv]

       if Set.null argsRemaining
         then evalThunk m_resolved ior_magicNumbers m_trail thunkEnv' thunk
         else pure $ Right $ noTrail $ Thunk argsRemaining thunkEnv' thunk
       
evalVal m_resolved _ ch_env _ v = pure $ Right $ noTrail v

type MagicNumber m r i = i -> Env m r i -> m Integer

evalThunk :: (Show i, Ord i, Ref m r)
          => Modifiable m r (Lam.Resolved i)
          -> MagicNumber m r i
          -> Modifiable m r (Trail m r i)
          -> Env m r i
          -> Thunk m r i
          -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i)))
evalThunk m_resolved ior_magicNumbers m_trail thunkEnv' thunk = go thunk >>= pure . fmap (\(Trailing expTrail val) -> Trailing (mappend expTrail $ MonoidMap.singleton (thunkId thunk) (Map.singleton thunkEnv' val)) val)
  where go (ThunkFn _ fn) = evalEitherVal m_resolved ior_magicNumbers (pure thunkEnv') m_trail (fn thunkEnv')
        go (ThunkTrailFn _ fn) = do trail <- readMod m_trail
                                    evalEitherVal m_resolved ior_magicNumbers (pure thunkEnv') m_trail (fn trail thunkEnv')
        go (ThunkResolvedFn i fn) = do magic <- inM $ ior_magicNumbers i thunkEnv'
                                       fnResult <- fn magic thunkEnv' (readMod m_resolved)
                                       withEitherTrail (evalVal m_resolved ior_magicNumbers (pure thunkEnv') m_trail) fnResult
        go (ThunkEvalFn _ fn) = do fnResult <- fn thunkEnv' (evalVal m_resolved ior_magicNumbers (pure thunkEnv') m_trail)
                                   withEitherTrail (evalVal m_resolved ior_magicNumbers (pure thunkEnv') m_trail) fnResult
        go (ThunkExp _ exp) = eval m_resolved ior_magicNumbers (pure thunkEnv') m_trail exp
                                    >>= readMod


evalEitherVal :: (Ref m r, Ord i, Show i)
        => Modifiable m r (Lam.Resolved i)
        -> MagicNumber m r i
        -> Changeable m r (Map i (Val m r i))
        -> Modifiable m r (Trail m r i)
        -> Either [EvalError i] (Val m r i)
        -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i)))
evalEitherVal m_resolved ior_magicNumbers ch_env m_trail (Right v)
  = evalVal m_resolved ior_magicNumbers ch_env m_trail v
evalEitherVal m_resolved ior_magicNumbers ch_env m_trail (Left e) = pure $ Left e

                      
lookupVar :: (Ord i, Ref m r, Show i) => i -> Modifiable m r (Lam.Resolved i) -> Changeable m r (Map i (Val m r i)) -> Changeable m r (Maybe (Val m r i))
lookupVar id m_resolved ch_env = do maybeVarId <- lookupVarId id m_resolved
                                    env <- ch_env
                                    pure $ do varId <- maybeVarId
                                              Map.lookup varId env

lookupVarId id m_resolved = do resolved <- readMod m_resolved
                               pure $ Map.lookup id resolved
