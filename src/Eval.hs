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

flattenBinding :: (Ref m r) => BindingWithMod m r i -> Changeable m r (i, i, Lam.Name, ExpWithMod m r i)
flattenBinding expMod = do (Id.WithId bindingId (Identity (Lam.Binding n exp@(Fix (Lam.ExpW (Modish exp')))))) <- readMod expMod
                           Id.WithId boundId _ <- readMod exp'
                           pure (bindingId, boundId, n, exp)

bindingWithMod :: (Ord i, Eq i, Monad m, Ref m r) => Lam.BindingWithId i -> Changeable m r (BindingWithMod m r i)
bindingWithMod = bindingWithModReusing Map.empty

bindingWithModReusing reuse (Id.WithId i (Identity (Lam.Binding n e))) = do e' <- expWithModReusing reuse e
                                                                            newModBy Id.withIdEq . inM . pure . (Id.WithId i) . Identity . Lam.Binding n $ e'

bindingExp :: (Ref m r) => BindingWithMod m r i -> Changeable m r (ExpWithMod m r i)
bindingExp binding = do (Id.WithId _ (Identity (Lam.Binding _ exp))) <- readMod binding
                        pure exp


data EvalError i = UndefinedVar i T.Text | TypeError i T.Text
  deriving (Eq, Ord, Show)

type ThunkWithId m r i = Id.WithId i Identity (Thunk m r i)

data Suspension m r i = Suspension i (Map i (Val m r i)) [Suspension m r i]
  deriving (Eq, Ord, Show)

data Val m r i = Primitive Prim.Prim
               | Thunk (Set i) (Env m r i) (ThunkWithId m r i)
               | ValSuspension (Suspension m r i)
               | ValFrame (Frame m r i)
               | ValList [Val m r i]

type Env m r i = Map i (Val m r i)

type Trail m r i = Set (Frame m r i)
data Trailing m r i a = Trailing (Trail m r i) a

data Thunk m r i = ThunkFn (Map i (Val m r i) -> Either [EvalError i] (Val m r i))
                 | ThunkResolvedFn (Integer -> Map i (Val m r i) -> Changeable m r (Lam.Resolved i) -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i))))
                 | ThunkEvalFn (Map i (Val m r i) -> (Val m r i -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i)))) -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i))))
                 | ThunkTrailFn (Trail m r i -> Map i (Val m r i) -> Either [EvalError i] (Val m r i))
                 | ThunkExp (ExpWithMod m r i)



instance Show i => Show (Thunk m r i) where
  show (ThunkFn _) = "thunkfn"
  show (ThunkResolvedFn _) = "thunkresolvedfn"
  show (ThunkTrailFn _) = "thunktrailfn"
  show (ThunkEvalFn _) = "thunkevalfn"
  show (ThunkExp _) = "thunkexp"

deriving instance (Eq i) => Eq (Val m r i)
deriving instance (Ord i, Show i) => Ord (Val m r i)

deriving instance (Show i) => Show (Val m r i)

pprintVal :: Show i => Val m r i -> String
pprintVal = unlines . go 0
  where
    put indent s = [replicate indent ' ' ++ s]
    go indent (Primitive p) = put indent ("Primitive " ++ show p)
    go indent (Thunk needed _ t) = concat $ [ put indent $ "Thunk " ++ (show $ t ^. Id.id) ++ " " ++ (show $ t ^. Id.value) ] ++ neededArgsRows
      where neededArgsRows
              | Set.null needed  = []
              | otherwise        = [ put (indent + 2) "needs"
                                   , concat $ (put (indent + 4) . show) <$> Set.toList needed
                                   ]
    go indent (ValSuspension _) = put indent ("Suspension")
    go indent (ValFrame frame) = concat [ put indent $ "ValFrame " ++ show (_frameCodeDbId frame) ++ " " ++ show (_frameNumber frame) ++ " " ++ show (_frameAncestors frame)
                                        , go (indent + 2) (_frameResult frame)
                                        , put (indent + 2) "ValFrameEnv"
                                        , concat [ concat [ put (indent + 4) "ValFrameEnvElem"
                                                          , put (indent + 6) (show k)
                                                          , go (indent + 8) v
                                                          ]
                                                 | (k, v) <- Map.assocs (_frameEnv frame)
                                                 ]
                                        ]
    go indent (ValList elems) = concat [ put indent "ValList"
                                       , concat $ go (indent + 2) <$> elems
                                       ]


data Frame m r i = Frame { _frameAncestors :: [Integer]
                         , _frameNumber :: Integer
                         , _frameCodeDbId :: i
                         , _frameEnv :: Env m r i
                         , _frameResult :: Val m r i
                         }
  deriving (Ord, Eq, Show)
makeLenses ''Frame

printTrail :: (Show i) => Eval.Trail m r i -> IO ()
printTrail = mapM_ printTrailElems
  where printTrailElems (Frame parent number codeDbId env result) = 
          do putStrLn $ show codeDbId ++ " " ++ show number ++ " (parent=" ++ show parent ++ ")"
             putStrLn $ "  " ++ show result
             mapM_ (\(name, v) -> putStrLn ("    " ++ show name ++ " " ++ show v)) $ Map.assocs env 



instance (Ref m r, Ord i, Show i, Monoid a) => Monoid (Trailing m r i a) where
  mempty = Trailing Set.empty mempty
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

withTrail f (Trailing t v) = Trailing (f t) v

expandTrail :: (Ord i, Show i) => Frame m r i -> Trailing m r i v -> Trailing m r i v
expandTrail frame = withTrail (Set.insert frame)

withEitherTrail :: (Monad m', Ord i, Show i) => (v1 -> m' (Either e (Trailing m r i v2))) -> Either e (Trailing m r i v1) -> m' (Either e (Trailing m r i v2))
withEitherTrail f (Left e) = pure $ Left e
withEitherTrail f (Right (Trailing t1 v1)) = do f  v1 >>= \case
                                                             Left e -> pure $ Left e
                                                             Right (Trailing t2 v2) -> pure $ Right $ Trailing (t1 `mappend` t2) v2

type ParentFrameNumbers = [Integer]

eval :: (Ref m r, Ord i, Show i)
  => Modifiable m r (Lam.Resolved i)
  -> MagicNumber m r i
  -> ParentFrameNumbers
  -> Changeable m r (Map i (Val m r i))
  -> Modifiable m r (Trail m r i)
  -> ExpWithMod m r i
  -> Changeable m r (Modifiable m r (Either [EvalError i] (Trailing m r i (Val m r i))))
eval m_resolved magicNumbers parentFrameNumbers ch_env m_trail (Fix (Lam.ExpW (Modish expMod))) = newMod $ do
  (Id.WithId id (Identity v)) <- readMod expMod 
  exp' <- case v of
    Lam.LamF args exp -> do env <- ch_env
                            argIds <- forM args $ \(Modish argMod) -> do (Id.WithId argId (Identity argName)) <- readMod argMod
                                                                         pure argId
                            pure $ Right $ noTrail $ Thunk (Set.fromList argIds) env (Id.withId id (ThunkExp exp))

    Lam.AppF exp args -> withEitherTrail (\argsEnv -> do env <- ch_env
                                                         readMod =<< eval m_resolved magicNumbers parentFrameNumbers (pure $ Map.union env argsEnv) m_trail exp)
                                         =<< (evalArgs m_resolved magicNumbers parentFrameNumbers ch_env m_trail args)

    Lam.LamArgIdF var -> do lookupVarId id m_resolved >>= 
                              \case
                                Nothing -> pure $ Left [UndefinedVar id var]
                                Just varId -> pure $ Right $ noTrail $ ValSuspension (Suspension varId Map.empty [])

    Lam.VarF var -> do lookupVar id m_resolved ch_env >>= 
                         \case
                           Nothing -> pure $ Left [UndefinedVar id var]
                           Just val -> pure $ Right $ noTrail val

    Lam.SuspendF suspendSpec -> do resolved <- readMod m_resolved
                                   let evalSuspension (Lam.SuspendSpec (Modish m_name)  args parents) = do
                                         (Id.WithId id (Identity name)) <- readMod m_name
                                         case Map.lookup id resolved of
                                           Nothing -> pure $ Left [UndefinedVar id name]
                                           Just resolvedId -> do argsEnv <- evalArgs m_resolved magicNumbers parentFrameNumbers ch_env m_trail args
                                                                 parentSuspensions <- mapM evalSuspension parents >>= pure . eitherList
                                                                 pure $ Suspension resolvedId <$> (dropTrail <$> argsEnv) <*> parentSuspensions
                                                                 -- noTrail . dropTrail is pretty weird! But since we're evaluating things to specify a suspension, we're kind of not in the real world maybe? Perhaps these shouldn't be expressions in their own right, but references to expressions in the tree that are fully legit? That way there wouldn't be this weird case where expressions don't leave a trail. We don't have a good 'syntax' for referring to expressions like that though.
                                   suspension <- evalSuspension suspendSpec
                                   pure $ noTrail . ValSuspension <$> suspension

    Lam.LitF (Lam.Number n) -> pure $ Right $ noTrail $ Primitive $ Prim.Number n
    Lam.LitF (Lam.Text n) -> pure $ Right $ noTrail $ Primitive $ Prim.Text n
  withEitherTrail (evalVal m_resolved magicNumbers parentFrameNumbers ch_env m_trail) exp'

evalArgs :: (Ref m r, Ord i, Show i)
         => Modifiable m r (Lam.Resolved i)
         -> MagicNumber m r i
         -> ParentFrameNumbers
         -> Changeable m r (Map i (Val m r i))
         -> Modifiable m r (Trail m r i)
         -> [(Modish m r (Id.WithId i Identity) Lam.Name, Fix (Lam.ExpW (Modish m r (Id.WithId i Identity))))]
         -> Changeable m r (Either [EvalError i] (Trailing m r i (Map i (Val m r i))))           
evalArgs m_resolved magicNumbers parentFrameNumbers ch_env m_trail args = 
  do argVals <- forM args $ \(Modish argNameMod, arg) -> do (Id.WithId argId (Identity argName)) <- readMod argNameMod
                                                            argResult <- eval m_resolved magicNumbers parentFrameNumbers ch_env m_trail arg >>= readMod
                                                            resolved <- readMod m_resolved
                                                            case Map.lookup argId resolved of
                                                              Just resolvedArgId -> pure (fmap (resolvedArgId,) <$> argResult)
                                                              Nothing -> pure $ Left [UndefinedVar argId argName]
     let (errors, successesList) = partitionEithers argVals
     if errors == []
       then pure . Right $ Map.fromList <$> sequenceA successesList
       else pure . Left . concat $ errors
         

eitherList :: Monoid e => [Either e r] -> Either e [r]
eitherList es = case partitionEithers es of
                  ([], successesList) -> Right successesList
                  (failuresList, _) -> Left (mconcat failuresList)

evalVal :: (Ref m r, Ord i, Show i)
        => Modifiable m r (Lam.Resolved i)
        -> MagicNumber m r i
        -> ParentFrameNumbers
        -> Changeable m r (Map i (Val m r i))
        -> Modifiable m r (Trail m r i)
        -> Val m r i
        -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i)))
evalVal m_resolved magicNumbers parentFrameNumbers ch_env m_trail (Thunk thunkArgs thunkEnv thunk)
  = do env <- ch_env
       let argsInEnv = Set.intersection thunkArgs (Map.keysSet env)
       let argsRemaining = Set.difference thunkArgs argsInEnv
       
       let argsEnv = Map.filterWithKey (\k _ -> k `Set.member` argsInEnv) env
       let thunkEnv' = Map.unions [env, argsEnv, thunkEnv]

       if Set.null argsRemaining
         then evalThunk m_resolved magicNumbers parentFrameNumbers m_trail thunkEnv' thunk
         else pure $ Right $ noTrail $ Thunk argsRemaining thunkEnv' thunk
       
evalVal m_resolved _ _ ch_env _ v = pure $ Right $ noTrail v

type MagicNumber m r i = i -> Env m r i -> [Integer] -> m Integer

expWithModTopCon :: Ref m r => ExpWithMod m r i -> Changeable m r T.Text
expWithModTopCon (Fix (Lam.ExpW (Modish mod))) = lamTopCon . Id.unId <$> readMod mod 

lamTopCon :: Lam.ExpF w e -> T.Text
lamTopCon (Lam.LamF _ _) = "LamF"
lamTopCon (Lam.AppF _ _) = "AppF"
lamTopCon (Lam.VarF _) = "VarF"
lamTopCon (Lam.SuspendF _) = "SuspendF"
lamTopCon (Lam.LamArgIdF _) = "LamArgIdF"
lamTopCon (Lam.LitF _) = "LitF"


evalThunk :: (Show i, Ord i, Ref m r)
          => Modifiable m r (Lam.Resolved i)
          -> MagicNumber m r i
          -> ParentFrameNumbers
          -> Modifiable m r (Trail m r i)
          -> Env m r i
          -> Id.WithId i Identity (Thunk m r i)
          -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i)))
evalThunk m_resolved magicNumbers parentFrameNumbers0 m_trail thunkEnv' thunk
   = do magic <- inM $ magicNumbers (thunk ^. Id.id) thunkEnv' parentFrameNumbers0
        let parentFrameNumbers = magic:parentFrameNumbers0
        either_result <- case Id.unId thunk of
          ThunkFn fn         -> evalEitherVal m_resolved magicNumbers parentFrameNumbers (pure thunkEnv') m_trail (fn thunkEnv')
          ThunkTrailFn fn    -> do trail <- readMod m_trail
                                   evalEitherVal m_resolved magicNumbers parentFrameNumbers (pure thunkEnv') m_trail (fn trail thunkEnv')
          ThunkResolvedFn fn -> do fnResult <- fn magic thunkEnv' (readMod m_resolved)
                                   withEitherTrail (evalVal m_resolved magicNumbers parentFrameNumbers (pure thunkEnv') m_trail) fnResult
          ThunkEvalFn fn     -> do fnResult <- fn thunkEnv' (evalVal m_resolved magicNumbers parentFrameNumbers (pure thunkEnv') m_trail)
                                   withEitherTrail (evalVal m_resolved magicNumbers parentFrameNumbers (pure thunkEnv') m_trail) fnResult
          ThunkExp exp       -> do exp' <- expWithModTopCon exp
                                   eval m_resolved magicNumbers parentFrameNumbers (pure thunkEnv') m_trail exp
                                             >>= readMod
        pure $ (\result -> expandTrail (Frame parentFrameNumbers0 magic (thunk ^. Id.id) thunkEnv' (dropTrail result)) result) <$> either_result


evalEitherVal :: (Ref m r, Ord i, Show i)
        => Modifiable m r (Lam.Resolved i)
        -> MagicNumber m r i
        -> ParentFrameNumbers
        -> Changeable m r (Map i (Val m r i))
        -> Modifiable m r (Trail m r i)
        -> Either [EvalError i] (Val m r i)
        -> Changeable m r (Either [EvalError i] (Trailing m r i (Val m r i)))
evalEitherVal m_resolved magicNumbers parentFrameNumbers ch_env m_trail (Right v)
  = evalVal m_resolved magicNumbers parentFrameNumbers ch_env m_trail v
evalEitherVal m_resolved magicNumbers parentFrameNumbers ch_env m_trail (Left e) = pure $ Left e

                      
lookupVar :: (Ord i, Ref m r, Show i) => i -> Modifiable m r (Lam.Resolved i) -> Changeable m r (Map i (Val m r i)) -> Changeable m r (Maybe (Val m r i))
lookupVar id m_resolved ch_env = do maybeVarId <- lookupVarId id m_resolved
                                    env <- ch_env
                                    pure $ do varId <- maybeVarId
                                              Map.lookup varId env

lookupVarId id m_resolved = do resolved <- readMod m_resolved
                               pure $ Map.lookup id resolved
