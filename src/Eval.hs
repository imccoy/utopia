{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving #-}
module Eval where

import Prelude hiding (id, exp)
import qualified Prelude
import Control.Lens hiding (reuses)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Data.Functor.Foldable.Extended
import qualified Id
import qualified Lam
import Lam (BindingWithId, ExpWithId)
import qualified Primitives

flattenBinding :: BindingWithId i -> Maybe (i, i, Lam.Name, ExpWithId i)
flattenBinding binding = flip fmap (Lam.bindingExp binding) $ \exp -> 
    (binding ^. Id.id, Lam.expTopId exp, Lam.bindingName binding, exp)

data EvalError i = UndefinedVar i T.Text | UndefinedMember i (Frame i) T.Text | TypeError i (Val i) T.Text
  deriving (Eq, Ord, Show)

type ThunkWithId i = Id.WithId i Identity (Thunk i)

data Suspension i = Suspension i (Map i (Val i)) [Suspension i]
  deriving (Eq, Ord, Show)

data Val i = Primitive Primitives.Prim
           | Thunk [[i]] (Set i) (Env i) (ThunkWithId i)
           | ValSuspension (Suspension i)
           | ValFrame (Frame i)
           | ValList [Val i]
  deriving (Show, Ord, Eq)

type Env i = Map i (Val i)

newtype GlobalEnv i  = GlobalEnv { unGlobalEnv :: Env i }

type Trail i = Set (Frame i, Val i)
data Trailing i a = Trailing (Trail i) a

data Thunk i = ThunkFn (Env i -> GlobalEnv i -> Either [EvalError i] (Val i))
             | ThunkResolvedFn (Frame i -> Env i -> GlobalEnv i -> Lam.Resolved i -> Either [EvalError i] (Trailing i (Val i)))
             | ThunkEvalFn (Env i -> GlobalEnv i -> (Val i -> Either [EvalError i] (Trailing i (Val i))) -> Either [EvalError i] (Trailing i (Val i)))
             | ThunkTrailFn (Trail i -> Env i -> GlobalEnv i -> Either [EvalError i] (Val i))
             | ThunkExp (ExpWithId i)
             | ThunkRecord

instance Show i => Show (Thunk i) where
  show (ThunkFn _) = "thunkfn"
  show (ThunkResolvedFn _) = "thunkresolvedfn"
  show (ThunkTrailFn _) = "thunktrailfn"
  show (ThunkEvalFn _) = "thunkevalfn"
  show (ThunkExp _) = "thunkexp"
  show (ThunkRecord) = "thunkrecord"


pprintVal :: Show i => Val i -> String
pprintVal = unlines . go 0
  where
    put indent s = [replicate indent ' ' ++ s]
    go indent (Primitive p) = put indent ("Primitive " ++ show p)
    go indent (Thunk ss needed env t) = concat $ [ put indent $ "Thunk " ++ (show $ t ^. Id.id) ++ " " ++ show ss ++ " " ++ (show $ t ^. Id.value)
                                                 , concat neededArgsRows 
                                                 , put (indent + 2) "has"
                                                 , concat [ put (indent + 4) (show k) ++
                                                            concat [ go (indent + 6) v ] 
                                                          | (k, v) <- Map.assocs env
                                                          ]
                                                 ]
      where neededArgsRows
              | Set.null needed  = [ put (indent + 2) "satisfied" ]
              | otherwise        = [ put (indent + 2) "needs"
                                   , concat $ (put (indent + 4) . show) <$> Set.toList needed
                                   ]
    go indent (ValSuspension _) = put indent ("Suspension")
    go indent (ValFrame frame) = concat [ put indent $ "ValFrame " ++ show (_frameCodeDbId frame) ++ " " ++ show (_frameParent frame)
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


data Frame i = Frame { _frameParent :: Maybe (Frame i)
                     , _frameCodeDbId :: i
                     , _frameEnv :: Env i
                     }
  deriving (Ord, Eq, Show)
makeLenses ''Frame

instance (Ord i, Show i, Monoid a) => Monoid (Trailing i a) where
  mempty = Trailing Set.empty mempty
  mappend (Trailing t1 v1) (Trailing t2 v2) = Trailing (mappend t1 t2) (mappend v1 v2)

instance (Eq a, Eq i) => Eq (Trailing i a) where
  Trailing t1 v1 == Trailing t2 v2 = (t1 == t2) && (v1 == v2)

instance (Show a) => Show (Trailing i a) where
  show (Trailing _ v1) = show v1


instance Functor (Trailing i) where
  fmap f (Trailing t v) = Trailing t (f v)

instance (Ord i, Show i) => Applicative (Trailing i) where
  pure v = noTrail v
  Trailing t1 f <*> Trailing t2 v = Trailing (mappend t1 t2) (f v)

noTrail :: Ord i => a -> Trailing i a
noTrail v = Trailing mempty v

dropTrail :: Trailing t t1 -> t1
dropTrail (Trailing _ v) = v

withTrail :: (Trail t -> Trail i) -> Trailing t a -> Trailing i a
withTrail f (Trailing t v) = Trailing (f t) v

expandTrail :: (Ord i, Show i) => [[i]] -> (Frame i, Val i) -> Trailing i v -> Trailing i v
expandTrail [] _               = Prelude.id
expandTrail _  frameWithResult = withTrail (Set.insert frameWithResult)

withEitherTrail :: (Ord i, Show i) => (v1 -> Either e (Trailing i v2)) -> Either e (Trailing i v1) -> Either e (Trailing i v2)
withEitherTrail f e = do Trailing t1 v1 <- e
                         Trailing t2 v2 <- f v1
                         pure $ Trailing (t1 `mappend` t2) v2

invertTrailingEither :: Trailing i (Either e v) -> Either e (Trailing i v)
invertTrailingEither (Trailing t v) = Trailing t <$> v

eval :: (Ord i, Show i)
  => Lam.Resolved i
  -> GlobalEnv i
  -> Frame i
  -> Map i (Val i)
  -> Trail i
  -> ExpWithId i
  -> Either [EvalError i] (Trailing i (Val i))
eval resolved globalEnv parentFrames env trail (Fix (Lam.ExpW (Id.WithId id (Identity v)))) =
  withEitherTrail (evalVal resolved globalEnv parentFrames env trail) $ case v of
    Lam.LamF susable args exp -> let argIds = map (^. Id.id) args
                                  in noTrail <$> (Thunk <$> flattenSusable resolved id susable <*> pure (Set.fromList argIds) <*> pure env <*> pure (Id.withId id (ThunkExp exp)))

    Lam.AppF exp args -> flip withEitherTrail (evalArgs resolved globalEnv parentFrames env trail args) $ \argValues ->
                              eval resolved globalEnv parentFrames (Map.union argValues env) trail exp

    Lam.LamArgIdF var -> case lookupVarId id resolved of
                           Nothing -> Left [UndefinedVar id var]
                           Just varId -> Right $ noTrail $ ValSuspension (Suspension varId Map.empty [])

    Lam.VarF var -> case lookupVar id resolved env globalEnv of
                      Nothing -> Left [UndefinedVar id var]
                      Just val -> Right $ noTrail val

    Lam.SuspendF suspendSpec -> let evalSuspension (Lam.SuspendSpec (Id.WithId suspensionId (Identity name)) args parents) = do
                                      case Map.lookup suspensionId resolved of
                                        Nothing -> Left [UndefinedVar id name]
                                        Just resolvedId -> do argsEnv <- evalArgs resolved globalEnv parentFrames env trail args
                                                              parentSuspensions <- eitherList $ (evalSuspension <$> parents)
                                                              pure $ Suspension resolvedId (dropTrail argsEnv) parentSuspensions
                                                              -- noTrail . dropTrail is pretty weird! But since we're evaluating things to specify a suspension, we're kind of not in the real world maybe? Perhaps these shouldn't be expressions in their own right, but references to expressions in the tree that are fully legit? That way there wouldn't be this weird case where expressions don't leave a trail. We don't have a good 'syntax' for referring to expressions like that though.
                                 in noTrail . ValSuspension <$> evalSuspension suspendSpec

    Lam.LitF (Lam.Number n) -> pure $ noTrail $ Primitive $ Primitives.Number n
    Lam.LitF (Lam.Text n) -> pure $ noTrail $ Primitive $ Primitives.Text n

evalArgs :: (Ord i, Show i)
         => Lam.Resolved i
         -> GlobalEnv i
         -> Frame i
         -> Map i (Val i)
         -> Trail i
         -> [(Id.WithId i Identity Lam.Name, ExpWithId i)]
         -> Either [EvalError i] (Trailing i (Map i (Val i)))
evalArgs resolved globalEnv parentFrame env trail args = 
  let evalArg (Id.WithId argId (Identity argName), exp) = case Map.lookup argId resolved of
                                                            Just resolvedArgId -> fmap (resolvedArgId,) <$> eval resolved globalEnv parentFrame env trail exp
                                                            Nothing -> Left [UndefinedVar argId argName]
   in fmap (fmap Map.fromList . sequenceA) . eitherList $ evalArg <$> args

eitherList :: Monoid e => [Either e r] -> Either e [r]
eitherList es = case partitionEithers es of
                  ([], successesList) -> Right successesList
                  (failuresList, _) -> Left (mconcat failuresList)

evalVal :: (Ord i, Show i)
        => Lam.Resolved i
        -> GlobalEnv i
        -> Frame i
        -> Map i (Val i)
        -> Trail i
        -> Val i
        -> Either [EvalError i] (Trailing i (Val i))
evalVal resolved globalEnv parentFrames env trail (Thunk thunkSusable thunkArgs thunkEnv thunk)
  = let argsInEnv = Set.intersection thunkArgs (Map.keysSet env)
       
        argsEnv = Map.filterWithKey (\k _ -> k `Set.member` argsInEnv) env
        thunkEnv' = Map.unions [env, argsEnv, thunkEnv]

     in if Set.size argsInEnv == Set.size thunkArgs
          then fmap (fmap (reSus thunkSusable)) . evalThunk resolved globalEnv parentFrames trail thunkSusable thunkEnv' argsEnv $ thunk
          else Right $ noTrail $ Thunk thunkSusable thunkArgs thunkEnv' thunk
       
evalVal _ _ _ _ _ v = Right $ noTrail v

evalThunk :: (Show i, Ord i)
          => Lam.Resolved i
          -> GlobalEnv i
          -> Frame i
          -> Trail i
          -> [[i]]
          -> Env i
          -> Env i
          -> Id.WithId i Identity (Thunk i)
          -> Either [EvalError i] (Trailing i (Val i))
evalThunk resolved globalEnv parentFrame trail susable thunkEnv argsEnv thunk
   = let newFrame = Frame (Just parentFrame) (thunk ^. Id.id) argsEnv
      in do result <- withEitherTrail (evalVal resolved globalEnv newFrame thunkEnv trail . reSus susable) $ case Id.unId thunk of
              ThunkFn fn         -> fmap noTrail $ fn thunkEnv globalEnv
              ThunkTrailFn fn    -> fmap noTrail $ fn trail thunkEnv globalEnv
              ThunkResolvedFn fn -> fn newFrame thunkEnv globalEnv resolved
              ThunkEvalFn fn     -> fn thunkEnv globalEnv (evalVal resolved globalEnv newFrame thunkEnv trail)
              ThunkExp exp       -> eval resolved globalEnv newFrame thunkEnv trail exp
              ThunkRecord        -> pure . pure . ValFrame $ newFrame
            let finalSusable = case result of
                                 Trailing _ (Thunk s _ _ _) -> s ++ susable
                                 _ -> susable
            pure $ expandTrail finalSusable (newFrame, dropTrail result) result

reSus :: [[i]] -> Val i -> Val i
reSus susable (Thunk susable' a e t) = Thunk (susable' ++ susable) a e t
reSus _       v = v

lookupVar :: (Ord i, Show i) => i -> Lam.Resolved i -> Map i (Val i) -> GlobalEnv i -> Maybe (Val i)
lookupVar id resolved env globalEnv = maybe Nothing (lookupVarByResolvedId env globalEnv) $ lookupVarId id resolved

lookupVarByResolvedId :: Ord i => Map i (Val i) -> GlobalEnv i -> i -> Maybe (Val i)
lookupVarByResolvedId env globalEnv k = case Map.lookup k env of
                                          Just x -> Just x
                                          Nothing -> Map.lookup k . unGlobalEnv $ globalEnv


lookupVarId :: Ord k => k -> Map k a -> Maybe a
lookupVarId id resolved = Map.lookup id resolved

flattenSusable :: (Show i, Ord i) => Lam.Resolved i -> i -> Lam.Susable (Id.WithId i Identity) -> Either [EvalError i] [[i]]
flattenSusable resolved errId (Lam.Susable ss) = eitherList ((\s -> eitherList (go <$> s)) <$> ss)
  where go (Id.WithId id (Identity name)) = maybe (Left [UndefinedVar errId name]) Right (Map.lookup id resolved)
