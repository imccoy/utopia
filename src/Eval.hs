{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving #-}
module Eval where

import Prelude hiding (id, exp)
import Control.Monad
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T

import Data.Functor.Foldable.Extended
import qualified Id
import qualified Lam

data Val m r i = Text T.Text | Number Integer | ValExp (ExpWithMod m r i)

instance Show (Val m r i) where
  show (Text t) = "Text " ++ T.unpack t
  show (Number n) = "Number " ++ show n
  show (ValExp _) = "Exp"

data InternalError i = DanglingVar i T.Text
  deriving (Eq, Show)
data EvalError i = UndefinedVar i T.Text | TypeError i T.Text | InternalError (InternalError i)
  deriving (Eq, Show)

data ExpWithModF m r i v = ExpWithModF (Changeable m r (Id.WithId i (Lam.ExpF v)))
deriving instance (Ref m r) => Functor (ExpWithModF m r i)

type ExpWithMod m r i = Fix (ExpWithModF m r i)
type BindingWithMod m r i = Changeable m r (Id.WithId i (Lam.Binding (ExpWithMod m r i)))

expWithMod :: (Monad m, Ref m r) => Lam.ExpWithId i -> ExpWithMod m r i
expWithMod = cata $ \(Lam.ExpWithIdF v) -> Fix $ ExpWithModF $ inM (pure v)

bindingWithMod :: (Monad m, Ref m r) => Lam.BindingWithId i -> BindingWithMod m r i
bindingWithMod (Id.WithId i (Lam.Binding n e)) = inM . pure . (Id.WithId i) . Lam.Binding n $ expWithMod e

bindingExp :: (Ref m r) => BindingWithMod m r i -> Changeable m r (i, Lam.Name, ExpWithMod m r i)
bindingExp ch = do (Id.WithId id (Lam.Binding n (Fix (ExpWithModF exp)))) <- ch
                   pure (id, n, Fix . ExpWithModF $ exp)


--expWithModById' :: (Eq i, Ref m r) => i -> ExpWithMod m r i -> Changeable m r (First (ExpWithMod m r i))
--expWithModById' id =
--  paraM $ \val@(ExpWithModF ch) -> do
--    Id.WithId id' expPair  <- ch
--    if id' == id
--      then inM $ pure $ First $ Just $ Fix $ ExpWithModF $ inM $ pure $ Id.WithId id' $ fst <$> expPair
--      else inM $ pure $ Data.Foldable.fold (snd <$> expPair)


expWithModByIdInExp :: (Eq i, Ref m r) => i -> ExpWithMod m r i -> Changeable m r (First (ExpWithMod m r i))
expWithModByIdInExp id exp = go exp
  where go (Fix val@(ExpWithModF ch)) = do
          Id.WithId id' v' <- ch
          if id' == id
            then inM $ pure $ First $ Just $ Fix val
            else
              case v' of
                Lam.LamF _ e -> go e
                Lam.AppF e es -> mconcat <$> mapM go (e:es)
                _ -> inM $ pure $ First Nothing

expWithModByIdInBindings :: (Eq i, Ref m r) => i -> [BindingWithMod m r i] -> Changeable m r (First (ExpWithMod m r i))
expWithModByIdInBindings id bindings = mconcat <$> mapM (expWithModByIdInBinding id) bindings

expWithModByIdInBinding :: (Eq i, Ref m r) => i -> BindingWithMod m r i -> Changeable m r (First (ExpWithMod m r i))
expWithModByIdInBinding id bindingWithMod = do
  (Id.WithId id' (Lam.Binding _ exp)) <- bindingWithMod
  if id == id'
    then inM $ pure $ First $ Just exp
    else expWithModByIdInExp id exp

eval :: (Ref m r, Ord i)
  =>  Changeable m r [BindingWithMod m r i]
  -> Changeable m r (Lam.Resolved i)
  -> ExpWithMod m r i
  -> Changeable m r (Map i (Map T.Text (Changeable m r (Val m r i))))
  -> [Changeable m r (Val m r i)]
  -> Changeable m r (Either [EvalError i] (Val m r i))
eval bindings resolved (Fix (ExpWithModF ch)) env stack = do
  (Id.WithId id v) <- ch
  case v of
    Lam.LamF args exp -> let (stackElems, stack') = splitAt (length args) stack
                             frame = Map.fromList $ zip args stackElems
                             env' = Map.insert id frame <$> env
                          in eval bindings resolved exp env' stack'
    Lam.AppF exp args -> do vals <- forM args $ \arg -> eval bindings resolved arg env stack
                            let (errors, successes) = partitionEithers vals
                            if errors == []
                              then eval bindings resolved exp env ((pure <$> successes) ++ stack)
                              else pure $ Left $ concat errors
    Lam.VarF var -> do maybeVal <- lookupVar id var <$> resolved <*> env
                       case maybeVal of
                         Nothing -> case primitive id var of
                                      Just prim -> prim bindings resolved env stack
                                      _ -> pure $ Left [UndefinedVar id var]
                         Just valCh -> do
                           val <- valCh
                           case val of
                             ValExp exp -> eval bindings resolved exp env stack
                             x -> pure $ Right x
    Lam.LitF (Lam.Number n) -> pure $ Right $ Number n
    Lam.LitF (Lam.Text n) -> pure $ Right $ Text n
                       
lookupVar :: (Ord i, Ref m r) => i -> T.Text -> Lam.Resolved i -> Map i (Map T.Text (Changeable m r (Val m r i))) -> Maybe (Changeable m r (Val m r i))
lookupVar id var resolved env = do frameId <- join $ Map.lookup id resolved
                                   frame <- Map.lookup frameId env
                                   Map.lookup var frame

primitive :: (Ref m r)
          => i
          -> T.Text
          -> Maybe (
            Changeable m r [BindingWithMod m r i] ->
            Changeable m r (Lam.Resolved i) ->
            Changeable m r (Map i (Map T.Text (Changeable m r (Val m r i)))) ->
            [Changeable m r (Val m r i)] ->
            Changeable m r (Either [EvalError i] (Val m r i))
          )
primitive id "+" = Just $ \_ _ _ stack -> valNumbers stack >>= \case 
                                         Right numbers -> pure $ Right $ Number $ sum numbers
                                         Left (i, v) -> pure $ Left [TypeError id $ "(+) at arg " `T.append` (T.pack $ show i) `T.append` ", was " `T.append` (T.pack $ show v)]
primitive _ _ = Nothing

valNumbers :: (Ref m r) => [Changeable m r (Val m r i)] -> Changeable m r (Either (Integer, Val m r i) [Integer])
valNumbers valChs = do 
  vals <- sequence valChs
  let valNumbers = valNumber <$> zip [0..] vals
  inM $ pure $ sequenceA valNumbers

valNumber (_, Number n) = Right n
valNumber (i, v)        = Left (i, v)
