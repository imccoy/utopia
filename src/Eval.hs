{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving #-}
module Eval where

import Prelude hiding (id, exp)
import Control.Monad
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import Data.Functor.Foldable.Extended
import qualified Id
import qualified Lam

data Val m r i = Text T.Text | Number Integer | ValExp (ExpWithMod m r i)
  deriving (Eq)

instance Show (Val m r i) where
  show (Text t) = "Text " ++ T.unpack t
  show (Number n) = "Number " ++ show n
  show (ValExp _) = "Exp"

data InternalError i = DanglingVar i T.Text
  deriving (Eq, Show)
data EvalError i = UndefinedVar i T.Text | TypeError i T.Text | InternalError (InternalError i)
  deriving (Eq, Show)

data ExpWithModF m r i v = ExpWithModF (Modifiable m r (Id.WithId i (Lam.ExpF v)))
  deriving (Eq)


type ExpWithMod m r i = Fix (ExpWithModF m r i)
type BindingWithMod m r i = Modifiable m r (Id.WithId i (Lam.Binding (ExpWithMod m r i)))

expWithMod :: (Eq i, Monad m, Ref m r) => Lam.ExpWithId i -> Changeable m r (ExpWithMod m r i)
expWithMod = cataM $ \(Lam.ExpWithIdF v) -> pure . Fix . ExpWithModF =<< (newModBy Id.withIdEq . inM . pure $ v)

bindingWithMod :: (Eq i, Monad m, Ref m r) => Lam.BindingWithId i -> Changeable m r (BindingWithMod m r i)
bindingWithMod (Id.WithId i (Lam.Binding n e)) = do e' <- expWithMod e
                                                    newModBy Id.withIdEq . inM . pure . (Id.WithId i) . Lam.Binding n $ e'

bindingExp :: (Ref m r) => BindingWithMod m r i -> Changeable m r (i, Lam.Name, ExpWithMod m r i)
bindingExp expMod = do (Id.WithId id (Lam.Binding n (Fix (ExpWithModF exp)))) <- readMod expMod
                       pure (id, n, Fix . ExpWithModF $ exp)


--expWithModById' :: (Eq i, Ref m r) => i -> ExpWithMod m r i -> Changeable m r (First (ExpWithMod m r i))
--expWithModById' id =
--  paraM $ \val@(ExpWithModF ch) -> do
--    Id.WithId id' expPair  <- ch
--    if id' == id
--      then inM $ pure $ First $ Just $ Fix $ ExpWithModF $ inM $ pure $ Id.WithId id' $ fst <$> expPair
--      else inM $ pure $ Data.Foldable.fold (snd <$> expPair)


--expWithModByIdInExp :: (Eq i, Ref m r) => i -> ExpWithMod m r i -> Changeable m r (First (ExpWithMod m r i))
--expWithModByIdInExp id exp = go exp
--  where go (Fix val@(ExpWithModF ch)) = do
--          Id.WithId id' v' <- ch
--          if id' == id
--            then inM $ pure $ First $ Just $ Fix val
--            else
--              case v' of
--                Lam.LamF _ e -> go e
--                Lam.AppF e es -> mconcat <$> mapM go (e:es)
--                _ -> inM $ pure $ First Nothing
--
--expWithModByIdInBindings :: (Eq i, Ref m r) => i -> [BindingWithMod m r i] -> Changeable m r (First (ExpWithMod m r i))
--expWithModByIdInBindings id bindings = mconcat <$> mapM (expWithModByIdInBinding id) bindings
--
--expWithModByIdInBinding :: (Eq i, Ref m r) => i -> BindingWithMod m r i -> Changeable m r (First (ExpWithMod m r i))
--expWithModByIdInBinding id bindingWithMod = do
--  (Id.WithId id' (Lam.Binding _ exp)) <- bindingWithMod
--  if id == id'
--    then inM $ pure $ First $ Just exp
--    else expWithModByIdInExp id exp

eval :: (Ref m r, Ord i)
  => Changeable m r [BindingWithMod m r i]
  -> Modifiable m r (Lam.Resolved i)
  -> ExpWithMod m r i
  -> Changeable m r (Map i (Map T.Text (Val m r i)))
  -> [Val m r i]
  -> Changeable m r (Modifiable m r (Either [EvalError i] (Val m r i)))
eval bindings resolved (Fix (ExpWithModF expMod)) ch_env stack = newMod $ do
  (Id.WithId id v) <- readMod expMod 
  case v of
    Lam.LamF args exp -> let (stackElems, stack') = splitAt (length args) stack
                             env' = do env <- ch_env
                                       let frame = Map.fromList $ zip args stackElems
                                       pure $ Map.insert id frame env
                          in eval bindings resolved exp env' stack' >>= readMod
    Lam.AppF exp args -> do vals <- forM args $ \arg -> eval bindings resolved arg ch_env stack >>= readMod
                            let (errors, successes) = partitionEithers vals
                            if errors == []
                              then eval bindings resolved exp ch_env (successes ++ stack) >>= readMod
                              else pure $ Left $ concat errors
    Lam.VarF var -> do lookupVar id var resolved ch_env >>= 
                         \case
                           Nothing -> case primitive id var of
                                        Just prim -> prim bindings resolved ch_env stack
                                        _ -> pure $ Left [UndefinedVar id var]
                           Just val -> do
                             case val of
                               ValExp exp -> eval bindings resolved exp ch_env stack >>= readMod
                               x -> pure $ Right x
    Lam.LitF (Lam.Number n) -> pure $ Right $ Number n
    Lam.LitF (Lam.Text n) -> pure $ Right $ Text n
                       
lookupVar :: (Ord i, Ref m r) => i -> T.Text -> Modifiable m r (Lam.Resolved i) -> Changeable m r (Map i (Map T.Text (Val m r i))) -> Changeable m r (Maybe (Val m r i))
lookupVar id var resolvedMod ch_env = do resolved <- readMod resolvedMod
                                         env <- ch_env
                                         pure $ do frameId <- join $ Map.lookup id resolved
                                                   frame <- Map.lookup frameId env
                                                   Map.lookup var frame

primitive :: (Ref m r)
          => i
          -> T.Text
          -> Maybe (
            Changeable m r ([BindingWithMod m r i]) ->
            (Modifiable m r (Lam.Resolved i)) ->
            (Changeable m r (Map i (Map T.Text (Val m r i)))) ->
            [Val m r i] ->
            Changeable m r (Either [EvalError i] (Val m r i))
          )
primitive id "+" = Just $ \_ _ _ stack -> case valNumbers stack of
                                            Right numbers -> pure $ Right $ Number $ sum numbers
                                            Left (i, v) -> pure $ Left [TypeError id $ "(+) at arg " `T.append` (T.pack $ show i) `T.append` ", was " `T.append` (T.pack $ show v)]
primitive _ _ = Nothing

valNumbers :: (Ref m r) => [Val m r i] -> Either (Integer, Val m r i) [Integer]
valNumbers vals =
  let valNumbers = valNumber <$> zip [0..] vals
   in sequence valNumbers

valNumber (_, Number n) = Right n
valNumber (i, v)        = Left (i, v)
