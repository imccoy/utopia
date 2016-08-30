{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving #-}
module Eval where

import Prelude hiding (id, exp)
import Control.Lens
import Control.Monad
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import Data.Either (partitionEithers)
import qualified Data.Foldable
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
flattenBinding expMod = do (Id.WithId id (Identity (Lam.Binding n exp@(Fix (Lam.ExpW _))))) <- readMod expMod
                           pure (id, n, exp)

bindingWithMod :: (Ord i, Eq i, Monad m, Ref m r) => Lam.BindingWithId i -> Changeable m r (BindingWithMod m r i)
bindingWithMod = bindingWithModReusing Map.empty

bindingWithModReusing reuse (Id.WithId i (Identity (Lam.Binding n e))) = do e' <- expWithModReusing reuse e
                                                                            newModBy Id.withIdEq . inM . pure . (Id.WithId i) . Identity . Lam.Binding n $ e'

bindingExp :: (Ref m r) => BindingWithMod m r i -> Changeable m r (ExpWithMod m r i)
bindingExp binding = do (Id.WithId _ (Identity (Lam.Binding _ exp))) <- readMod binding
                        pure exp


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
  -> Changeable m r (Map i (Val m r i))
  -> [Val m r i]
  -> Changeable m r (Modifiable m r (Either [EvalError i] (Val m r i)))
eval bindings resolved (Fix (Lam.ExpW (Modish expMod))) ch_env stack = newMod $ do
  (Id.WithId id (Identity v)) <- readMod expMod 
  case v of
    Lam.LamF args exp -> let (stackElems, stack') = splitAt (length args) stack
                             env' = do env <- ch_env
                                       argIds <- mapM (\(Modish argMod) -> readMod argMod >>= (\(Id.WithId id _) -> pure id)) args
                                       let frame = Map.fromList $ zip argIds stackElems
                                       pure $ Map.union env frame
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
                       
lookupVar :: (Ord i, Ref m r) => i -> T.Text -> Modifiable m r (Lam.Resolved i) -> Changeable m r (Map i (Val m r i)) -> Changeable m r (Maybe (Val m r i))
lookupVar id var resolvedMod ch_env = do resolved <- readMod resolvedMod
                                         env <- ch_env
                                         pure $ do varId <- join $ Map.lookup id resolved
                                                   Map.lookup varId env

primitive :: (Ref m r)
          => i
          -> T.Text
          -> Maybe (
            Changeable m r ([BindingWithMod m r i]) ->
            (Modifiable m r (Lam.Resolved i)) ->
            (Changeable m r (Map i (Val m r i))) ->
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
