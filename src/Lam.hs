{-# LANGUAGE DeriveTraversable, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
module Lam where

import Prelude hiding (exp, id)

import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Foldable.Extended
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import DiffTree (DiffTree(..))
import qualified Id

type Name = T.Text

data Literal = Number Integer | Text T.Text
  deriving (Show)

data ExpF w e = LamF [w Name] e
              | AppF e [e]
              | VarF Name
              | LitF Literal
  deriving (Functor, Prelude.Foldable, Traversable)

data Binding e = Binding Name e
  deriving (Functor, Show)

newtype ExpW w e = ExpW (w (ExpF w e))
  deriving (Functor, Prelude.Foldable, Traversable)
type Exp = Fix (ExpW Identity)

deriving instance (Eq (w e), Eq (w (ExpF w e))) => Eq (ExpW w e)

lam :: [T.Text] -> Exp -> Exp
lam args body = Fix $ ExpW $ pure $ LamF (pure <$> args) body

app :: Exp -> [Exp] -> Exp
app f args = Fix $ ExpW $ pure $ AppF f args

var :: T.Text -> Exp
var name = Fix $ ExpW $ pure $ VarF name

lit :: Literal -> Exp
lit literal = Fix $ ExpW $ pure $ LitF literal

--expChangeW :: (w a -> w' a) -> ExpF w e -> ExpF w' e
expChangeW :: (w Name -> w' Name) -> ExpF w e -> ExpF w' e
expChangeW f (LamF names e) = LamF (map f names) e
expChangeW _ (AppF fun args) = AppF fun args
expChangeW _ (VarF n) = VarF n
expChangeW _ (LitF l) = LitF l


expChangeWM :: (Monad m) => (w Name -> m (w' Name)) -> ExpF w e -> m (ExpF w' e)
expChangeWM f (LamF names e) = LamF <$> mapM f names <*> pure e
expChangeWM _ (AppF fun args) = pure $ AppF fun args
expChangeWM _ (VarF n) = pure $ VarF n
expChangeWM _ (LitF l) = pure $ LitF l

type ExpWithId i = Fix (ExpW (Id.WithId i Identity))
type BindingWithId i = Id.WithId i Identity (Binding (ExpWithId i))

bindingWithId :: (Monad m) => m i -> Binding Exp -> m (BindingWithId i)
bindingWithId gen (Binding name exp) = do
  bindingId <- gen
  expsWithIds <- expsWithIdM gen exp
  pure $ Id.WithId bindingId $ pure (Binding name expsWithIds)


addId :: (Monad m) => m i -> Identity v -> m (Id.WithId i Identity v)
addId gen val = addIdM gen $ pure val

addIdM :: (Monad m) => m i -> m (Identity v) -> m (Id.WithId i Identity v)
addIdM gen val = Id.WithId <$> gen <*> val

expsWithIdM' :: (Monad m) => m i -> ExpW Identity (ExpWithId i) -> m (ExpWithId i)
expsWithIdM' gen (ExpW (Identity e)) = do
  e' <- expChangeWM (\n -> addIdM gen (pure n)) e
  id <- gen
  pure $ Fix $ ExpW $ Id.WithId id (Identity e')

expsWithIdM :: (Monad m) => m i -> Exp -> m (ExpWithId i)
expsWithIdM gen exp = cataM (expsWithIdM' gen) exp

mapBindingId :: (i1 -> i2) -> BindingWithId i1 -> BindingWithId i2
mapBindingId f (Id.WithId i (Identity (Binding n v))) = Id.WithId (f i) $ Identity $ Binding n $ cata go v
  where go (ExpW withId) = Fix $ ExpW $ Id.mapId f $ (expChangeW (Id.mapId f) <$> withId)

newtype MonoidMap k v = MonoidMap { unMonoidMap :: Map k v }

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap Map.empty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap (Map.unionWith mappend a b)

type Resolved i = Map i (Maybe i)

resolveVars :: (Ord i) => [BindingWithId i] -> Resolved i
resolveVars bindings = (Just <$> boundInExprs) `mappend` mconcat [ let maybeId = Map.lookup name topLevelBindings
                                                                    in Map.fromList $ (, maybeId) <$> Set.toList varIds
                                                                 | (name, varIds) <- Map.assocs $ unMonoidMap unboundInExprs]
  where (topLevelBindingList, topLevelBindingExpList) = unzip [((name, i), exp) | (Id.WithId i (Identity (Binding name exp))) <- bindings]
        topLevelBindings = Map.fromList topLevelBindingList
        (unboundInExprs, boundInExprs) = mconcat (resolveExpVars <$> topLevelBindingExpList)

resolveExpVars :: (Ord i) => ExpWithId i -> (MonoidMap Name (Set i), Map i i)
resolveExpVars = 
  cata $ \(ExpW (Id.WithId i (Identity v))) ->
    case v of
      VarF name -> (MonoidMap $ Map.singleton name (Set.singleton i), Map.empty)
      LamF names exp -> foldr bindName exp names
        where bindName (Id.WithId _ (Identity name)) (unbound, bound) = case Map.lookup name $ unMonoidMap unbound of
                                                                          Just varIds -> (MonoidMap (Map.delete name (unMonoidMap unbound)), foldr (\varId -> Map.insert varId i) bound varIds)
                                                                          Nothing -> (unbound, bound)
      _ -> Data.Foldable.fold v

bindingDiffTree :: Resolved T.Text -> BindingWithId T.Text -> DiffTree
bindingDiffTree env (Id.WithId id (Identity (Binding name exp))) = DiffTree id "Binding" (Just name) [expDiffTree env exp]

bindingDiffTrees :: [BindingWithId T.Text] -> [DiffTree]
bindingDiffTrees bindings = fmap (bindingDiffTree env) bindings
  where env = resolveVars bindings

expDiffTree :: Resolved T.Text -> ExpWithId T.Text -> DiffTree
expDiffTree env = cata $ \(ExpW (Id.WithId i (Identity v))) -> case v of
  LamF args body  -> DiffTree i "Lam" Nothing $
                                      [DiffTree argId
                                                "LamArg"
                                                (Just arg) 
                                                []
                                      | (n, (Id.WithId argId (Identity arg))) <- zip [0..] args
                                      ] ++ [body]
  VarF name       -> DiffTree i "Var" (Just name) 
                                      --[DiffTree (i `T.append` ".Ref")
                                      --          "METADATA-REF"
                                      --          (Data.Foldable.fold $ Map.lookup i env)
                                      --          []
                                      --]
                                      []
  AppF f args     -> DiffTree i "App" Nothing $
                                      f:[wrap (i `T.append` ".Args[" `T.append` T.pack (show n) `T.append` "]")
                                              "AppArg"
                                              [arg]
                                        | (n, arg) <- zip [0..] args
                                        ]
  LitF (Number n) -> DiffTree i "LitNumber" (Just (T.pack $ show n)) []
  LitF (Text t)   -> DiffTree i "LitText" (Just t) []

wrap :: T.Text -> T.Text -> [DiffTree] -> DiffTree
wrap i name exps = DiffTree i name Nothing exps
