{-# LANGUAGE DeriveTraversable, ScopedTypeVariables #-}
module Lam where

import Prelude hiding (exp, id)

import Data.Foldable (fold)
import Data.Functor.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import DiffTree (DiffTree(..))
import qualified Id

type Name = T.Text

data Literal = Number Int | Text T.Text
  deriving (Show)

data ExpF e = LamF [Name] e
            | AppF e [e]
            | VarF Name
            | LitF Literal
  deriving (Functor, Prelude.Foldable, Show, Traversable)

data Binding e = Binding Name e
  deriving (Functor, Show)

type Exp = Fix ExpF

lam :: [T.Text] -> Exp -> Exp
lam args body = Fix (LamF args body)

app :: Exp -> [Exp] -> Exp
app f args = Fix (AppF f args)

var :: T.Text -> Exp
var name = Fix (VarF name)

lit :: Literal -> Exp
lit literal = Fix (LitF literal)

type ExpWithId i = Id.WithIdR i ExpF
type BindingWithId i = Id.WithId i (Binding (ExpWithId i))

bindingWithId :: (Monad m) => m i -> Binding Exp -> m (BindingWithId i)
bindingWithId gen (Binding name exp) = do
  (bindingId :: i) <- gen
  (expWithIds :: ExpWithId i) <- Id.withIdM gen exp
  pure $ Id.WithId bindingId (Binding name expWithIds)

mapBindingId :: (i1 -> i2) -> BindingWithId i1 -> BindingWithId i2
mapBindingId f (Id.WithId i (Binding n v)) = Id.WithId (f i) $ Binding n (Id.mapIds f v)

newtype MonoidMap k v = MonoidMap (Map k v)
unMonoidMap (MonoidMap m) = m

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap Map.empty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap (Map.unionWith mappend a b)

type Env i = Map i (Maybe i)

resolveVars :: (Ord i) => [BindingWithId i] -> Env i
resolveVars bindings = (Just <$> boundInExprs) `mappend` mconcat [ let maybeId = Map.lookup name topLevelBindings
                                                                    in Map.fromList $ (, maybeId) <$> Set.toList varIds
                                                                 | (name, varIds) <- Map.assocs $ unMonoidMap unboundInExprs]
  where (topLevelBindingList, topLevelBindingExpList) = unzip [((name, i), exp) | (Id.WithId i (Binding name exp)) <- bindings]
        topLevelBindings = Map.fromList topLevelBindingList
        (unboundInExprs, boundInExprs) = mconcat (resolveExpVars <$> topLevelBindingExpList)

resolveExpVars :: (Ord i) => ExpWithId i -> (MonoidMap Name (Set i), Map i i)
resolveExpVars = 
  cata $ \(Id.WithIdF (Id.WithId i v)) ->
    case v of
      VarF name -> (MonoidMap $ Map.singleton name (Set.singleton i), Map.empty)
      LamF names exp -> foldr bindName exp names
        where bindName name (unbound, bound) = case Map.lookup name $ unMonoidMap unbound of
                                                 Just varIds -> (MonoidMap (Map.delete name (unMonoidMap unbound)), foldr (\varId -> Map.insert varId i) bound varIds)
                                                 Nothing -> (unbound, bound)
      _ -> Data.Foldable.fold v

bindingDiffTree :: Env T.Text -> BindingWithId T.Text -> DiffTree
bindingDiffTree env (Id.WithId id (Binding name exp)) = DiffTree id "Binding" (Just name) [expDiffTree env exp]

bindingDiffTrees :: [BindingWithId T.Text] -> [DiffTree]
bindingDiffTrees bindings = fmap (bindingDiffTree env) bindings
  where env = resolveVars bindings

expDiffTree :: Env T.Text -> ExpWithId T.Text -> DiffTree
expDiffTree env = cata $ \(Id.WithIdF (Id.WithId i v)) -> case v of
  LamF args body  -> DiffTree i "Lam" Nothing $
                                      [DiffTree (argId i n)
                                                 "LamArg"
                                                 (Just arg) 
                                                 []
                                      | (n, arg) <- zip [0..] args
                                      ] ++ [body]
  VarF name       -> DiffTree i "Var" (Just name) 
                                      [DiffTree (i `T.append` ".Ref")
                                                "METADATA-REF"
                                                (Data.Foldable.fold $ Map.lookup i env)
                                                []
                                      ]
  AppF f args     -> DiffTree i "App" Nothing $
                                      f:[wrap (argId i n)
                                              "AppArg"
                                              [arg]
                                        | (n, arg) <- zip [0..] args
                                        ]
  LitF (Number n) -> DiffTree i "LitNumber" (Just (T.pack $ show n)) []
  LitF (Text t)   -> DiffTree i "LitText" (Just t) []

argsId :: T.Text -> T.Text
argsId id = id `T.append` ".Args"

argId :: T.Text -> Int -> T.Text
argId id n = argsId id `T.append` ".Args[" `T.append` T.pack (show n) `T.append` "]"

wrap :: T.Text -> T.Text -> [DiffTree] -> DiffTree
wrap i name exps = DiffTree i name Nothing exps
