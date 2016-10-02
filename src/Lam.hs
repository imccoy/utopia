{-# LANGUAGE DeriveTraversable, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
module Lam where

import Prelude hiding (exp, id)

import Control.Lens
import Control.Lens.TH
import Data.Either.Validation
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
              | AppF e [(w Name, e)]
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

app :: Exp -> [(T.Text, Exp)] -> Exp
app f args = Fix $ ExpW $ pure $ AppF f (traverse . _1 %~ (\name -> pure name) $ args)

var :: T.Text -> Exp
var name = Fix $ ExpW $ pure $ VarF name

lit :: Literal -> Exp
lit literal = Fix $ ExpW $ pure $ LitF literal

--expChangeW :: (w a -> w' a) -> ExpF w e -> ExpF w' e
expChangeW :: (w Name -> w' Name) -> ExpF w e -> ExpF w' e
expChangeW f (LamF names e) = LamF (map f names) e
expChangeW f (AppF fun args) = AppF fun (map (_1 %~ f) args)
expChangeW _ (VarF n) = VarF n
expChangeW _ (LitF l) = LitF l


expChangeWM :: (Monad m) => (w Name -> m (w' Name)) -> ExpF w e -> m (ExpF w' e)
expChangeWM f (LamF names e) = LamF <$> mapM f names <*> pure e
expChangeWM f (AppF fun args) = AppF fun <$> mapM (_1 %%~ f) args
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
  deriving (Eq, Ord, Show)

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap Map.empty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap (Map.unionWith mappend a b)

type Resolved i = Map i i

type ArgNameDuplicates i = MonoidMap Name (Set i)
type UndefinedNames i = MonoidMap Name (Set i)
data NameResolutionError i = DuplicatedArgNames (ArgNameDuplicates i) | UndefinedNames (UndefinedNames i)
  deriving (Eq, Ord, Show)



data GlobalNames i = GlobalNames { globalNamesTopLevelBindings :: Map Name i, globalNamesArgs :: Map Name i }

globalNames :: (Ord i) => GlobalNames i -> [BindingWithId i] -> Either (ArgNameDuplicates i) (GlobalNames i)
globalNames (GlobalNames builtinsIds builtinsArgIds) bindings = GlobalNames (builtinsIds `Map.union` topLevelBindingNamesFromBindings bindings) <$> (argNamesFromBindings builtinsArgIds bindings)

bindingExps :: [BindingWithId i] -> [ExpWithId i]
bindingExps = map (\(Id.WithId _ (Identity (Binding _ e))) -> e)

smooshEithers :: Monoid l => (r -> r -> Either l r) -> (r -> l -> l) -> Either l r -> Either l r -> Either l r
smooshEithers _     _      (Left err1) (Left err2) = Left $ err1 `mappend` err2
smooshEithers _     fOkErr (Right ok)  (Left err)  = Left $ fOkErr ok err
smooshEithers _     fOkErr (Left err)  (Right ok)  = Left $ fOkErr ok err
smooshEithers fOkOk _      (Right ok1) (Right ok2) = fOkOk ok1 ok2

topLevelBindingNamesFromBindings :: (Ord i) => [BindingWithId i] -> Map Name i
topLevelBindingNamesFromBindings bindings = Map.fromList [(name, id) | (Id.WithId _ (Identity (Binding name (Fix (ExpW (Id.WithId id _)))))) <- bindings]

argNamesFromBindings :: (Ord i) => Map Name i -> [BindingWithId i] -> Either (ArgNameDuplicates i) (Map Name i)
argNamesFromBindings builtinsArgsIds = argNamesFromExps builtinsArgsIds . bindingExps

argNamesFromExps :: (Ord i) => Map Name i -> [ExpWithId i] -> Either (ArgNameDuplicates i) (Map Name i)
argNamesFromExps builtinsArgsIds exps = combineAll $ (Right builtinsArgsIds):(map argNamesFromExp exps)
  where
    argNamesFromExp :: (Ord i) => ExpWithId i -> Either (ArgNameDuplicates i) (Map Name i)
    argNamesFromExp = cata $ \(ExpW (Id.WithId i (Identity v))) ->
       case v of
         LamF args e -> combineAll $ e:[Right $ Map.singleton name id | (Id.WithId id (Identity name)) <- args]
         _ -> combineAll v

    combineAll :: (Ord i, Data.Foldable.Foldable f) => f (Either (ArgNameDuplicates i) (Map Name i)) -> Either (ArgNameDuplicates i) (Map Name i)
    combineAll = foldr combine (Right $ Map.empty)

    combine :: (Ord i) => Either (ArgNameDuplicates i) (Map Name i) -> Either (ArgNameDuplicates i) (Map Name i) -> Either (ArgNameDuplicates i) (Map Name i)
    combine = smooshEithers combineNoErrors combineOneError

    combineOneError :: (Ord i) => Map Name i -> ArgNameDuplicates i -> ArgNameDuplicates i
    combineOneError ok err = MonoidMap $ Map.mapWithKey f $ unMonoidMap err
      where f k errAtK = case Map.lookup k ok of
                           Just v -> Set.insert v errAtK
                           Nothing -> errAtK

    combineNoErrors :: (Ord i) => Map Name i -> Map Name i -> Either (ArgNameDuplicates i) (Map Name i)
    combineNoErrors l r = let intersectionKeys = Set.intersection (Map.keysSet l) (Map.keysSet r)
                              singletonIfIntersecting k v = if Set.member k intersectionKeys
                                                              then Just $ Set.singleton v
                                                              else Nothing
                           in if Set.null intersectionKeys
                                then Right $ Map.union l r
                                else Left $ MonoidMap $ Map.unionWith (Set.union) 
                                                                      (Map.mapMaybeWithKey singletonIfIntersecting l)
                                                                      (Map.mapMaybeWithKey singletonIfIntersecting r)


resolveVars :: (Ord i) => GlobalNames i -> [BindingWithId i] -> Either (NameResolutionError i) (Resolved i)
resolveVars builtins bindings = do
  globalNameBindings <- _Left %~ DuplicatedArgNames $ globalNames builtins bindings
  resolved <- _Left %~ UndefinedNames $ resolveBindingsExpVars globalNameBindings bindings
  pure resolved

resolveBindingsExpVars :: (Ord i) => GlobalNames i -> [BindingWithId i] -> Either (UndefinedNames i) (Resolved i)
resolveBindingsExpVars globalNames = concatExpVars . map (resolveExpVars globalNames) . bindingExps

smooshExpVars :: (Ord i) => Either (UndefinedNames i) (Resolved i) -> Either (UndefinedNames i) (Resolved i) -> Either (UndefinedNames i) (Resolved i)
smooshExpVars = smooshEithers (\a b -> Right $ a `mappend` b) (\_ err -> err)

concatExpVars :: (Ord i, Data.Foldable.Foldable t) => t (Either (UndefinedNames i) (Resolved i)) -> Either (UndefinedNames i) (Resolved i)
concatExpVars = Data.Foldable.foldr smooshExpVars (Right Map.empty)

resolveExpVars :: (Ord i) => GlobalNames i -> ExpWithId i -> Either (UndefinedNames i) (Resolved i)
resolveExpVars globalNames expr = go Map.empty expr
  where
    go env (Fix (ExpW (Id.WithId i (Identity e)))) = go' env i e
    go' env i (LamF names e) = go (addNamesToEnv names env) e
    go' env i (AppF e args) = concatExpVars $ (go env e):[lookupArg env arg | arg <- args]
    go' env i (VarF name) = case Map.lookup name env of
                              Just targetId -> Right $ Map.singleton i targetId
                              Nothing -> case Map.lookup name (globalNamesTopLevelBindings globalNames) of
                                           Just targetId -> Right $ Map.singleton i targetId
                                           Nothing -> Left $ MonoidMap $ Map.singleton name (Set.singleton i)
    go' env i (LitF _) = Right $ Map.empty

    addNamesToEnv :: [Id.WithId i Identity T.Text] -> Map Name i -> Map Name i
    addNamesToEnv names = Map.union (Map.fromList [(name, id) | (Id.WithId id (Identity name)) <- names])

    lookupArg env ((Id.WithId id (Identity (argName))), exp) = concatExpVars [ case Map.lookup argName (globalNamesArgs globalNames) of
                                                                                 Just targetId -> Right $ Map.singleton id targetId
                                                                                 Nothing -> Left $ MonoidMap $ Map.singleton argName (Set.singleton id)
                                                                             , go env exp
                                                                             ]
                                                                
                                        

bindingDiffTree :: Resolved T.Text -> BindingWithId T.Text -> DiffTree
bindingDiffTree env (Id.WithId id (Identity (Binding name exp))) = DiffTree id "Binding" (Just name) [expDiffTree env exp]

bindingDiffTrees :: GlobalNames T.Text -> [BindingWithId T.Text] -> Either (NameResolutionError T.Text) [DiffTree]
bindingDiffTrees builtins bindings = do
  resolved <- resolveVars builtins bindings
  pure $ map (bindingDiffTree resolved) bindings

expDiffTree :: Resolved T.Text -> ExpWithId T.Text -> DiffTree
expDiffTree env = cata $ \(ExpW (Id.WithId i (Identity v))) -> case v of
  LamF args body  -> DiffTree i "Lam" Nothing $
                                      [DiffTree argId
                                                "LamArg"
                                                (Just arg) 
                                                []
                                      | (Id.WithId argId (Identity arg)) <- args
                                      ] ++ [body]
  VarF name       -> DiffTree i "Var" (Just name) 
                                      --[DiffTree (i `T.append` ".Ref")
                                      --          "METADATA-REF"
                                      --          (Data.Foldable.fold $ Map.lookup i env)
                                      --          []
                                      --]
                                      []
  AppF f args     -> DiffTree i "App" Nothing $
                                      f:[DiffTree argId
                                                  "AppArg"
                                                  (Just arg)
                                                  [e]
                                        | (Id.WithId argId (Identity arg), e) <- args
                                        ]
  LitF (Number n) -> DiffTree i "LitNumber" (Just (T.pack $ show n)) []
  LitF (Text t)   -> DiffTree i "LitText" (Just t) []

wrap :: T.Text -> T.Text -> [DiffTree] -> DiffTree
wrap i name exps = DiffTree i name Nothing exps
