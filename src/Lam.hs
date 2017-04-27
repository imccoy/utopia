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
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import DiffTree (DiffTree(..))
import qualified Id
import MonoidMap (MonoidMap(..))
import qualified MonoidMap

type Name = T.Text

data Literal = Number Integer | Text T.Text
  deriving (Show)

data SuspendSpec w e = SuspendSpec (w Name) [(w Name, e)] [SuspendSpec w e]
  deriving (Functor, Prelude.Foldable, Traversable)

deriving instance (Show e, Show (w e), Show (w Name)) => Show (SuspendSpec w e)

data ExpF w e = LamF [w Name] e
              | AppF e [(w Name, e)]
              | RecordF [w Name]
              | VarF Name
              | SuspendF (SuspendSpec w e)
              | LamArgIdF Name
              | LitF Literal
  deriving (Functor, Prelude.Foldable, Traversable)

deriving instance (Show e, Show (w e), Show (w Name)) => Show (ExpF w e)

data Binding w = Binding Name (BindingContents w)

deriving instance (Show (BindingContents w)) => Show (Binding w)

data BindingContents w = BindingExp (Fix (ExpW w))
                       | BindingTypeish (Typeish w)

deriving instance (Show (w Name), Show (Fix (ExpW w))) => Show (BindingContents w)

data Typeish w = Union [w Name]

deriving instance (Show (w Name)) => Show (Typeish w)

newtype ExpW w e = ExpW (w (ExpF w e))
  deriving (Functor, Prelude.Foldable, Traversable)
type Exp = Fix (ExpW Identity)


deriving instance (Eq (w e), Eq (w (ExpF w e))) => Eq (ExpW w e)

deriving instance (Show (w (ExpF w e))) => Show (ExpW w e)

expBinding :: T.Text -> Exp -> Binding Identity
expBinding name exp = Binding name $ BindingExp exp

lam :: [T.Text] -> Exp -> Exp
lam args body = Fix $ ExpW $ pure $ LamF (pure <$> args) body

app :: Exp -> [(T.Text, Exp)] -> Exp
app f args = Fix $ ExpW $ pure $ AppF f (traverse . _1 %~ (\name -> pure name) $ args)

var :: T.Text -> Exp
var = Fix . ExpW . pure . VarF

lit :: Literal -> Exp
lit = Fix . ExpW . pure . LitF

suspendSpec :: Name -> [(T.Text, Exp)] -> [SuspendSpec Identity Exp] -> SuspendSpec Identity Exp
suspendSpec name args parents = SuspendSpec (pure name) (traverse . _1 %~ (\name -> pure name) $ args) parents

suspend :: SuspendSpec Identity Exp -> Exp
suspend = Fix . ExpW . pure . SuspendF

lamArgId :: T.Text -> Exp
lamArgId = Fix . ExpW . pure . LamArgIdF

record :: [T.Text] -> Exp
record = Fix . ExpW . pure . RecordF . fmap pure

typeishBinding :: T.Text -> Typeish Identity -> Binding Identity
typeishBinding name = Binding name . BindingTypeish 

union :: [T.Text] -> Typeish Identity
union = Union . fmap pure

-- in theory, this is:
--
-- expChangeW :: (forall. w a -> w' a) -> ExpF w e -> ExpF w' e
--
-- But that needs Rank2Types or similar and it's not worth it.
expChangeW :: (w Name -> w' Name) -> ExpF w e -> ExpF w' e
expChangeW f (AppF fun args) = AppF fun (map (_1 %~ f) args)
expChangeW f (LamF names e) = LamF (map f names) e
expChangeW f (RecordF names) = RecordF (map f names)
expChangeW f (SuspendF suspendSpec) = SuspendF $ go suspendSpec
  where go (SuspendSpec name args parents) = SuspendSpec (f name) (map (_1 %~ f) args) (go <$> parents)
expChangeW _ (LamArgIdF n) = LamArgIdF n
expChangeW _ (VarF n) = VarF n
expChangeW _ (LitF l) = LitF l


expChangeWM :: (Monad m) => (w Name -> m (w' Name)) -> ExpF w e -> m (ExpF w' e)
expChangeWM f (AppF fun args) = AppF fun <$> mapM (_1 %%~ f) args
expChangeWM f (LamF names e) = LamF <$> mapM f names <*> pure e
expChangeWM f (RecordF names) = RecordF <$> mapM f names
expChangeWM f (SuspendF suspendSpec) = SuspendF <$> go suspendSpec
  where go (SuspendSpec name args parents) = SuspendSpec <$> f name <*> mapM (_1 %%~ f) args <*> mapM go parents
expChangeWM _ (VarF n) = pure $ VarF n
expChangeWM _ (LamArgIdF n) = pure $ LamArgIdF n
expChangeWM _ (LitF l) = pure $ LitF l

typeishChangeW f (Union cons) = Union $ f <$> cons

type ExpWithId i = Fix (ExpW (Id.WithId i Identity))
type BindingWithId i = Id.WithId i Identity (Binding (Id.WithId i Identity))

bindingWithId :: (Monad m) => m i -> Binding Identity -> m (BindingWithId i)
bindingWithId gen (Binding name contents) = do
  bindingId <- gen
  Id.WithId bindingId . Identity . Binding name <$> case contents of
     BindingExp exp -> BindingExp <$> expsWithIdM gen exp
     BindingTypeish typeish -> BindingTypeish <$> typeishWithIdM gen typeish


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

typeishWithIdM :: Monad m => m i -> Typeish Identity -> m (Typeish (Id.WithId i Identity))
typeishWithIdM gen (Union constructors) = Union <$> traverse (addId gen) constructors

mapBindingId :: (i1 -> i2) -> BindingWithId i1 -> BindingWithId i2
mapBindingId f (Id.WithId i (Identity (Binding n v))) = Id.WithId (f i) $ Identity $ Binding n $ mapBindingContentsId f v

mapBindingContentsId f (BindingExp v) = BindingExp $ cata go v
  where go (ExpW withId) = Fix $ ExpW $ Id.mapId f $ (expChangeW (Id.mapId f) <$> withId)
mapBindingContentsId f (BindingTypeish t) = BindingTypeish $ typeishChangeW (Id.mapId f) t

type Resolved i = Map i i

type ArgNameDuplicates i = MonoidMap Name (Set i)
type UndefinedNames i = MonoidMap Name (Set i)
data NameResolutionError i = DuplicatedArgNames (ArgNameDuplicates i) | UndefinedNames (UndefinedNames i)
  deriving (Eq, Ord, Show)



data GlobalNames i = GlobalNames { globalNamesTopLevelBindings :: Map Name i, globalNamesArgs :: Map Name i }

globalNames :: (Ord i) => GlobalNames i -> [BindingWithId i] -> Either (ArgNameDuplicates i) (GlobalNames i)
globalNames (GlobalNames builtinsIds builtinsArgIds) bindings = GlobalNames (builtinsIds `Map.union` topLevelBindingNamesFromBindingWithExps bindings) <$> (argNamesFromBindings builtinsArgIds bindings)

contentsFromBinding :: BindingWithId i -> BindingContents (Id.WithId i Identity)
contentsFromBinding (Id.WithId _ (Identity (Binding _ contents))) = contents

bindingName :: BindingWithId i -> Name
bindingName (Id.WithId _ (Identity (Binding n _))) = n

bindingExp :: BindingWithId i -> Maybe (ExpWithId i)
bindingExp = bindingContentsExp . contentsFromBinding

bindingContentsExp (BindingExp exp) = Just exp
bindingContentsExp _ = Nothing

bindingTypeish :: BindingWithId i -> Maybe (Typeish (Id.WithId i Identity))
bindingTypeish = bindingContentsTypeish . contentsFromBinding

bindingContentsTypeish (BindingTypeish typeish) = Just typeish
bindingContentsTypeish _ = Nothing

bindingExps :: [BindingWithId i] -> [ExpWithId i]
bindingExps = catMaybes . map bindingExp

bindingTypeishs :: [BindingWithId i] -> [Typeish (Id.WithId i Identity)]
bindingTypeishs = catMaybes . map bindingTypeish

smooshEithers :: Monoid l => (r -> r -> Either l r) -> (r -> l -> l) -> Either l r -> Either l r -> Either l r
smooshEithers _     _      (Left err1) (Left err2) = Left $ err1 `mappend` err2
smooshEithers _     fOkErr (Right ok)  (Left err)  = Left $ fOkErr ok err
smooshEithers _     fOkErr (Left err)  (Right ok)  = Left $ fOkErr ok err
smooshEithers fOkOk _      (Right ok1) (Right ok2) = fOkOk ok1 ok2


topLevelBindingNamesFromBindingWithExps :: (Ord i) => [BindingWithId i] -> Map Name i
topLevelBindingNamesFromBindingWithExps = Map.fromList . catMaybes . map (\(Id.WithId _ (Identity (Binding name contents))) -> ((name,) . expTopId) <$> bindingContentsExp contents)

expTopId :: Fix (ExpW (Id.WithId i Identity)) -> i
expTopId (Fix (ExpW (Id.WithId i _))) = i

argNamesFromBindings :: (Ord i) => Map Name i -> [BindingWithId i] -> Either (ArgNameDuplicates i) (Map Name i)
argNamesFromBindings builtinsArgsIds bindings = argNamesFromExps builtinsArgsIds (catMaybes $ typeishArgIds <$> bindings) (bindingExps bindings)

argNamesFromExps :: (Ord i) => Map Name i -> [Map Name i] -> [ExpWithId i] -> Either (ArgNameDuplicates i) (Map Name i)
argNamesFromExps builtinsArgsIds typeishsArgsIds exps = combineAll $ (Right <$> typeishsArgsIds) ++ (Right builtinsArgsIds):(map argNamesFromExp exps)
  where
    argNamesFromExp :: (Ord i) => ExpWithId i -> Either (ArgNameDuplicates i) (Map Name i)
    argNamesFromExp = cata $ \(ExpW (Id.WithId i (Identity v))) ->
       case v of
         LamF args e -> combineAll $ e:[Right $ Map.singleton name id | (Id.WithId id (Identity name)) <- args]
         RecordF args -> combineAll $ [Right $ Map.singleton name id | (Id.WithId id (Identity name)) <- args]
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


typeishArgIds :: BindingWithId i -> Maybe (Map Name i)
typeishArgIds binding = go <$> bindingTypeish binding
  where go (Union constructors) = Map.fromList [ (name, id) 
                                               | (Id.WithId id (Identity name)) <- constructors
                                               ]
                          

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

resolveExpVars :: forall i. (Ord i) => GlobalNames i -> ExpWithId i -> Either (UndefinedNames i) (Resolved i)
resolveExpVars globalNames expr = go Map.empty expr
  where
    go env (Fix (ExpW (Id.WithId i (Identity e)))) = go' env i e
    go' env i (AppF e args) = concatExpVars $ (go env e):[lookupArg env arg | arg <- args]
    go' env i (LamF names e) = go (addNamesToEnv names env) e
      where 
        addNamesToEnv :: [Id.WithId i Identity T.Text] -> Map Name i -> Map Name i
        addNamesToEnv names = Map.union (Map.fromList [(name, id) | (Id.WithId id (Identity name)) <- names])
    go' env i (RecordF _) = Right $ Map.empty -- a RecordF is like a degenerate case of a lam that just has no body and returns its arguments

    go' env i (LamArgIdF name) = lookupArgName i name
    go' env i (VarF name) = resolveVarName env i name
    go' env i (SuspendF suspendSpec) = resolveSuspendSpec env suspendSpec
    go' env i (LitF _) = Right $ Map.empty

    resolveVarName :: forall a. Map Name i -> a -> Name -> Either (MonoidMap Name (Set a)) (Map a i)
    resolveVarName env i name = case Map.lookup name env of
                                  Just targetId -> Right $ Map.singleton i targetId
                                  Nothing -> case Map.lookup name (globalNamesTopLevelBindings globalNames) of
                                               Just targetId -> Right $ Map.singleton i targetId
                                               Nothing -> Left $ MonoidMap $ Map.singleton name (Set.singleton i)

    resolveSuspendSpec env (SuspendSpec (Id.WithId id (Identity name)) args parents) = concatExpVars $ (resolveVarName env id name):[lookupArg env arg | arg <- args] ++ [resolveSuspendSpec env parent | parent <- parents]

    lookupArg :: Map Name i -> (Id.WithId i Identity Name, Fix (ExpW (Id.WithId i Identity))) -> Either (UndefinedNames i) (Resolved i)
    lookupArg env ((Id.WithId id (Identity (argName))), exp) = concatExpVars [ lookupArgName id argName 
                                                                             , go env exp
                                                                             ]
    lookupArgName :: i -> Name -> Either (MonoidMap Name (Set i)) (Map i i)
    lookupArgName id argName = case Map.lookup argName (globalNamesArgs globalNames) of
                                 Just targetId -> Right $ Map.singleton id targetId
                                 Nothing -> Left $ MonoidMap $ Map.singleton argName (Set.singleton id)

bindingDiffTree :: Resolved T.Text -> BindingWithId T.Text -> DiffTree
bindingDiffTree env (Id.WithId id (Identity (Binding name contents))) = bindingContentsDiffTree env id name contents

bindingContentsDiffTree env id name (BindingExp exp) = DiffTree id "BindingExp" (Just name) [expDiffTree env exp]
bindingContentsDiffTree env id name (BindingTypeish typeish) = typeishDiffTree env id typeish

bindingDiffTrees :: GlobalNames T.Text -> [BindingWithId T.Text] -> Either (NameResolutionError T.Text) [DiffTree]
bindingDiffTrees builtins bindings = do
  resolved <- resolveVars builtins bindings
  pure $ map (bindingDiffTree resolved) bindings

expDiffTree :: Resolved T.Text -> ExpWithId T.Text -> DiffTree
expDiffTree env = cata $ \(ExpW (Id.WithId i (Identity v))) -> case v of
  AppF f args        -> DiffTree i "App" Nothing $
                                         f:[DiffTree argId
                                                     "AppArg"
                                                     (Just arg)
                                                     [e]
                                           | (Id.WithId argId (Identity arg), e) <- args
                                           ]
  LamF args body      -> DiffTree i "Lam" Nothing $
                                          [DiffTree argId
                                                    "LamArg"
                                                    (Just arg) 
                                                    []
                                          | (Id.WithId argId (Identity arg)) <- args
                                          ] ++ [body]
  RecordF fields         -> DiffTree i "Record" Nothing
                                                [DiffTree fieldId
                                                          "RecordField"
                                                          (Just field) 
                                                          []
                                                | (Id.WithId fieldId (Identity field)) <- fields
                                                ]


  VarF name            -> DiffTree i "Var" (Just name) 
                                          --[DiffTree (i `T.append` ".Ref")
                                          --          "METADATA-REF"
                                          --          (Data.Foldable.fold $ Map.lookup i env)
                                          --          []
                                          --]
                                          []
  SuspendF suspendSpec -> suspendSpecDiffTree suspendSpec
  
  LamArgIdF name       -> DiffTree i "LamArgId" (Just (T.pack $ show name)) []
  LitF (Number n)      -> DiffTree i "LitNumber" (Just (T.pack $ show n)) []
  LitF (Text t)        -> DiffTree i "LitText" (Just t) []

suspendSpecDiffTree (SuspendSpec (Id.WithId i (Identity name)) args parents) = DiffTree i "Suspend" (Just name) $
                                                                                                    [DiffTree argId
                                                                                                               "SuspendArg"
                                                                                                               (Just arg)
                                                                                                               [e]
                                                                                                    | (Id.WithId argId (Identity arg), e) <- args
                                                                                                    ] ++ 
                                                                                                    [ DiffTree (i `T.append` "-parent-" `T.append` (T.pack . show $ parentNumber))
                                                                                                               "SuspendParent"
                                                                                                               Nothing
                                                                                                               [suspendSpecDiffTree parent]
                                                                                                    | (parentNumber, parent) <- zip [0..] parents]

typeishDiffTree env id (Union constructors) = DiffTree id
                                                       "Union"
                                                       Nothing
                                                       [ DiffTree constructorId "UnionCon" (Just constructor) []
                                                       | Id.WithId constructorId (Identity constructor) <- constructors
                                               ]
