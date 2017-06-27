{-# LANGUAGE FlexibleInstances, InstanceSigs, DeriveTraversable, ScopedTypeVariables, TypeOperators, FlexibleContexts, UndecidableInstances #-}
module Lam where

import Prelude hiding (exp, id)

import Control.Lens
import Data.Foldable
import Data.Functor.Foldable.Extended
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import DiffTree (DiffTree(..))
import qualified Id
import MonoidMap (MonoidMap(..))

type Name = T.Text

data Literal = Number Integer | Text T.Text
  deriving (Show, Eq)

data SuspendSpec w e = SuspendSpec (w Name) [(w Name, e)] [SuspendSpec w e]
  deriving (Functor, Prelude.Foldable, Traversable)

deriving instance (Show e, Show (w Name)) => Show (SuspendSpec w e)
deriving instance (Eq e, Eq (w Name)) => Eq (SuspendSpec w e)

newtype Susable w = Susable [[w Name]]

deriving instance (Show (w Name)) => Show (Susable w)
deriving instance (Eq (w Name)) => Eq (Susable w)

data ExpF w e = LamF (Susable w) [w Name] e
              | AppF e [(w Name, e)]
              | VarF Name
              | SuspendF (SuspendSpec w e)
              | LamArgIdF Name
              | LitF Literal
  deriving (Functor, Prelude.Foldable, Traversable)

deriving instance (Show e, Show (w e), Show (w Name)) => Show (ExpF w e)
deriving instance (Eq e, Eq (w e), Eq (w Name)) => Eq (ExpF w e)


data Binding w = Binding Name (BindingContents w)

deriving instance (Show (BindingContents w)) => Show (Binding w)

data BindingContents w = BindingExp (Fix (ExpW w))
                       | BindingTypeish (Typeish w)

deriving instance (Show (w Name), Show (Fix (ExpW w))) => Show (BindingContents w)

data Typeish w = Union [w Name] | Record [w Name]

deriving instance (Show (w Name)) => Show (Typeish w)
deriving instance (Eq (w Name)) => Eq (Typeish w)

newtype ExpW w e = ExpW (w (ExpF w e))
  deriving (Functor, Prelude.Foldable, Traversable)

type Exp = Fix (ExpW Identity)



expBinding :: T.Text -> Exp -> Binding Identity
expBinding name exp = Binding name $ BindingExp exp

lam :: [T.Text] -> Exp -> Exp
lam args body = Fix $ ExpW $ pure $ LamF (Susable []) (pure <$> args) body

lamS :: [[Name]] -> [T.Text] -> Exp -> Exp
lamS susables args body = Fix $ ExpW $ pure $ LamF (Susable $ (fmap (fmap Identity)) susables) (pure <$> args) body


app :: Exp -> [(T.Text, Exp)] -> Exp
app f args = Fix $ ExpW $ pure $ AppF f (traverse . _1 %~ (\name -> pure name) $ args)

var :: T.Text -> Exp
var = Fix . ExpW . pure . VarF

lit :: Literal -> Exp
lit = Fix . ExpW . pure . LitF

suspendSpec :: Name -> [(T.Text, Exp)] -> [SuspendSpec Identity Exp] -> SuspendSpec Identity Exp
suspendSpec suspendedName args parents = SuspendSpec (pure suspendedName)
                                                     (traverse . _1 %~ (\argName -> pure argName) $ args) 
                                                     parents

suspendSpecWithArg :: SuspendSpec Identity Exp -> (T.Text, Exp) -> SuspendSpec Identity Exp
suspendSpecWithArg (SuspendSpec name args parents) arg = SuspendSpec name ((_1 %~ pure $ arg):args) parents

suspendSpecWithParent :: SuspendSpec Identity Exp -> SuspendSpec Identity Exp -> SuspendSpec Identity Exp
suspendSpecWithParent (SuspendSpec name args parents) parent = SuspendSpec name args (parent:parents)



suspend :: SuspendSpec Identity Exp -> Exp
suspend = Fix . ExpW . pure . SuspendF

lamArgId :: T.Text -> Exp
lamArgId = Fix . ExpW . pure . LamArgIdF


typeishBinding :: T.Text -> Typeish Identity -> Binding Identity
typeishBinding name = Binding name . BindingTypeish 

union :: [T.Text] -> Typeish Identity
union = Union . fmap pure

record :: [T.Text] -> Typeish Identity
record = Record . fmap pure

-- in theory, this is:
--
-- expChangeW :: (forall. w a -> w' a) -> ExpF w e -> ExpF w' e
--
-- But that needs Rank2Types or similar and it's not worth it.
expChangeW :: (w Name -> w' Name) -> ExpF w e -> ExpF w' e
expChangeW f (AppF fun args) = AppF fun (map (_1 %~ f) args)
expChangeW f (LamF (Susable ss) names e) = LamF (Susable $ fmap (fmap f) ss) (map f names) e
expChangeW f (SuspendF spec) = SuspendF $ go spec
  where go (SuspendSpec name args parents) = SuspendSpec (f name) (map (_1 %~ f) args) (go <$> parents)
expChangeW _ (LamArgIdF n) = LamArgIdF n
expChangeW _ (VarF n) = VarF n
expChangeW _ (LitF l) = LitF l


expChangeWM :: (Monad m) => (w Name -> m (w' Name)) -> ExpF w e -> m (ExpF w' e)
expChangeWM f (AppF fun args) = AppF fun <$> mapM (_1 %%~ f) args
expChangeWM f (LamF (Susable ss) names e) = LamF <$> (Susable <$> mapM (mapM f) ss) <*> mapM f names <*> pure e
expChangeWM f (SuspendF spec) = SuspendF <$> go spec
  where go (SuspendSpec name args parents) = SuspendSpec <$> f name <*> mapM (_1 %%~ f) args <*> mapM go parents
expChangeWM _ (VarF n) = pure $ VarF n
expChangeWM _ (LamArgIdF n) = pure $ LamArgIdF n
expChangeWM _ (LitF l) = pure $ LitF l

typeishChangeW :: (t Name -> w Name) -> Typeish t -> Typeish w
typeishChangeW f (Union constructors) = Union $ f <$> constructors
typeishChangeW f (Record constructors) = Record $ f <$> constructors

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
typeishWithIdM gen (Record constructors) = Record <$> traverse (addId gen) constructors

mapBindingId :: (i1 -> i2) -> BindingWithId i1 -> BindingWithId i2
mapBindingId f (Id.WithId i (Identity (Binding n v))) = Id.WithId (f i) $ Identity $ Binding n $ mapBindingContentsId f v

mapBindingContentsId :: Functor f => (i1 -> i2) -> BindingContents (Id.WithId i1 f) -> BindingContents (Id.WithId i2 f)
mapBindingContentsId f (BindingExp v) = BindingExp $ cata go v
  where go (ExpW withId) = Fix $ ExpW $ Id.mapId f $ (expChangeW (Id.mapId f) <$> withId)
mapBindingContentsId f (BindingTypeish t) = BindingTypeish $ typeishChangeW (Id.mapId f) t

type Resolved i = Map i i

type ArgNameDuplicates i = MonoidMap Name (Set i)
type UndefinedNames i = MonoidMap Name (Set i)
data NameResolutionError i = DuplicatedArgNames (ArgNameDuplicates i) | UndefinedNames (UndefinedNames i)
  deriving (Eq, Ord, Show)



data GlobalNames i = GlobalNames { globalNamesTopLevelBindings :: Map Name i, globalNamesArgs :: Map Name i }

globalNamesFromBindings :: (Ord i) => GlobalNames i -> [BindingWithId i] -> Either (ArgNameDuplicates i) (GlobalNames i)
globalNamesFromBindings (GlobalNames builtinsIds builtinsArgIds) bindings = GlobalNames (builtinsIds `Map.union` topLevelBindingNames bindings) <$> (argNamesFromBindings builtinsArgIds bindings)

contentsFromBinding :: BindingWithId i -> BindingContents (Id.WithId i Identity)
contentsFromBinding (Id.WithId _ (Identity (Binding _ contents))) = contents

bindingName :: BindingWithId i -> Name
bindingName (Id.WithId _ (Identity (Binding n _))) = n

bindingExp :: BindingWithId i -> Maybe (ExpWithId i)
bindingExp = bindingContentsExp . contentsFromBinding

bindingContentsExp :: BindingContents t -> Maybe (Fix (ExpW t))
bindingContentsExp (BindingExp exp) = Just exp
bindingContentsExp _ = Nothing

bindingTypeish :: BindingWithId i -> Maybe (Typeish (Id.WithId i Identity))
bindingTypeish = bindingContentsTypeish . contentsFromBinding

bindingContentsTypeish :: BindingContents t -> Maybe (Typeish t)
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


topLevelBindingNames :: (Ord i) => [BindingWithId i] -> Map Name i
topLevelBindingNames = Map.fromList . catMaybes . map (\(Id.WithId topLevelId (Identity (Binding name contents))) -> (name,) <$> bindingContentsId topLevelId contents)
  where bindingContentsId _          (BindingExp exp) = Just . expTopId $ exp
        bindingContentsId topLevelId (BindingTypeish (Record _)) = Just topLevelId
        bindingContentsId _          _ = Nothing


expTopId :: Fix (ExpW (Id.WithId i Identity)) -> i
expTopId (Fix (ExpW (Id.WithId i _))) = i

argNamesFromBindings :: (Ord i) => Map Name i -> [BindingWithId i] -> Either (ArgNameDuplicates i) (Map Name i)
argNamesFromBindings builtinsArgsIds bindings = argNamesFromExps builtinsArgsIds (catMaybes $ typeishArgIds <$> bindings) (bindingExps bindings)

argNamesFromExps :: (Ord i) => Map Name i -> [Map Name i] -> [ExpWithId i] -> Either (ArgNameDuplicates i) (Map Name i)
argNamesFromExps builtinsArgsIds typeishsArgsIds exps = combineAll $ (Right <$> typeishsArgsIds) ++ (Right builtinsArgsIds):(map argNamesFromExp exps)
  where
    argNamesFromExp :: (Ord i) => ExpWithId i -> Either (ArgNameDuplicates i) (Map Name i)
    argNamesFromExp = cata $ \(ExpW (Id.WithId _ (Identity v))) ->
       case v of
         LamF _ args e -> combineAll $ e:[Right $ Map.singleton name id | (Id.WithId id (Identity name)) <- args]
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
        go (Record fields) = Map.fromList [ (name, id)
                                          | (Id.WithId id (Identity name)) <- fields
                                          ]
                                
                          

resolveVars :: (Ord i) => GlobalNames i -> [BindingWithId i] -> Either (NameResolutionError i) (Resolved i)
resolveVars builtins bindings = do
  globalNameBindings <- _Left %~ DuplicatedArgNames $ globalNamesFromBindings builtins bindings
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
    go' env _ (AppF e args) = concatExpVars $ (go env e):[lookupArg env arg | arg <- args]
    go' env _ (LamF (Susable susable) lamNames e) = Map.union <$> lamBodyResolved <*> susableResolved
      where 
        lamBodyResolved = go (Map.union namesEnv env) e
        namesEnv :: Map Name i
        namesEnv = Map.fromList [(lamName, id) | (Id.WithId id (Identity lamName)) <- lamNames]
        susableResolved :: Either (UndefinedNames i) (Resolved i)
        susableResolved = concatExpVars . fmap (concatExpVars . fmap (\(Id.WithId argId (Identity argName)) -> lookupArgName argId argName)) $ susable

    go' _   i (LamArgIdF name) = lookupArgName i name
    go' env i (VarF name) = resolveVarName env i name
    go' env _ (SuspendF spec) = resolveSuspendSpec env spec
    go' _   _ (LitF _) = Right $ Map.empty

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
    lookupArgName :: i -> Name -> Either (UndefinedNames i) (Map i i)
    lookupArgName id argName = case Map.lookup argName (globalNamesArgs globalNames) of
                                 Just targetId -> Right $ Map.singleton id targetId
                                 Nothing -> Left $ MonoidMap $ Map.singleton argName (Set.singleton id)

bindingsWithIdText :: [BindingWithId T.Text] -> T.Text
bindingsWithIdText = T.unlines . map bindingWithIdText
  where bindingWithIdText (Id.WithId id (Identity (Binding name bound))) = "Binding " <> id <> " " <> name <> "\n" <> indent (bindingText bound)
        bindingText (BindingExp exp) = expText exp
        bindingText (BindingTypeish typeish) = typeishText typeish
        expText (Fix (ExpW (Id.WithId id (Identity expBody)))) = lamTopCon expBody <> "@" <> id <> " " <> expBodyText expBody
        expBodyText (LamF _ names body) = namesText names <> "\n" <> indent (expText body)
        expBodyText (AppF fn args) = "(\n" <> indent (expText fn) <> "\n" <> ")\n" <>
                                     argsText args <> "\n"
        expBodyText (VarF name) = name
        expBodyText (SuspendF spec) = suspendSpecText spec
        expBodyText (LamArgIdF name) = name
        expBodyText (LitF literal) = T.pack $ show literal
        argsText args = "[" <> (T.intercalate "\n," [name <> "@" <> id <> ": \n" <> indent (expText exp) | (Id.WithId id (Identity name), exp) <- args]) <> "\n]"
        suspendSpecText (SuspendSpec (Id.WithId id (Identity name)) args parents) = name <> "@" <> id <> "\n" <>
                                                                                    argsText args <>
                                                                                    "[\n" <>
                                                                                    T.intercalate "\n," (indent . suspendSpecText <$> parents) <>
                                                                                    "\n]"
        namesText names = "[" <> T.intercalate ", " [name <> "@" <> id | (Id.WithId id (Identity name)) <- names] <> "]"
        typeishText (Union names) = "Union " <> namesText names
        typeishText (Record names) = "Record " <> namesText names
        indent = T.unlines . map ("  " <>) . T.lines

lamTopCon :: ExpF w e -> T.Text
lamTopCon (Lam.LamF _ _ _) = "LamF"
lamTopCon (Lam.AppF _ _) = "AppF"
lamTopCon (Lam.VarF _) = "VarF"
lamTopCon (Lam.SuspendF _) = "SuspendF"
lamTopCon (Lam.LamArgIdF _) = "LamArgIdF"
lamTopCon (Lam.LitF _) = "LitF"



bindingDiffTree :: BindingWithId T.Text -> DiffTree
bindingDiffTree (Id.WithId id (Identity (Binding name contents))) = bindingContentsDiffTree id name contents

bindingContentsDiffTree :: T.Text -> T.Text -> BindingContents (Id.WithId T.Text Identity) -> DiffTree
bindingContentsDiffTree id name (BindingExp exp) = DiffTree id "BindingExp" (Just name) [expDiffTree exp]
bindingContentsDiffTree id name (BindingTypeish typeish) = typeishDiffTree id name typeish

bindingDiffTrees :: [BindingWithId T.Text] -> [DiffTree]
bindingDiffTrees bindings = do
  bindingDiffTree <$> bindings

expDiffTree :: ExpWithId T.Text -> DiffTree
expDiffTree = cata $ \(ExpW (Id.WithId i (Identity v))) -> case v of
  AppF f args            -> DiffTree i "App" Nothing $
                                             f:[DiffTree argId
                                                         "AppArg"
                                                         (Just arg)
                                                         [e]
                                               | (Id.WithId argId (Identity arg), e) <- args
                                               ]
  LamF _ args body       -> DiffTree i "Lam" Nothing $
                                             [DiffTree argId
                                                       "LamArg"
                                                       (Just arg) 
                                                       []
                                             | (Id.WithId argId (Identity arg)) <- args
                                             ] ++ [body]

  VarF name            -> DiffTree i "Var" (Just name) 
                                          --[DiffTree (i `T.append` ".Ref")
                                          --          "METADATA-REF"
                                          --          (Data.Foldable.fold $ Map.lookup i env)
                                          --          []
                                          --]
                                          []
  SuspendF spec        -> suspendSpecDiffTree spec
  
  LamArgIdF name       -> DiffTree i "LamArgId" (Just (T.pack $ show name)) []
  LitF (Number n)      -> DiffTree i "LitNumber" (Just (T.pack $ show n)) []
  LitF (Text t)        -> DiffTree i "LitText" (Just t) []

suspendSpecDiffTree :: SuspendSpec (Id.WithId T.Text Identity) DiffTree -> DiffTree
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
                                                                                                    | (parentNumber, parent) <- zip [(0::Integer)..] parents]

typeishDiffTree :: T.Text -> T.Text -> Typeish (Id.WithId T.Text Identity) -> DiffTree
typeishDiffTree id name (Union constructors) = DiffTree id
                                                        "Union"
                                                        (Just name)
                                                        [ DiffTree constructorId "UnionCon" (Just constructor) []
                                                        | Id.WithId constructorId (Identity constructor) <- constructors
                                                ]
typeishDiffTree id name (Record constructors) = DiffTree id
                                                         "Record"
                                                         (Just name)
                                                         [ DiffTree constructorId "RecordCon" (Just constructor) []
                                                         | Id.WithId constructorId (Identity constructor) <- constructors
                                                ]
