{-# LANGUAGE DeriveTraversable, ScopedTypeVariables #-}
module Lam where

import Prelude hiding (exp, id)

import Data.Functor.Foldable
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

bindingDiffTree :: BindingWithId T.Text -> DiffTree
bindingDiffTree (Id.WithId id (Binding name exp)) = DiffTree id "Binding" (Just name) [expDiffTree exp]

expDiffTree :: ExpWithId T.Text -> DiffTree
expDiffTree = cata $ \(Id.WithIdF (Id.WithId i v)) -> case v of
  LamF args body  -> DiffTree i "Lam" Nothing $
                                      [DiffTree (argId i n)
                                                 "LamArg"
                                                 (Just arg) 
                                                 []
                                      | (n, arg) <- zip [0..] args
                                      ] ++ [body]
  VarF name       -> DiffTree i "Var" (Just name) []
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
