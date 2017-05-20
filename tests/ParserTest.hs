{-# LANGUAGE FlexibleInstances, InstanceSigs, DeriveTraversable, ScopedTypeVariables, TypeOperators, FlexibleContexts, UndecidableInstances #-}
module ParserTest where

import Control.Lens
import Data.Functor.Foldable.Extended
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Error

import qualified Parser as Parser
import qualified Lam as Lam

newtype TestExp = TestExp Lam.Exp

instance Eq TestExp where
  TestExp (Fix (Lam.ExpW (Identity a))) == TestExp (Fix (Lam.ExpW (Identity b))) = go a b
    where Lam.LamF args1 body1 `go` Lam.LamF args2 body2 = args1 == args2 && (TestExp body1) == (TestExp body2)
          Lam.AppF fun1 args1  `go` Lam.AppF fun2 args2  = (TestExp fun1) == (TestExp fun2) && testExpArgs args1 == testExpArgs args2
          Lam.RecordF args1    `go` Lam.RecordF args2    = args1 == args2
          Lam.VarF name1       `go` Lam.VarF name2       = name1 == name2
          Lam.SuspendF spec1   `go` Lam.SuspendF spec2   = TestSpec spec1 == TestSpec spec2
          Lam.LamArgIdF name1  `go` Lam.LamArgIdF name2  = name1 == name2
          Lam.LitF lit1        `go` Lam.LitF lit2        = lit1 == lit2
          _                    `go` _                    = False

instance Show TestExp where
  showsPrec _ (TestExp (Fix (Lam.ExpW (Identity a)))) = go a
    where
      go (Lam.LamF args body) = ("LamF " ++) . showsPrec 0 args . (" " ++) . showsPrec 0 (TestExp body)
      go (Lam.AppF fun args)  = ("AppF " ++) . showsPrec 0 (TestExp fun) . (" " ++) . showsPrec 0 (testExpArgs args)
      go (Lam.RecordF args)   = ("RecordF " ++) . showsPrec 0 args
      go (Lam.VarF name)      = ("VarF " ++) . showsPrec 0 name
      go (Lam.SuspendF spec)  = ("SuspendF " ++) . showsPrec 0 (TestSpec spec)
      go (Lam.LamArgIdF name) = ("LamArgIdF " ++) . showsPrec 0 name
      go (Lam.LitF val)       = ("LitF " ++) . showsPrec 0 val

newtype TestSpec = TestSpec (Lam.SuspendSpec Identity Lam.Exp)

instance Eq TestSpec where
  TestSpec (Lam.SuspendSpec name1 args1 parents1) == TestSpec (Lam.SuspendSpec name2 args2 parents2) = name1 == name2 && testExpArgs args1 == testExpArgs args2 && (TestSpec <$> parents1) == (TestSpec <$> parents2)

instance Show TestSpec where
  showsPrec _ (TestSpec (Lam.SuspendSpec name args parents)) = ("SuspendSpec " ++) . (showsPrec 10 name) . (showsPrec 10 (testExpArgs args)) . (" " ++) . (showsPrec 10 (TestSpec <$> parents))

testExpArgs :: [(n, Lam.Exp)] -> [(n, TestExp)]
testExpArgs = traverse . _2 %~ TestExp

parseWithEof :: String -> (Text.Megaparsec.Parsec Text.Megaparsec.Dec String () -> Text.Megaparsec.Parsec Text.Megaparsec.Dec String a) -> Either (Text.Megaparsec.Error.ParseError Char Text.Megaparsec.Error.Dec) a
parseWithEof s p = parse (do r <- p Parser.sc 
                             Text.Megaparsec.eof
                             pure r)
                         "inline" s



parseExp :: String -> Either (Text.Megaparsec.Error.ParseError Char Text.Megaparsec.Error.Dec) (TestExp)
parseExp s = TestExp <$> parseWithEof s Parser.pExp

mkTestExp :: forall a. Lam.Exp -> Either a TestExp
mkTestExp = Right . TestExp

tests :: TestTree
tests = testGroup "Parser"
  [ testCase "Exp number" $
      parseExp "3" 
        @?= (mkTestExp $ Lam.lit $ Lam.Number 3)
  , testCase "Exp string" $
      parseExp "\"here\"" 
        @?= (mkTestExp $ Lam.lit $ Lam.Text "here")
  , testCase "Exp var" $
      parseExp "referent"
        @?= (mkTestExp $ Lam.var "referent")
  , testCase "Exp lam" $
      parseExp "\\this, that -> 3"
        @?= (mkTestExp $ Lam.lam ["this", "that"] (Lam.lit $ Lam.Number 3))
  , testCase "Exp app" $
      parseExp "plus plus_1:3 plus_2:4"
        @?= (mkTestExp $ Lam.app (Lam.var "plus") [("plus_1", Lam.lit $ Lam.Number 3), ("plus_2", Lam.lit $ Lam.Number 4)])
  , testCase "Arg no space" $
      (testExpArgs . pure <$> (parseWithEof "plus_1:7" Parser.pArg))
        @?= Right [("plus_1", TestExp $ Lam.lit $ Lam.Number 7)]
  , testCase "Arg with space" $
      (testExpArgs . pure <$> (parseWithEof "plus_2 : 8" Parser.pArg))
        @?= Right [("plus_2", TestExp $ Lam.lit $ Lam.Number 8)]
  , testCase "ExpApp args" $
      (TestExp <$> (parseWithEof "plus plus_1:10 plus_2 : 9" Parser.pExpApp))
        @?= (mkTestExp $ Lam.app (Lam.var "plus") [("plus_1", Lam.lit $ Lam.Number 10), ("plus_2", Lam.lit $ Lam.Number 9)])
  ]
