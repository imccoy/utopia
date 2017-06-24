{-# LANGUAGE FlexibleInstances, InstanceSigs, DeriveTraversable, ScopedTypeVariables, TypeOperators, FlexibleContexts, UndecidableInstances, TupleSections, LambdaCase #-}
module ParserTest where

import Control.Lens
import Data.Functor.Foldable.Extended
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Error

import qualified Code
import qualified Parser as Parser
import qualified Lam as Lam

newtype TestExp = TestExp Lam.Exp

instance Eq TestExp where
  TestExp (Fix (Lam.ExpW (Identity a))) == TestExp (Fix (Lam.ExpW (Identity b))) = go a b
    where Lam.LamF ss1 args1 body1 `go` Lam.LamF ss2 args2 body2 = args1 == args2 && (TestExp body1) == (TestExp body2) && ss1 == ss2
          Lam.AppF fun1 args1      `go` Lam.AppF fun2 args2      = (TestExp fun1) == (TestExp fun2) && testExpArgs args1 == testExpArgs args2
          Lam.RecordF args1        `go` Lam.RecordF args2        = args1 == args2
          Lam.VarF name1           `go` Lam.VarF name2           = name1 == name2
          Lam.SuspendF spec1       `go` Lam.SuspendF spec2       = TestSpec spec1 == TestSpec spec2
          Lam.LamArgIdF name1      `go` Lam.LamArgIdF name2      = name1 == name2
          Lam.LitF lit1            `go` Lam.LitF lit2            = lit1 == lit2
          _                        `go` _                        = False

instance Show TestExp where
  showsPrec _ (TestExp (Fix (Lam.ExpW (Identity a)))) = go a
    where
      go (Lam.LamF ss args body) = ("(LamF " ++) . showsPrec 0 ss . (" " ++) . showsPrec 0 args . (" " ++) . showsPrec 0 (TestExp body) . (++ ")")
      go (Lam.AppF fun args)     = ("(AppF " ++) . showsPrec 0 (TestExp fun) . (" " ++) . showsPrec 0 (testExpArgs args) . (++ ")")
      go (Lam.RecordF args)      = ("(RecordF " ++) . showsPrec 0 args . (++ ")")
      go (Lam.VarF name)         = ("(VarF " ++) . showsPrec 0 name . (++ ")")
      go (Lam.SuspendF spec)     = ("(SuspendF " ++) . showsPrec 0 (TestSpec spec) . (++ ")")
      go (Lam.LamArgIdF name)    = ("(LamArgIdF " ++) . showsPrec 0 name . (++ ")")
      go (Lam.LitF val)          = ("(LitF " ++) . showsPrec 0 val . (++ ")")

newtype TestSpec = TestSpec (Lam.SuspendSpec Identity Lam.Exp)

instance Eq TestSpec where
  TestSpec (Lam.SuspendSpec name1 args1 parents1) == TestSpec (Lam.SuspendSpec name2 args2 parents2) = name1 == name2 && testExpArgs args1 == testExpArgs args2 && (TestSpec <$> parents1) == (TestSpec <$> parents2)

instance Show TestSpec where
  showsPrec _ (TestSpec (Lam.SuspendSpec name args parents)) = ("SuspendSpec " ++) . (showsPrec 10 name) . (" " ++) . (showsPrec 10 (testExpArgs args)) . (" " ++) . (showsPrec 10 (TestSpec <$> parents))

data TestBindingContents = TestExpBinding TestExp | TestTypeishBinding (Lam.Typeish Identity)

deriving instance Show TestBindingContents
deriving instance Eq TestBindingContents

testExpArgs :: [(n, Lam.Exp)] -> [(n, TestExp)]
testExpArgs = traverse . _2 %~ TestExp

parseWithEof :: String -> Text.Megaparsec.Parsec Text.Megaparsec.Dec String a -> Either (Text.Megaparsec.Error.ParseError Char Text.Megaparsec.Error.Dec) a
parseWithEof s p = parse (p <* Parser.scn <* Text.Megaparsec.eof)
                         "inline" (s ++ "\n")


type ParseResult r = Either (Text.Megaparsec.Error.ParseError Char Text.Megaparsec.Error.Dec) r

parseExp :: String -> ParseResult TestExp
parseExp s = TestExp <$> parseWithEof s Parser.pExp

parseBinding :: String -> ParseResult (Lam.Name, TestBindingContents)
parseBinding s = do Lam.Binding n binding <- parseWithEof s Parser.pBinding
                    pure (n, testBindingContents binding)

testBindingContents :: Lam.BindingContents Identity -> TestBindingContents
testBindingContents (Lam.BindingExp e) = TestExpBinding $ TestExp e
testBindingContents (Lam.BindingTypeish t) = TestTypeishBinding t

testBinding :: Lam.Binding Identity -> (Lam.Name, TestBindingContents)
testBinding (Lam.Binding n contents) = (n, testBindingContents contents)

parseBindings :: String -> ParseResult [(Lam.Name, TestBindingContents)]
parseBindings s = testBindings <$> parseWithEof s Parser.pBindings

testBindings :: [Lam.Binding Identity] -> [(Lam.Name, TestBindingContents)]
testBindings = map testBinding

mkTestExp :: Lam.Exp -> ParseResult TestExp
mkTestExp = Right . TestExp

mkTestExpBinding :: T.Text -> Lam.Exp -> ParseResult (Lam.Name, TestBindingContents)
mkTestExpBinding name binding = Right (name, TestExpBinding . TestExp $ binding)

mkTestExpsBinding :: [(T.Text, Lam.Exp)] -> ParseResult [(Lam.Name, TestBindingContents)]
mkTestExpsBinding nameBindings = Right [(name, TestExpBinding . TestExp $ binding) | (name, binding) <- nameBindings]



mkTestTypeishBinding :: T.Text -> Lam.Typeish Identity -> ParseResult [(Lam.Name, TestBindingContents)]
mkTestTypeishBinding name binding = Right [(name, TestTypeishBinding binding)]

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
      parseExp "\\(this that -> 3)"
        @?= (mkTestExp $ Lam.lam ["this", "that"] (Lam.lit $ Lam.Number 3))
  , testCase "Exp app" $
      parseExp "plus plus_1:(3) plus_2:(4)"
        @?= (mkTestExp $ Lam.app (Lam.var "plus") [("plus_1", Lam.lit $ Lam.Number 3), ("plus_2", Lam.lit $ Lam.Number 4)])
  , testCase "Arg no space" $
      (testExpArgs . pure <$> (parseWithEof "plus_1:7" (Parser.pArg Parser.pNonAppExp)))
        @?= Right [("plus_1", TestExp $ Lam.lit $ Lam.Number 7)]
  , testCase "Arg with space" $
      (testExpArgs . pure <$> (parseWithEof "plus_2 : 8 " (Parser.pArg Parser.pNonAppExp)))
        @?= Right [("plus_2", TestExp $ Lam.lit $ Lam.Number 8)]
  , testCase "Exp app" $
      parseExp "plus plus_1:3 plus_2:4"
        @?= (mkTestExp $ Lam.app (Lam.var "plus") [("plus_1", Lam.lit $ Lam.Number 3), ("plus_2", Lam.lit $ Lam.Number 4)])
  , testCase "Exp suspend with only name" $
      parseExp "'(plus)"
        @?= (mkTestExp $ Lam.suspend $ Lam.suspendSpec "plus"
                                                       []
                                                       [])
  , testCase "Exp suspend with arg" $
      parseExp "'(plus plus_2:(7))"
        @?= (mkTestExp $ Lam.suspend $ Lam.suspendSpec "plus"
                                                       [("plus_2", Lam.lit $ Lam.Number 7)]
                                                       [])
  , testCase "Exp suspend with arg and parent" $
      parseExp "'(plus plus_1:(3) ^(minus minus_1:(4)))"
        @?= (mkTestExp $ Lam.suspend $ Lam.suspendSpec "plus"
                                                       [("plus_1", Lam.lit $ Lam.Number 3)]
                                                       [Lam.suspendSpec "minus"
                                                                        [("minus_1", Lam.lit $ Lam.Number 4)]
                                                                        []
                                                       ])
  , testCase "Exp varArgId" $
      parseExp "*whowhat"
        @?= (mkTestExp $ Lam.lamArgId "whowhat")
  , testCase "Exp Record" $
      parseExp "{ a b }"
        @?= (mkTestExp $ Lam.record ["a", "b"])
  , testCase "Exp binding" $
      parseBinding "a = 1"
        @?= (mkTestExpBinding "a" (Lam.lit $ Lam.Number 1))
  , testCase "Exp binding with all the trimmings" $
      parseBinding "b = thingy thing:(\\(a -> plus plus_1:a plus_2:a) a:1) thingor:'(thang thang:3) thingy:*what"
        @?= (mkTestExpBinding "b" $ Lam.app (Lam.var "thingy")
                                            [("thing", Lam.app (Lam.lam ["a"] $
                                                                  Lam.app (Lam.var "plus")
                                                                          [("plus_1", Lam.var "a")
                                                                          ,("plus_2", Lam.var "a")]
                                                               )
                                                               [("a", Lam.lit $ Lam.Number 1)]
                                             )
                                            ,("thingor", Lam.suspend $ Lam.suspendSpec "thang" [("thang", Lam.lit $ Lam.Number 3)] [])
                                            ,("thingy", Lam.lamArgId "what")
                                            ]
            )
  , testCase "Two bindings" $
      parseBindings "a = \\(c -> 1)\nb=2"
        @?= (mkTestExpsBinding [("a", Lam.lam ["c"] $
                                        Lam.lit $ Lam.Number 1)
                               ,("b", Lam.lit $ Lam.Number 2)
                               ])
  , testCase "Union binding" $
      parseBindings "z = Union { a b }"
        @?= (mkTestTypeishBinding "z" (Lam.union ["a", "b"]))
  , testCase "collapse lines" $
      Parser.collapseLines ["   z", "a", " b", "         c", "d", " e", "f"]
        @?= ["   z", "a b c", "d e", "f"]
  , testCase "big thing is accurately transcribed" $
      parseBindings (Parser.collapseCode $ T.unpack Code.todoWeb')
        @?= Right (testBindings Code.todoWeb)
  ]
