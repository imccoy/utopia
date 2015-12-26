module Lam where

import qualified Data.Text as T

import CodeTree

type Name = T.Text

data Module = Module [Binding]

data Binding = Binding Name Exp

data Literal = Number Int | Text T.Text

data Exp = Lam [Name] Exp | Var Name | App Exp [Exp] | Lit Literal

codeTree :: Module -> CodeTree
codeTree (Module bindings)
  = CodeTree "Module" Nothing
             [bindingCodeTree binding | binding <- bindings]

bindingCodeTree (Binding name exp)
  = CodeTree "Binding" (Just name)
             [expCodeTree exp]

expCodeTree = go
  where go (Lam args body) = CodeTree "Lam" Nothing
                                      [lamArgsCodeTree args
                                      ,lamBodyCodeTree body
                                      ]
        go (Var name) = CodeTree "Var" (Just name)
                                 []
        go (App f args) = CodeTree "App" Nothing
                                   [appFunCodeTree f
                                   ,appArgsCodeTree args
                                   ]
        go (Lit ((Number n))) = CodeTree "LitNumber" (Just (T.pack $ show n))
                                         []
        go (Lit ((Text t))) = CodeTree "LitText" (Just t)
                                       []

lamArgsCodeTree args = CodeTree "LamArgs" Nothing
                                [CodeTree "LamArg" (Just arg)
                                          []
                                |arg <- args
                                ]

lamBodyCodeTree body = CodeTree "LamBody" Nothing
                                [expCodeTree body]

appFunCodeTree fun = CodeTree "AppFun" Nothing
                              [expCodeTree fun]

appArgsCodeTree args = CodeTree "AppArgs" Nothing
                                [CodeTree "AppArg" Nothing
                                          [expCodeTree arg]
                                | arg <- args
                                ]
