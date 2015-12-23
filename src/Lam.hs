module Lam where

import qualified Data.Text as T

import DiffTree

type Name = T.Text

data Module = Module [Binding]

data Binding = Binding Name Exp

data Literal = Number Int | Text T.Text

data Exp = Lam [Name] Exp | Var Name | App Exp [Exp] | Lit Literal


diffTree (Module bindings)
  = DiffTree "M"
             "Module" Nothing
             [bindingDiffTree (T.pack $ show n) binding | (n, binding) <- zip [0..] bindings]

bindingDiffTree idPrefix (Binding name exp)
  = DiffTree (idPrefix `T.append` ".name") 
             "Binding" (Just name)
             [expDiffTree idPrefix exp]

expDiffTree idPrefix = go
  where go (Lam args body) = DiffTree (idPrefix `T.append` ".lam")
                                      "Lam" Nothing
                                      [lamArgsDiffTree (idPrefix `T.append` ".lam") args
                                      ,lamBodyDiffTree (idPrefix `T.append` ".lam") body
                                      ]
        go (Var name) = DiffTree (idPrefix `T.append` ".var")
                                 "Var" (Just name)
                                 []
        go (App f args) = DiffTree (idPrefix `T.append` ".app")
                                   "App" Nothing
                                   [appFunDiffTree (idPrefix `T.append` ".app") f
                                   ,appArgsDiffTree (idPrefix `T.append` ".app") args
                                   ]
        go (Lit ((Number n))) = DiffTree (idPrefix `T.append` ".lit")
                                         "LitNumber" (Just (T.pack $ show n))
                                         []
        go (Lit ((Text t))) = DiffTree (idPrefix `T.append` ".lit")
                                       "LitText" (Just t)
                                       []

lamArgsDiffTree idPrefix args = DiffTree (idPrefix `T.append` ".args")
                                         "LamArgs" Nothing
                                         [DiffTree (idPrefix `T.append` ".args[" `T.append` T.pack (show n) `T.append` "]")
                                                   "LamArg" (Just arg)
                                                   []
                                         |(n,arg) <- zip [0..] args
                                         ]

lamBodyDiffTree idPrefix body = DiffTree (idPrefix `T.append` ".body")
                                         "LamBody" Nothing
                                         [expDiffTree (idPrefix `T.append` ".body") body]

appFunDiffTree idPrefix fun = DiffTree (idPrefix `T.append` ".fun")
                                       "AppFun" Nothing
                                       [expDiffTree (idPrefix `T.append` ".fun") fun]

appArgsDiffTree idPrefix args = DiffTree (idPrefix `T.append` ".args")
                                         "AppArgs" Nothing
                                         [DiffTree (idPrefix `T.append` ".args[" `T.append` T.pack (show n) `T.append` "]")
                                                   "AppArg" Nothing
                                                   [expDiffTree (idPrefix `T.append` ".args[" `T.append` T.pack (show n) `T.append` "]") arg]
                                         |(n,arg) <- zip [0..] args
                                         ]
