module Lam where

import Data.Text (Text)

type Name = Text

data Module = Module [Binding]

data Binding = Binding Name Exp

data Literal = Number Int | Text Text

data Exp = Lam [Name] Exp | Var Name | App Exp [Exp] | Lit Literal
