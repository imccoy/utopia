module Code where

import Lam

v1 :: [Binding Exp]
v1 = [ Binding "add2" $ lam ["n"] $
           app (var "+") [lit (Number 2), var "n"]
     , Binding "main" $ lam [] $
           app (var "add2") [lit (Number 5)]
     ]

v2 :: [Binding Exp]
v2 = [ Binding "add2twice" $ lam ["n"] $
           app (var "+")
               [ lit (Number 2)
               , app (var "+") [ lit (Number 2), var "n"]
               ]
     , Binding "main" $ lam [] $
           app (var "add2twice") [lit (Number 5)]
     ]
