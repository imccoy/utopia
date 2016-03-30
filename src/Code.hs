module Code where

import Lam

v1 :: [Binding Exp]
v1 = [ Binding "add2" $ lam ["n"] $
           app (var "+") [lit (Number 2), var "n"]
     , Binding "add2twice" $ lam ["ntwice"] $
           app (var "add2") [app (var "add2") [var "ntwice"]]
     , Binding "main" $ lam [] $
           app (var "add2twice") [lit (Number 5)]
     ]

v2 :: [Binding Exp]
v2 = [ Binding "addn" $ lam ["n", "m"] $
           app (var "+") [var "m", var "n"]
     , Binding "addntwice" $ lam ["ntwice", "mtwice"] $
           app (var "addn") [app (var "addn") [var "ntwice", var "mtwice"], var "ntwice"]
     , Binding "main" $ lam [] $
           app (var "addntwice") [lit (Number 7), lit (Number 3)]
     ]
