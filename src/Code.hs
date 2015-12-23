module Code where

import Lam

v1 = Module [ Binding "add2" $ Lam ["n"] $
                  App (Var "+") [Lit (Number 2), Var "n"]
            , Binding "add2twice" $ Lam ["ntwice"] $
                  App (Var "add2") [App (Var "add2") [Var "ntwice"]]
            ]

v2 = Module [ Binding "addn" $ Lam ["n", "m"] $
                  App (Var "+") [Var "m", Var "n"]
            , Binding "addntwice" $ Lam ["ntwice", "mtwice"] $
                  App (Var "addn") [App (Var "addn") [Var "ntwice", Var "mtwice"], Var "ntwice"]
            ]
