module Code where

import Lam

v1 = Module [ Binding "add2" $ Lam ["n"] $
                  App (Var "+") [Lit (Number 2), Var "n"]
            , Binding "add2twice" $ Lam ["n"] $
                  App (Var "add2") [App (Var "add2") [Var "n"]]
            ]

v2 = Module [ Binding "addn" $ Lam ["m", "n"] $
                  App (Var "+") [Var "m", Var "n"]
            , Binding "addntwice" $ Lam ["m", "n"] $
                  App (Var "addn") [App (Var "addn") [Var "m", Var "n"], Var "n"]
            ]

