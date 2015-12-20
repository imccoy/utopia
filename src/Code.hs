module Code where

import Lam

--v1 = Module [ Binding "add2" $ Lam ["n"] $
--                  App (Var "+") [Lit (Number 2), Var "n"]
--            , Binding "add2twice" $ Lam ["nn"] $
--                  App (Var "add2") [App (Var "add2") [Var "nn"]]
--            ]
--
--v2 = Module [ Binding "addn" $ Lam ["n", "m"] $
--                  App (Var "+") [Var "m", Var "n"]
--            , Binding "addntwice" $ Lam ["nn", "mm"] $
--                  App (Var "addn") [App (Var "addn") [Var "nn", Var "mm"], Var "nn"]
--            ]

v1 = Module [ Binding "n" $ Lit $ Number 4
            , Binding "m" $ Lit $ Number 2
            ]

v2 = Module [ Binding "m" $ Lit $ Number 4
            , Binding "n" $ Lit $ Number 2
            ]

