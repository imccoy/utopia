module Code where

import Lam

v1 :: [Binding Exp]
v1 = [ Binding "add2" $ lam ["n"] $
           app (var "+") [("+_1", lit (Number 2)), ("+_2", var "n")]
     , Binding "main" $ lam [] $
           app (var "add2") [("n", lit (Number 5))]
     ]

v2 :: [Binding Exp]
v2 = [ Binding "add2twice" $ lam ["n"] $
           app (var "+")
               [ ("+_1", lit (Number 2))
               , ("+_2", app (var "+") [("+_1", lit (Number 2)), 
                                        ("+_2", var "n")])
               ]
     , Binding "main" $ lam [] $
           app (var "add2twice") [("n", lit (Number 5))]
     ]

-- data Event details env = Event Time details env
-- data EventDetails = ButtonClick
-- type Click = Event ButtonClick
-- buttonClicked :: (Suspended env Button) -> [Click env]
-- inputValue :: Input -> Time -> String
-- when :: [Event _ env] -> (env -> Suspended env' input) -> (input -> Time -> a) -> [Event a env']
-- eventDetails :: Event a _ -> a
-- map :: (a -> b) -> [a] -> [b]
-- 

--suspend = undefined
--
--todo :: [Binding Exp]
--todo = [ Binding "addTodo" $ lam [] $
--           app (var "button") [lit (Text "Add Todo")]
--     , Binding "todoTextEntry" $ lam [] $
--           app (var "textBox") []
--     , Binding "addTodoReq" $ lam [] $
--           app (var "buttonClicked") [suspend $ var "addTodo"]
--     , Binding "addTodoReqText" $ lam ["req"] $
--           app (var "when") 
--               [ var "addTodoReq"
--               , lam ["env"] $ suspend $ var "todoTextEntry"
--               , var "textboxValue"]
--     , Binding "savedEntries" $ lam [] $
--           app (var "map") [var "eventDetails"]
--     , Binding "app" $ lam [] $
--           app (var "listOf")
--              [ var "todoTextEntry"
--              , var "addTodo"
--              , var "savedEntries"
--              ]
--     ]
