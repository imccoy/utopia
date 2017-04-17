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

tracey :: [Binding Exp]
tracey = [ Binding "gen1" $ lam ["gen1_n"] $ 
               var "gen1_n"
         , Binding "gen2" $ lam ["gen2_n"] $
               var "gen2_n"
         , Binding "glue1" $ lam [] $
               listOf [app (var "gen1") [("gen1_n", lit (Number 1))]
                      ,app (var "gen1") [("gen1_n", lit (Number 2))]
                      ,app (var "gen1") [("gen1_n", lit (Number 3))]
                      ]
         , Binding "glue2a" $ lam [] $
               app (var "suspensionFrameList")
                   [("suspensionFrameList_suspension", suspend $ suspendSpec "gen1" [] [])]
         , Binding "glue2" $ lam [] $
               app (var "listMap")
                   [("listMap_list", (app (var "glue2a") []))
                   ,("listMap_f", lam ["builtin-listMap-listMap_f-elem"] $
                                      app (var "gen2")
                                          [("gen2_n", app (var "frameArg") [("frameArg_frame", var "builtin-listMap-listMap_f-elem"), ("frameArg_arg", lamArgId "gen1_n")])])
                   ]
         , Binding "glue3" $ lam [] $
               app (var "suspensionFrameList")
                   [("suspensionFrameList_suspension", suspend $ suspendSpec "gen2" [] [])]
         , Binding "main" $ lam [] $
               listOf [ var "glue1"
--                      , var "glue2a"
                      , var "glue2"
                      , var "glue3"
                      ]
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




listAdd :: Exp -> Exp -> Exp
listAdd elem list = app (var "listAdd")
                        [ ("listAdd_list", list)
                        , ("listAdd_elem", elem)
                        ]

listOf :: [Exp] -> Exp
listOf exps = foldr listAdd (var "listEmpty") exps

buttonWeb :: [Binding Exp]
buttonWeb = [ Binding "incrementButton" $ lam [] $
                app (var "htmlButton")
                    [ ("htmlButton_text", lit (Text "+"))
                    ]
            , Binding "decrementButton" $ lam [] $
                app (var "htmlButton")
                    [ ("htmlButton_text", lit (Text "-"))
                    ]

            , Binding "clickCount" $ lam ["clickCount_button"] $
                app (var "listSum")
                    [("listSum_list", app (var "listMap")
                                          [("listMap_list" ,app (var "suspensionFrameList")
                                                                [("suspensionFrameList_suspension", (var "clickCount_button"))])
                                          ,("listMap_f", lam ["builtin-listMap-listMap_f-elem"] $
                                                           app (var "listLength")
                                                               [("listLength_list", app (var "htmlElementEvents")
                                                                                        [("htmlElementEvents_element", app (var "frameResult")
                                                                                                                           [("frameResult_frame", var "builtin-listMap-listMap_f-elem")])
                                                                                        ])
                                                               ])
                                          ])
                    ]
            , Binding "main" $ lam [] $
                listOf [ app (var "htmlText")
                             [ ("htmlText_text", lit (Text "Oh, hello there"))
                             ]
                       , app (var "htmlText")
                             [ ("htmlText_text", app (var "numberToText")
                                                     [ ("numberToText_number", app (var "-")
                                                                                   [("-_1", app (var "clickCount")
                                                                                                [("clickCount_button", suspend $ suspendSpec "incrementButton" [] [])])
                                                                                   ,("-_2", app (var "clickCount")
                                                                                                [("clickCount_button", suspend $ suspendSpec "decrementButton" [] [])])
                                                                                   ]
                                                     )]
                             )]
 
                       , app (var "incrementButton")
                             []
                       , app (var "decrementButton")
                             []
                       ]
            ] 

letBindings :: [(Name, Exp)] -> Exp -> Exp
letBindings namesExps exp = app (lam (map fst namesExps) exp)
                                namesExps

-- give a name to the function whose frame is going to bind these names
namedLetBindings :: Name -> [(Name, Exp)] -> Exp -> Exp
namedLetBindings name namesExps inner = letBindings [(name, lam (map fst namesExps) inner)]
                                                    (app (var name) namesExps)

web = buttonWeb

todoWeb :: [Binding Exp]
todoWeb = [ Binding "todoTextBox" $ lam [] $
              app (var "htmlInputText") []
          , Binding "todoAddButton" $ lam [] $
              app (var "htmlButton") [("htmlButton_text", lit (Text "Add"))]
          , Binding "todoForm" $ lam ["todoForm_n"] $
              listOf [ app (var "todoTextBox") []
                     , app (var "todoAddButton") []
                     ]
          , Binding "savedTodoForms" $ lam [] $
              suspend $ suspendSpec "todoForm" [] []
          , Binding "unsavedTodoForms" $ lam [] $
              lit (Text "hah")
          , Binding "savedTodoWidgets" $ lam [] $
              lit (Text "hoh")
          , Binding "main" $ lam [] $
              listOf [ app (var "unsavedTodoForms") []
                     , app (var "savedTodoWidgets") []
                     ]
          ] 

--
--todo :: [Binding Exp]
--todo = [ Binding "addTodo" $ lam [] $
--             app (var "button") [("button-text", lit (Text "Add Todo"))]
--       , Binding "todoTextEntry" $ lam [] $
--             app (var "textBox") []
--       , Binding "addTodoReq" $ lam [] $
--             app (var "buttonClicked") [("buttonClicked-button", suspend "addTodo" [])]
--       , Binding "addTodoReqText" $ lam ["req"] $
--             app (var "when") 
--                 [ ("when-events", var "addTodoReq")
--                 , ("when-env", lam ["env"] $ suspend "todoTextEntry" [])
--                 , ("when-get", var "textboxValue")]
--       , Binding "app" $ lam [] $
--             listOf
--                [ var "todoTextEntry"
--                , var "addTodo"
--                , var "addTodoReqText"
--                ]
--       ]
