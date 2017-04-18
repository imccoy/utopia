module Code where

import Lam
import Data.Text (Text)

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

nestedMaps :: [Binding Exp]
nestedMaps = [ Binding "main" $ lam [] $
                 listOf [
                          app (var "+") [("+_1", lit $ Number 1), ("+_2", (app (var "+") [("+_1", lit $ Number 2), ("+_2", lit $ Number 3)]))]
                        , app (var "+") [("+_1", (app (var "+") [("+_1", lit $ Number 2), ("+_2", lit $ Number 3)])), ("+_2", lit $ Number 1)]
                        , listMap (lam ["lists"] $
                                      listMap (lam ["number"] $
                                                   app (var "+") [("+_1", (var "number")), ("+_2", (var "number"))]
                                              )
                                              (var "lists")
                                  )
                                  (listOf [listOf [lit $ Number 1, lit $ Number 2], listOf [lit $ Number 3, lit $ Number 4]])
                        ]
             ]

oneshot = tracey

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

listSum :: Exp -> Exp
listSum list = app (var "listSum")
                   [ ("listSum_list", list)
                   ]

listMap :: Exp -> Exp -> Exp
listMap f list = app (var "listMap")
                     [ ("listMap_list", list)
                     , ("listMap_f", f)
                     ]

listFilter :: Exp -> Exp -> Exp
listFilter f list = app (var "listFilter")
                        [ ("listFilter_list", list)
                        , ("listFilter_f", f)
                        ]



suspensionFrameList suspension = app (var "suspensionFrameList") 
                                     [("suspensionFrameList_suspension", suspension)]

listLength :: Exp -> Exp
listLength l = app (var "listLength")
                   [("listLength_list", l)]

htmlElementEvents :: Exp -> Exp
htmlElementEvents element = app (var "htmlElementEvents")
                                [("htmlElementEvents_element", element)]

htmlTextLit :: Text -> Exp
htmlTextLit = htmlText . lit . Text

htmlText :: Exp -> Exp
htmlText text = app (var "htmlText")
                    [ ("htmlText_text", text) ]

numberToText :: Exp -> Exp
numberToText number = app (var "numberToText")
                          [("numberToText_number", number)]

frameArg :: Exp -> Exp -> Exp
frameArg frame arg = app (var "frameArg")
                         [ ("frameArg_frame", frame)
                         , ("frameArg_arg", arg)]

frameArgLit :: Exp -> Text -> Exp
frameArgLit frame = frameArg frame . lamArgId


frameResult :: Exp -> Exp
frameResult frame = app (var "frameResult")
                        [("frameResult_frame", frame)]

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
                listSum $ listMap (lam ["builtin-listMap-listMap_f-elem"] $
                                       listLength $ htmlElementEvents $ frameResult (var "builtin-listMap-listMap_f-elem"))
                                  (suspensionFrameList (var "clickCount_button"))
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

web = todoWeb


todoWeb :: [Binding Exp]
todoWeb = [ Binding "todoTextBox" $ lam [] $
              app (var "htmlTextInput") []
          , Binding "todoAddButton" $ lam [] $
              app (var "htmlButton") [("htmlButton_text", lit (Text "Add"))]
          , Binding "todoForm" $ lam ["todoForm_n"] $
              listOf [ app (var "todoTextBox") []
                     , app (var "todoAddButton") []
                     ]
          , Binding "savedTodoWidgets" $ lam [] $
              listMap (lam ["savedTodoForms_mapFrames"] $
                          listMap (lam ["savedTodoForm_frame"] $ 
                                      listMap ((lam ["event"] (htmlText (var "event"))) :: Exp)
                                              (htmlElementEvents (frameResult (var "savedTodoForm_frame")))
                                  )
                                  (suspensionFrameList (suspend $ suspendSpec "todoTextBox" 
                                                                              []
                                                                              [suspendSpec "todoForm" [("todoForm_n", frameArgLit (var "savedTodoForms_mapFrames") "todoForm_n")] []]))
                      )
                      (listFilter (lam ["savedTodoForms_filterFrames"] $
                                       app (var "clickCount")
                                           [("clickCount_button", suspend $ suspendSpec "todoAddButton" 
                                                                                        []
                                                                                        [suspendSpec "todoForm" [("todoForm_n", frameArgLit (var "savedTodoForms_filterFrames") "todoForm_n")] []])]
                                  )
                                  (suspensionFrameList (suspend $ suspendSpec "todoForm" [] []))
                      )
          , Binding "unsavedTodoForm" $ lam [] $
              app (var "todoForm") [("todoForm_n", listLength (app (var "savedTodoWidgets") []))]
              --app (var "todoForm") [("todoForm_n", lit $ Number 0)]
          , Binding "main" $ lam [] $
              listOf [ app (var "unsavedTodoForm") []
                     , app (var "savedTodoWidgets") []
                     , htmlText (numberToText (listLength (app (var "savedTodoWidgets") [])))
                     ]
          , Binding "clickCount" $ lam ["clickCount_button"] $
             listSum $ listMap (lam ["builtin-listMap-listMap_f-elem"] $
                                    listLength $ htmlElementEvents $ frameResult (var "builtin-listMap-listMap_f-elem"))
                               (suspensionFrameList (var "clickCount_button"))
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
