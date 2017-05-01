module Code where

import Lam
import Data.Functor.Identity
import Data.Text (Text)

v1 :: [Binding Identity]
v1 = [ expBinding "add2" $ lam ["n"] $
           app (var "+") [("+_1", lit (Number 2)), ("+_2", var "n")]
     , expBinding "main" $ lam [] $
           app (var "add2") [("n", lit (Number 5))]
     ]

v2 :: [Binding Identity]
v2 = [ expBinding "add2twice" $ lam ["n"] $
           app (var "+")
               [ ("+_1", lit (Number 2))
               , ("+_2", app (var "+") [("+_1", lit (Number 2)), 
                                        ("+_2", var "n")])
               ]
     , expBinding "main" $ lam [] $
           app (var "add2twice") [("n", lit (Number 5))]
     ]

tracey :: [Binding Identity]
tracey = [ expBinding "gen1" $ lam ["gen1_n"] $ 
               var "gen1_n"
         , expBinding "gen2" $ lam ["gen2_n"] $
               var "gen2_n"
         , expBinding "glue1" $ lam [] $
               listOf [app (var "gen1") [("gen1_n", lit (Number 1))]
                      ,app (var "gen1") [("gen1_n", lit (Number 2))]
                      ,app (var "gen1") [("gen1_n", lit (Number 3))]
                      ]
         , expBinding "glue2a" $ lam [] $
               app (var "suspensionFrameList")
                   [("suspensionFrameList_suspension", suspend $ suspendSpec "gen1" [] [])]
         , expBinding "glue2" $ lam [] $
               app (var "listMap")
                   [("listMap_list", (app (var "glue2a") []))
                   ,("listMap_f", lam ["builtin-listMap-listMap_f-elem"] $
                                      app (var "gen2")
                                          [("gen2_n", app (var "frameArg") [("frameArg_frame", var "builtin-listMap-listMap_f-elem"), ("frameArg_arg", lamArgId "gen1_n")])])
                   ]
         , expBinding "glue3" $ lam [] $
               app (var "suspensionFrameList")
                   [("suspensionFrameList_suspension", suspend $ suspendSpec "gen2" [] [])]
         , expBinding "main" $ lam [] $
               listOf [ var "glue1"
--                      , var "glue2a"
                      , var "glue2"
                      , var "glue3"
                      ]
         ]

nestedMaps :: [Binding Identity]
nestedMaps = [ expBinding "main" $ lam [] $
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

findMax :: [Binding Identity]
findMax = [ expBinding "main" $ lam [] $ 
                app (var "listReduce")
                    [("listReduce_list", listOf $ lit . Number <$> [1,3,5,9,8,7,6,4,2])
                    ,("listReduce_base", lit . Number $ 0)
                    ,("listReduce_merge_curr", lamArgId "findMax_curr")
                    ,("listReduce_merge_next", lamArgId "findMax_next")
                    ,("listReduce_merge", lam ["findMax_curr", "findMax_next"] $
                                              app (app (var "numberCompare")
                                                       [("numberCompare_1", var "findMax_next")
                                                       ,("numberCompare_2", var "findMax_curr")
                                                       ]
                                                  )
                                                  [("compareResult_1_smaller", lam ["unused_1"] $ var "findMax_curr")
                                                  ,("compareResult_1_greater", lam ["unused_2"] $ var "findMax_next")
                                                  ,("compareResult_equal", lam ["unused_3"] $ var "findMax_next")
                                                  ]
                     ) 
                    ]
          ]


oneshot :: [Binding Identity]
oneshot = findMax

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
listAdd newElem list = app (var "listAdd")
                           [ ("listAdd_list", list)
                           , ("listAdd_elem", newElem)
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



listConcat :: Exp -> Exp
listConcat lists = app (var "listConcat")
                       [ ("listConcat_lists", lists)
                       ]

listFilter :: Exp -> Exp -> Exp
listFilter f list = app (var "listFilter")
                        [ ("listFilter_list", list)
                        , ("listFilter_f", f)
                        ]


suspensionFrameList :: Exp -> Exp
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

buttonWeb :: [Binding Identity]
buttonWeb = [ expBinding "incrementButton" $ lam [] $
                app (var "htmlButton")
                    [ ("htmlButton_text", lit (Text "+"))
                    ]
            , expBinding "decrementButton" $ lam [] $
                app (var "htmlButton")
                    [ ("htmlButton_text", lit (Text "-"))
                    ]

            , expBinding "clickCount" $ lam ["clickCount_button"] $
                listSum $ listMap (lam ["builtin-listMap-listMap_f-elem"] $
                                       listLength $ htmlElementEvents $ frameResult (var "builtin-listMap-listMap_f-elem"))
                                  (suspensionFrameList (var "clickCount_button"))
            , expBinding "main" $ lam [] $
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
letBindings namesExps body = app (lam (map fst namesExps) body)
                                 namesExps

-- give a name to the function whose frame is going to bind these names
namedLetBindings :: Name -> [(Name, Exp)] -> Exp -> Exp
namedLetBindings name namesExps inner = letBindings [(name, lam (map fst namesExps) inner)]
                                                    (app (var name) namesExps)


listReduce :: Exp -> Exp -> Text -> Text -> Exp -> Exp
listReduce list base currName nextName lamBody = app (var "listReduce")
                                                     [("listReduce_list", list)
                                                     ,("listReduce_base", base)
                                                     ,("listReduce_merge_curr", lamArgId currName)
                                                     ,("listReduce_merge_next", lamArgId nextName)
                                                     ,("listReduce_merge", lam [currName, nextName] lamBody)
                                                     ]

maybeResult_nothing = app (var "construct") [("construct_with", lamArgId "maybeResult_nothing"), ("construct_payload", lit (Text ""))]

maybeResult_just v = app (var "construct") [("construct_with", lamArgId "maybeResult_just"), ("construct_payload", v)]

web :: [Binding Identity]
web = todoWeb

todoWeb :: [Binding Identity]
todoWeb = [ expBinding "todoRecord" $ record ["todoRecord_id", "todoRecord_text"]
          , expBinding "todoTextBox" $ lam [] $
              app (var "htmlTextInput") []
          , expBinding "todoAddButton" $ lam [] $
              app (var "htmlButton") [("htmlButton_text", lit (Text "Add"))]
          , expBinding "todoForm" $ lam ["todoForm_n"] $
              listOf [ app (var "todoTextBox") []
                     , app (var "todoAddButton") []
                     ]
          , expBinding "savedTodos" $ lam [] $
              listConcat $ listMap (lam ["savedTodoForms_mapFrames"] $
                               listConcat $ listMap (lam ["savedTodoForm_todoTextBoxFrame"] $
                                                         app (app (var "lastEvent")
                                                                  [("lastEvent_events", (htmlElementEvents (frameResult (var "savedTodoForm_todoTextBoxFrame"))))]
                                                             )
                                                             [("maybeResult_nothing", lam ["unused_4"] $ listOf [])
                                                             ,("maybeResult_just", lam ["event"] $
                                                                                 app (var "todoRecord")
                                                                                     [("todoRecord_id", frameArgLit (var "event") "event_instant")
                                                                                     ,("todoRecord_text", app (frameArgLit (var "event") "event_details")
                                                                                                              [("htmlEventDetails_textChange", var "identityFunction")]
                                                                                      )
                                                                                     ]
                                                              )
                                                             ]
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

          , expBinding "savedTodoWidgets" $ lam [] $
              listMap (lam ["savedTodos_mapTodo"] $
                           htmlText $ (frameArgLit (var "savedTodos_mapTodo") "todoRecord_text")
                      )
                      (var "savedTodos")
          , expBinding "unsavedTodoForm" $ lam [] $
              app (var "todoForm") [("todoForm_n", listLength (app (var "savedTodoWidgets") []))]
          , expBinding "lastEvent" $ lam ["lastEvent_events"] $
              listReduce (var "lastEvent_events")
                         maybeResult_nothing
                         "lastEvent_currMaybe" "lastEvent_next" $
                             app (var "lastEvent_currMaybe")
                                 [
                                 ("maybeResult_just", lam ["lastEvent_curr"] $
                                                           maybeResult_just $ app (app (var "instantCompare")
                                                                                   [("instantCompare_1", frameArgLit (var "lastEvent_next") "event_instant")
                                                                                   ,("instantCompare_2", frameArgLit (var "lastEvent_curr") "event_instant")
                                                                                   ]
                                                                              )
                                                                              [("compareResult_1_smaller", lam ["unused_1"] $ var "lastEvent_curr")
                                                                              ,("compareResult_1_greater", lam ["unused_2"] $ var "lastEvent_next")
                                                                              ,("compareResult_equal", lam ["unused_3"] $ var "lastEvent_next")
                                                                              ]
                                  )
                                 ]
          , expBinding "main" $ lam [] $
              listOf [ app (var "unsavedTodoForm") []
                     , app (var "savedTodoWidgets") []
                     , htmlText (numberToText (listLength (app (var "savedTodoWidgets") [])))
                     , listMap (lam ["savedTodoForm_todoTextBoxFrameDbg1"] $
                                   listMap (lam ["eventttt"] (listOf [(app (var "htmlText") [("htmlText_text", (lit $ Text "OHAI"))])
                                                                     ,(app (var "htmlText") [("htmlText_text", (app (frameArgLit (var "eventttt") "event_details")
                                                                                                                    [("htmlEventDetails_textChange", var "identityFunction")]))])
                                                                     ,(app (var "htmlText") [("htmlText_text", (app (var "instantToText") [("instantToText_instant", (frameArgLit (var "eventttt") "event_instant"))]))])
                                                                     ]))
                                           (htmlElementEvents (frameResult (var "savedTodoForm_todoTextBoxFrameDbg1"))))
                               (suspensionFrameList (suspend $ suspendSpec "todoTextBox" [] []))

                     , listMap (lam ["savedTodoForm_todoTextBoxFrameDbg"] $
                                   app (app (var "lastEvent")
                                            [("lastEvent_events", (htmlElementEvents (frameResult (var "savedTodoForm_todoTextBoxFrameDbg"))))]
                                       )
                                       [("maybeResult_nothing", lam ["unused_4Dbg"] $ app (var "htmlText") [("htmlText_text", lit $ Text "NUTTIN")])
                                       ,("maybeResult_just", lam ["eventDbg"] $
                                                                 app (var "htmlText") 
                                                                     [("htmlText_text", app (frameArgLit (var "eventDbg") "event_details")
                                                                                            [("htmlEventDetails_textChange", var "identityFunction")]
                                                               
                                                                      )
                                                                     ]
                                        )
                                       ]
                               )
                               (suspensionFrameList (suspend $ suspendSpec "todoTextBox" [] []))
                     , listMap (lam ["lastEvent2_element"] $
                                    app (var "htmlText") [("htmlText_text", app (listReduce (htmlElementEvents (frameResult (var "lastEvent2_element")))
                                                                                            maybeResult_nothing
                                                                                            "lastEvent2_currMaybe"
                                                                                            "lastEvent2_next" $
                                                                                            maybeResult_just (var "lastEvent2_next"))
                                                                                [("maybeResult_nothing", lam ["unused6"] $ lit $ Text "NONE YET")
                                                                                ,("maybeResult_just", lam ["lastEvent2_found"] $
                                                                                                        app (frameArgLit (var "lastEvent2_found") "event_details") [("htmlEventDetails_textChange", var "identityFunction")])
                                                                                ]
                                                          )]
                               )
                               (suspensionFrameList (suspend $ suspendSpec "todoTextBox" [] []))
                     , listMap (lam ["lastEvent2_dbg_frame"] $
                                    htmlText $ app (listReduce (htmlElementEvents (frameResult (var "lastEvent2_dbg_frame")))
                                                               maybeResult_nothing
                                                               "lastEvent2_dbg_currMaybe"
                                                               "lastEvent2_dbg_next" $
                                                                   app (var "lastEvent2_dbg_currMaybe")
                                                                       [
                                                                       ("maybeResult_just", lam ["lastEvent2_dbg_curr"] $
                                                                                                 maybeResult_just $ app (app (var "instantCompare")
                                                                                                                         [("instantCompare_1", frameArgLit (var "lastEvent2_dbg_next") "event_instant")
                                                                                                                         ,("instantCompare_2", frameArgLit (var "lastEvent2_dbg_curr") "event_instant")
                                                                                                                         ]
                                                                                                                    )
                                                                                                                    [("compareResult_1_smaller", lam ["unused_8"] $ var "lastEvent2_dbg_curr")
                                                                                                                    ,("compareResult_1_greater", lam ["unused_9"] $ var "lastEvent2_dbg_next")
                                                                                                                    ,("compareResult_equal", lam ["unused_a"] $ var "lastEvent2_dbg_next")
                                                                                                                    ]
                                                                        )
                                                                       ]
                                                   )
                                                   [("maybeResult_just", lam ["lastEvent2_dbg_res"] $ app (frameArgLit (var "lastEvent2_dbg_res") "event_details")
                                                                                                          [("htmlEventDetails_textChange", var "identityFunction")])
                                                   ,("maybeResult_nothing", lam ["lastEvent2_dbg_resb"] $ lit $ Text "OOP")
                                                   ] 
                               )
                               (suspensionFrameList (suspend $ suspendSpec "todoTextBox" [] []))
              


                     ]
          , expBinding "clickCount" $ lam ["clickCount_button"] $
             listSum $ listMap (lam ["builtin-listMap-listMap_f-elem"] $
                                    listLength $ htmlElementEvents $ frameResult (var "builtin-listMap-listMap_f-elem"))
                               (suspensionFrameList (var "clickCount_button"))
          , expBinding "identityFunction" $ lam ["identityFunction_arg"] (var "identityFunction_arg")
          ] 

--
--todo :: [Binding Identity]
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
