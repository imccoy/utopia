module DiffTree (diffTree) where

import qualified Data.Text as T
import Lam
import Diff

data ConcreteDiffTree = ConcreteDiffTree T.Text LabelAndName [ConcreteDiffTree]
  deriving Show

instance DiffTree ConcreteDiffTree where
  getId (ConcreteDiffTree id _ _) = id
  getChildren (ConcreteDiffTree _ _ children) = children
  getLabelAndName (ConcreteDiffTree _ labelAndName _) = labelAndName

diffTree (Module bindings)
  = ConcreteDiffTree "M"
                     (LabelOnly "Module")
                     [bindingDiffTree (T.pack $ show n) binding | (n, binding) <- zip [0..] bindings]

bindingDiffTree idPrefix (Binding name exp)
  = ConcreteDiffTree (idPrefix `T.append` ".name") 
                     (LabelAndName "Binding" name)
                     [expDiffTree idPrefix exp]

expDiffTree idPrefix = go
  where go (Lam args body) = ConcreteDiffTree (idPrefix `T.append` ".lam")
                                              (LabelOnly "Lam")
                                              [lamArgsDiffTree (idPrefix `T.append` ".lam") args
                                              ,lamBodyDiffTree (idPrefix `T.append` ".lam") body
                                              ]
        go (Var name) = ConcreteDiffTree (idPrefix `T.append` ".var")
                                         (LabelAndName "Var" name)
                                         []
        go (App f args) = ConcreteDiffTree (idPrefix `T.append` ".app")
                                           (LabelOnly "App")
                                           [appFunDiffTree (idPrefix `T.append` ".app") f
                                           ,appArgsDiffTree (idPrefix `T.append` ".app") args
                                           ]
        go (Lit ((Number n))) = ConcreteDiffTree (idPrefix `T.append` ".lit")
                                                 (LabelAndName "LitNumber" (T.pack $ show n))
                                                 []
        go (Lit ((Text t))) = ConcreteDiffTree (idPrefix `T.append` ".lit")
                                               (LabelAndName "LitText" t)
                                               []

lamArgsDiffTree idPrefix args = ConcreteDiffTree (idPrefix `T.append` ".args")
                                            (LabelOnly "LamArgs")
                                            [ConcreteDiffTree (idPrefix `T.append` ".args[" `T.append` T.pack (show n) `T.append` "]")
                                                              (LabelAndName "LamArg" arg)
                                                              []
                                            |(n,arg) <- zip [0..] args
                                            ]

lamBodyDiffTree idPrefix body = ConcreteDiffTree (idPrefix `T.append` ".body")
                                                 (LabelOnly "LamBody")
                                                 [expDiffTree (idPrefix `T.append` ".body") body]

appFunDiffTree idPrefix fun = ConcreteDiffTree (idPrefix `T.append` ".fun")
                                               (LabelOnly "AppFun")
                                               [expDiffTree (idPrefix `T.append` ".fun") fun]

appArgsDiffTree idPrefix args = ConcreteDiffTree (idPrefix `T.append` ".args")
                                                 (LabelOnly "AppArgs")
                                                 [ConcreteDiffTree (idPrefix `T.append` ".args[" `T.append` T.pack (show n) `T.append` "]")
                                                                   (LabelOnly "AppArg")
                                                                   []
                                                 |(n,arg) <- zip [0..] args
                                                 ]
