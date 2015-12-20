module Diffs where

import Control.Lens
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import DiffTree
import Diff (Mapping, mappingDst, mappingSrc, mappingCost, mappingChildren)
import qualified Diff
import Lam

mappingChild n = _Just . mappingChildren . ix n

srcNodeId = _Just . mappingSrc . to Diff.getId

tests :: TestTree
tests = testGroup "DiffTree" 
  [ testCase "simple ordering change" $
      let v1 = Module [ Binding "n" $ Lit $ Number 4
                      , Binding "m" $ Lit $ Number 2
                      ]
          v2 = Module [ Binding "m" $ Lit $ Number 2
                      , Binding "n" $ Lit $ Number 4
                      ]
          diffResult = Diff.diff (diffTree v1) (diffTree v2)
          bindingNameAndValue bindingMapping = (diffResult ^? bindingMapping . srcNodeId, diffResult ^? bindingMapping . mappingChild 0 . srcNodeId)
       in do (Just "1.name", Just "1.lit") `compare` bindingNameAndValue (mappingChild 0) @?= EQ
             (Just "0.name", Just "0.lit") `compare` bindingNameAndValue (mappingChild 1) @?= EQ
  , testCase "simple value swap" $
      let v1 = Module [ Binding "Frederick" $ Lit $ Text "Aye aye, cap'n"
                      , Binding "Gregory" $ Lit $ Text "Shiver me timbers"
                      ]
          v2 = Module [ Binding "Frederick" $ Lit $ Text "Shiver me timbers"
                      , Binding "Gregory" $ Lit $ Text "Aye aye, cap'n"
                      ]
          diffResult = Diff.diff (diffTree v1) (diffTree v2)
          bindingNameAndValue bindingMapping = (diffResult ^? bindingMapping . srcNodeId, diffResult ^? bindingMapping . mappingChild 0 . srcNodeId)
       in do (Just "0.name", Just "1.lit") `compare` bindingNameAndValue (mappingChild 0) @?= EQ
             (Just "1.name", Just "0.lit") `compare` bindingNameAndValue (mappingChild 1) @?= EQ
  , testCase "ordering change with value change" $
      let v1 = Module [ Binding "nine" $ Lit $ Number 4
                      , Binding "mate" $ Lit $ Number 2
                      ]
          v2 = Module [ Binding "mute" $ Lit $ Number 2
                      , Binding "none" $ Lit $ Number 4
                      ]
          diffResult = Diff.diff (diffTree v1) (diffTree v2)
          bindingNameAndValue bindingMapping = (diffResult ^? bindingMapping . srcNodeId, diffResult ^? bindingMapping . mappingChild 0 . srcNodeId)
       in do (Just "1.name", Just "1.lit") `compare` bindingNameAndValue (mappingChild 0) @?= EQ
             (Just "0.name", Just "0.lit") `compare` bindingNameAndValue (mappingChild 1) @?= EQ
  , testCase "simple value swap with value change" $
      let v1 = Module [ Binding "Frederick" $ Lit $ Text "Aye aye, cap'n"
                      , Binding "Gregory" $ Lit $ Text "Shiver me tumblr"
                      ]
          v2 = Module [ Binding "Frederick" $ Lit $ Text "Shiver me timbers"
                      , Binding "Gregory" $ Lit $ Text "Aye aye, captain"
                      ]
          diffResult = Diff.diff (diffTree v1) (diffTree v2)
          bindingNameAndValue bindingMapping = (diffResult ^? bindingMapping . srcNodeId, diffResult ^? bindingMapping . mappingChild 0 . srcNodeId)
       in do (Just "0.name", Just "1.lit") `compare` bindingNameAndValue (mappingChild 0) @?= EQ
             (Just "1.name", Just "0.lit") `compare` bindingNameAndValue (mappingChild 1) @?= EQ

  ]
