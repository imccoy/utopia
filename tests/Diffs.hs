module Diffs where

import Control.Lens
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import DiffTree hiding (diffTree)
import Diff (Mapping, mappingDst, mappingSrc, mappingCost, mappingChildren)
import qualified Diff
import Lam

mappingChild n = mappingChildren . ix n

mappingAt [] = id
mappingAt (n:ns) = mappingChild n . mappingAt ns

mappingIdsAt :: Mapping -> [Int] -> Maybe (T.Text, Maybe T.Text)
mappingIdsAt mapping ids = do m <- mapping ^? mappingAt ids
                              return ( m ^. mappingDst . dstNodeId . (to $ T.pack . show)

                                     , m ^? mappingSrc . _Just . srcNodeId . (to $ T.pack . show)

                                     )

tests :: TestTree
tests = testGroup "DiffTree" 
  [ testCase "simple ordering change" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Lit" (Just "2") []
                , DiffTree "v1b" "Lit" (Just "4") []
                ]
          v2 =  DiffTree "M" "Module" Nothing $
                [ DiffTree "v2b" "Lit" (Just "4") []
                , DiffTree "v2a" "Lit" (Just "2") []
                ]
          diffResult = Diff.diff v1 v2
       in do (Just ("v2b", Just "v1b")) `compare` mappingIdsAt diffResult [0] @?= EQ
             (Just ("v2a", Just "v1a")) `compare` mappingIdsAt diffResult [1] @?= EQ
  , testCase "simple value swap" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Binding" (Just "Frederick") $
                  [ DiffTree "v1a.value" "Lit" (Just "Aye Aye, cap'n") [] ]
                , DiffTree "v1b" "Binding" (Just "Gregory") $
                  [ DiffTree "v1b.value" "Lit" (Just "Shiver me timbers") [] ]
                ]
          v2 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v2a" "Binding" (Just "Frederick") $
                  [ DiffTree "v2b.value" "Lit" (Just "Shiver me timbers") [] ]
                , DiffTree "v2b" "Binding" (Just "Gregory") $
                  [ DiffTree "v2a.value" "Lit" (Just "Aye Aye, cap'n") [] ]
                ]
          diffResult = Diff.diff v1 v2
       in do (Just ("v2a", Just "v1a")) `compare` mappingIdsAt diffResult [0] @?= EQ
             (Just ("v2b.value", Just "v1b.value")) `compare` mappingIdsAt diffResult [0,0] @?= EQ
             (Just ("v2b", Just "v1b")) `compare` mappingIdsAt diffResult [1] @?= EQ
             (Just ("v2a.value", Just "v1a.value")) `compare` mappingIdsAt diffResult [1,0] @?= EQ
  , testCase "ordering change with value change" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Lit" (Just "mate") []
                , DiffTree "v1b" "Lit" (Just "nine") []
                ]
          v2 =  DiffTree "M" "Module" Nothing $
                [ DiffTree "v2b" "Lit" (Just "none") [] -- should map to nine, aka v1b
                , DiffTree "v2a" "Lit" (Just "mute") [] -- should map to mate, aka v1a
                ]
          diffResult = Diff.diff v1 v2
       in do (Just ("v2b", Just "v1b")) `compare` mappingIdsAt diffResult [0] @?= EQ
             (Just ("v2a", Just "v1a")) `compare` mappingIdsAt diffResult [1] @?= EQ
  , testCase "simple value swap with value change" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Binding" (Just "Frederick") $
                  [ DiffTree "v1a.value" "Lit" (Just "Aye Aye, captain") [] ]
                , DiffTree "v1b" "Binding" (Just "Gregory") $
                  [ DiffTree "v1b.value" "Lit" (Just "Shiver me tumblr") [] ]
                ]
          v2 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v2a" "Binding" (Just "Frederick") $
                  [ DiffTree "v2b.value" "Lit" (Just "Shiver me timbers") [] ]
                , DiffTree "v2b" "Binding" (Just "Gregory") $
                  [ DiffTree "v2a.value" "Lit" (Just "Aye Aye, cap'n") [] ]
                ]
          diffResult = Diff.diff v1 v2
       in do (Just ("v2a", Just "v1a")) `compare` mappingIdsAt diffResult [0] @?= EQ
             (Just ("v2b.value", Just "v1b.value")) `compare` mappingIdsAt diffResult [0,0] @?= EQ
             (Just ("v2b", Just "v1b")) `compare` mappingIdsAt diffResult [1] @?= EQ
             (Just ("v2a.value", Just "v1a.value")) `compare` mappingIdsAt diffResult [1,0] @?= EQ
  , testCase "moving something across the tree" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Binding" (Just "Frederick") $
                  [ DiffTree "v1a.1" "Lit" (Just "Aye Aye, captain") [] ]
                , DiffTree "v1b" "Binding" (Just "George") []
                ]
          v2 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v2a" "Binding" (Just "Frederick") []
                , DiffTree "v2b" "Binding" (Just "George") $
                  [ DiffTree "v2b.1" "Lit" (Just "Aye Aye, captain") [] ]
                ]
          diffResult = Diff.diff v1 v2
       in do (Just ("v2a", Just "v1a")) `compare` mappingIdsAt diffResult [0] @?= EQ
             (Just ("v2b", Just "v1b")) `compare` mappingIdsAt diffResult [1] @?= EQ
             (Just ("v2b.1", Just "v1a.1")) `compare` mappingIdsAt diffResult [1,0] @?= EQ
  , testCase "moving something down the tree" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Binding" (Just "Frederick") $
                  [ DiffTree "v1c" "Lit" (Just "Aye Aye, captain") [] ]
                ]
          v2 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v2a" "Binding" (Just "Frederick") $
                  [ DiffTree "v2b" "Hmmdyhmm" (Just "Says") $
                    [ DiffTree "v2c" "Lit" (Just "Aye Aye, captain") [] ]
                  ]
                ]
          diffResult = Diff.diff v1 v2
       in do (Just ("v2a", Just "v1a")) `compare` mappingIdsAt diffResult [0] @?= EQ
             (Just ("v2b", Nothing)) `compare` mappingIdsAt diffResult [0,0] @?= EQ
             (Just ("v2c", Just "v1c")) `compare` mappingIdsAt diffResult [0,0,0] @?= EQ
  ]
