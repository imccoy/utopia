module Diff (diff, DiffTree(..), Mapping, mappingDst, mappingSrc, mappingCost, mappingChildren) where

import Control.Lens
import Data.List
import Data.Text (Text)
import Data.Maybe
import Safe (fromJustDef)

import Text.EditDistance

import DiffTree

{- 
 - EASY:
 -
 - BigStructure
 -   Text "A"
 -     ->
 - BigStructure
 -   Text "B"
 -
 - The Text node gets matched A->B with a cost of 1, so that's the cost of the overall change
 -
 -------------------------
 -
 - EASY:
 -
 - Text "A"
 -   BigStructure
 -     ->
 - Text "B"
 -   BigStructure
 -
 - Children match perfectly, so then we just care about the Text nodes
 --------------------------
 -
 - HARD:
 -
 - Text "A"
 -   BigStructure
 -     Text "C"
 -     ->
 - Text "B"
 -   BigStructure
 -     Text "D"
 -
 -}

data Mapping = Mapping { _mappingDst :: DstNode
                       , _mappingSrc :: SrcNode
                       , _mappingCost :: Int
                       , _mappingChildren :: [Maybe Mapping]
                       }
makeLenses ''Mapping

instance Show Mapping where
  show = unlines . printMappings' ""
    where printMappings' prefix m = (printMapping prefix m):(concat $ printChildren (prefix ++ "  ") m)
          printMapping prefix (Mapping d s _ _) = prefix ++ show (d ^. dstNodeId) ++ "<-" ++ show (s ^. srcNodeId)
          printChildren prefix (Mapping _ _ _ children) = [maybe [prefix ++ "Nothing"] (printMappings' prefix) child | child <- children]

transitiveClosure :: (a -> [a]) -> a -> [a]
transitiveClosure f v = v:(concat [transitiveClosure f c | c <- f v])

renameCost :: Text -> Maybe Text -> Text -> Maybe Text -> Maybe Int
renameCost srcLabel (Just srcName) dstLabel (Just dstName)
  | srcLabel == dstLabel = Just $ levenshteinDistance defaultEditCosts (show srcName) (show dstName)
  | otherwise            = Nothing
renameCost srcLabel Nothing dstLabel Nothing
  | srcLabel == dstLabel = Just 0
  | otherwise            = Nothing
renameCost _ _ _ _       = Nothing

scoreMapping :: Mapping -> Int
scoreMapping (Mapping dst src renameCost childMappings) = numUnmappedChildren * 10 + numDroppedSrcChildren * 5 + renameCost
  where numUnmappedChildren = length $ filter isNothing childMappings
        numDroppedSrcChildren = let srcChildIds = map (^. srcNodeId) (src ^. srcNodeChildren)
                                    childMappingSrcIds = map (^. srcNodeId) $ map (^. mappingSrc) (catMaybes childMappings)
                                 in length $ srcChildIds \\ childMappingSrcIds

compareMappings a b = compare (scoreMapping a) (scoreMapping b)

findMatchingNode :: DstNode -> [Maybe Mapping] -> SrcNode -> Maybe Mapping
findMatchingNode dst dstChildMappings = findMatchingNode'
  where findMatchingNode' src = case possibleMappings src of
                                  [] -> Nothing
                                  mappings -> Just $ minimumBy compareMappings mappings
        possibleMappings src = catMaybes [ mappingTo srcNode | srcNode <- transitiveClosure (^. srcNodeChildren) src]
        mappingTo src = Mapping dst src <$> renameCost (src ^. diffTree . label) (src ^. diffTree . name) (dst ^. diffTree . label) (dst ^. diffTree . name) <*> pure dstChildMappings

matchTrees :: SrcNode -> DstNode -> Maybe Mapping
matchTrees src = matchTrees'
  where matchTrees' dst = let dstChildMappings = map (matchTrees src) (dst ^. dstNodeChildren)
                           in findMatchingNode dst dstChildMappings src

diff src dst = matchTrees (SrcNode src) (DstNode dst)
