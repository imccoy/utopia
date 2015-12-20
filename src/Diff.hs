module Diff (diff, DiffTree(..), LabelAndName(..), Mapping, mappingDst, mappingSrc, mappingCost, mappingChildren) where

import Control.Lens
import Data.List
import Data.Text (Text)
import Data.Maybe
import Safe (fromJustDef)

import Text.EditDistance

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

class DiffTree a where
  getId :: a -> Text
  getChildren :: a -> [a]
  getLabelAndName :: a -> LabelAndName

newtype SrcNode a = SrcNode a
  deriving (Eq, Show)
newtype DstNode a = DstNode a
  deriving (Eq, Show)

newtype SrcNodeId = SrcNodeId Text
  deriving (Eq, Show)
newtype DstNodeId = DstNodeId Text
  deriving (Eq, Show)

srcNodeId (SrcNode a) = SrcNodeId $ getId a
dstNodeId (DstNode a) = DstNodeId $ getId a

instance (DiffTree a) => DiffTree (SrcNode a) where
  getId (SrcNode a) = getId a
  getChildren (SrcNode a) = map SrcNode $ getChildren a
  getLabelAndName (SrcNode a) = getLabelAndName a

instance (DiffTree a) => DiffTree (DstNode a) where
  getId (DstNode a) = getId a
  getChildren (DstNode a) = map DstNode $ getChildren a
  getLabelAndName (DstNode a) = getLabelAndName a

data LabelAndName = LabelAndName Text Text | LabelOnly Text
  deriving (Show)

data Mapping s d = Mapping { _mappingDst :: DstNode d
                           , _mappingSrc :: SrcNode s
                           , _mappingCost :: Int
                           , _mappingChildren :: [Maybe (Mapping s d)]
                           }
makeLenses ''Mapping

instance (DiffTree s, DiffTree d) => Show (Mapping s d) where
  show = unlines . printMappings' ""
    where printMappings' prefix m = (printMapping prefix m):(concat $ printChildren (prefix ++ "  ") m)
          printMapping prefix (Mapping d s _ _) = prefix ++ show (getId d) ++ "<-" ++ show (getId s)
          printChildren prefix (Mapping _ _ _ children) = [maybe [prefix ++ "Nothing"] (printMappings' prefix) child | child <- children]

withAllDescendants :: DiffTree a => a -> [a]
withAllDescendants t = t:(concat [withAllDescendants c | c <- getChildren t])

renameCost :: LabelAndName -> LabelAndName -> Maybe Int
renameCost (LabelAndName srcLabel srcName) (LabelAndName dstLabel dstName)
  | srcLabel == dstLabel = Just $ levenshteinDistance defaultEditCosts (show srcName) (show dstName)
  | otherwise            = Nothing
renameCost (LabelOnly srcLabel) (LabelOnly dstLabel)
  | srcLabel == dstLabel = Just 0
  | otherwise            = Nothing
renameCost _ _           = Nothing

scoreMapping (Mapping dst src renameCost childMappings) = numUnmappedChildren * 10 + numDroppedSrcChildren * 5 + renameCost
  where numUnmappedChildren = length $ filter isNothing childMappings
        numDroppedSrcChildren = let srcChildIds = map srcNodeId $ getChildren src
                                    childMappingSrcIds = map (srcNodeId . view mappingSrc) $ catMaybes childMappings
                                 in length $ srcChildIds \\ childMappingSrcIds

compareMappings a b = compare (scoreMapping a) (scoreMapping b)

findMatchingNode :: (DiffTree s, DiffTree d) => DstNode d -> [Maybe (Mapping s d)] -> SrcNode s -> Maybe (Mapping s d)
findMatchingNode dst dstChildMappings = findMatchingNode'
  where findMatchingNode' src = case possibleMappings src of
                                  [] -> Nothing
                                  mappings -> Just $ minimumBy compareMappings mappings
        possibleMappings src = catMaybes [ mappingTo srcNode | srcNode <- withAllDescendants src]
        mappingTo src = addChildMappings src <$> renameCost (getLabelAndName src) (getLabelAndName dst)
        addChildMappings src cost = Mapping dst src cost dstChildMappings

matchTrees :: (DiffTree s, DiffTree d) => SrcNode s -> DstNode d -> Maybe (Mapping s d)
matchTrees src = matchTrees'
  where matchTrees' dst = let dstChildMappings = map (matchTrees src) (getChildren dst)
                           in findMatchingNode dst dstChildMappings src

diff src dst = matchTrees (SrcNode src) (DstNode dst)
