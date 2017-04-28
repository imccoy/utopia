module Html where

import Prelude hiding (head, div)

import Control.Lens hiding (children, mapping)
import Control.Monad (forM_)
import qualified Data.Text as T
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import Diff (Mapping, mappingCost, mappingDst, mappingSrc, mappingChildren, ReverseMapping, reverseMappingSrc, reverseMappingDsts, reverseMappingChildren)
import DiffTree hiding (name, label)
import qualified DiffTree

mappingHtml :: ReverseMapping -> Mapping -> Html
mappingHtml reverseMapping mapping = do
  docType
  head $ do
    link ! rel "stylesheet" ! href "styles.css"
    script ! src "script.js" $ text ""
  body $ do
    table $
      tr $ do
        td ! width "50%" $ srcMapping reverseMapping
        td ! width "50%" $ dstMapping mapping

srcMapping :: ReverseMapping -> Html
srcMapping reverseMapping =
  let nodeLabel = reverseMapping ^. reverseMappingSrc . diffTree . DiffTree.label
      diffTreeName = reverseMapping ^. reverseMappingSrc . diffTree . DiffTree.name . non ""
   in div ! class_ "src-node" $ do
     if nodeLabel == "METADATA-REF"
        then a ! href (textValue $ "#" `T.append` diffTreeName) $ do (text ">")
        else 
          div ! class_ "src-node__label" $ do
            a ! name (stringValue $ show $ reverseMapping ^. reverseMappingSrc . srcNodeId) $ do
              text nodeLabel
              text " "
              text diffTreeName
            forM_ (reverseMapping ^. reverseMappingDsts) $ \(dst, cost) -> do
              text " "
              a ! href (stringValue $ "#" ++ (show $ dst ^. dstNodeId)) $ do
                text "*"
                sup $ text $ T.pack $ show cost
            div ! class_ "src-node__children" $
              forM_ (reverseMapping ^. reverseMappingChildren) srcMapping

dstMapping :: Mapping -> Html
dstMapping mapping = div ! class_ "dst-node" $
  if mapping ^. mappingDst . diffTree . DiffTree.label == "METADATA-REF"
    then a ! href (textValue $ "#" `T.append` (mapping ^. mappingDst . diffTree . DiffTree.name . non "")) $ do (text ">")
    else do
      div ! class_ "dst-node__label" $ do
        a ! name (stringValue $ show $ mapping ^. mappingDst . dstNodeId) ! href (dstMappingLink mapping) $ do
          text $ mapping ^. mappingDst . diffTree . DiffTree.label
          text " "
          text $ mapping ^. mappingDst . diffTree . DiffTree.name . non ""
          sup $ text $ T.pack $ show $ mapping ^. mappingCost
      div ! class_ "dst-node_children" $
        forM_ (mapping ^. mappingChildren) dstMapping

dstMappingLink :: Mapping -> AttributeValue
dstMappingLink mapping = case mapping ^. mappingSrc of
  Just srcNode -> stringValue $ "#" ++ (show $ srcNode ^. srcNodeId)
  Nothing -> mempty
