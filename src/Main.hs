module Main where

import Debug.Trace

import Prelude hiding (putStrLn)

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Lens ((^.))
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Aeson.Encode
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import Data.Text.Lazy.Builder
import qualified Data.UUID as UUID
import qualified Data.UUID.V1
import qualified Data.UUID.V4
import Network.HTTP.Client (HttpException (NoResponseDataReceived))
import Network.Wreq
import Safe

import qualified Lam
import qualified Code

data CodeTree = CodeTree (Maybe Text) Text [CodeTree]

class ToCodeTree a where
  toCodeTree :: a -> CodeTree

instance ToCodeTree Lam.Module where
  toCodeTree (Lam.Module bindings) = CodeTree Nothing "Module" $ map toCodeTree bindings

instance ToCodeTree Lam.Binding where
  toCodeTree (Lam.Binding name exp) = CodeTree (Just name) "Binding" [toCodeTree exp]

instance ToCodeTree Lam.Exp where
  toCodeTree (Lam.Lit literal) = toCodeTree literal
  toCodeTree (Lam.Var name) = CodeTree (Just name) "Var" []
  toCodeTree (Lam.Lam args exp) = CodeTree Nothing "Lam" $ (map arg args) ++ [CodeTree Nothing "LamBody" [toCodeTree exp]]
    where arg name = CodeTree (Just name) "LamArg" []
  toCodeTree (Lam.App body args) = CodeTree Nothing "App" $ (CodeTree Nothing "AppBody" [toCodeTree body]):(map arg args)
    where arg exp = CodeTree Nothing "AppArg" [toCodeTree exp]

instance ToCodeTree Lam.Literal where
  toCodeTree (Lam.Number n) = CodeTree (Just $ T.pack $ show n) "LiteralNumber" []
  toCodeTree (Lam.Text t) = CodeTree (Just t) "LiteralText" []

newtype CodeDbId = CodeDbId Text
  deriving (Ord, Eq, Show)
codeDbIdText (CodeDbId id) = id

newtype CodeDbType = CodeDbType Text
  deriving (Ord, Eq, Show)
codeDbTypeText (CodeDbType ty) = ty

data CodeDbTreeEntry = CodeDbTreeEntry CodeDbType [CodeDbId]

data ProjectionCode = ProjectionCode CodeDbId CodeDbType [ProjectionCode]
data Projection = Projection { projectionCode :: ProjectionCode, projectionNames :: Map CodeDbId Text }

type CodeDbIdGen a = ReaderT Text IO a

newtype SrcMapId = SrcMapId Integer
  deriving (Show,Ord,Eq)
newtype DstMapId = DstMapId Integer
  deriving (Show,Ord,Eq)
data SrcMapping = Delete | SrcChange CodeDbId | SrcMove CodeDbId
  deriving (Show)
data DstMapping = Add | DstChange CodeDbId | DstMove CodeDbId
  deriving (Show)

runCodeDbIdGen f = do
  baseUUID <- getBaseUUID
  runReaderT f (UUID.toText baseUUID)

getBaseUUID = do
  baseUUID <- Data.UUID.V1.nextUUID
  case baseUUID of
    Just u -> return u
    Nothing -> do
      Data.UUID.V4.nextRandom

nextCodeDbId :: CodeDbIdGen CodeDbId
nextCodeDbId = do
  baseUUID <- ask
  tailUUID <- liftIO Data.UUID.V4.nextRandom
  return $ CodeDbId $ baseUUID `T.append` "-" `T.append` UUID.toText tailUUID

main = do
  v1 <- runCodeDbIdGen $ ingest $ toCodeTree Code.v1
  printProjection v1
  v2 <- runCodeDbIdGen $ ingest $ toCodeTree Code.v2
  printProjection v2
  diffResult <- diffProjections v1 v2
  case diffResult of
    (Left e) -> putStrLn . T.pack $ "Couldn't parse diff: " ++ e
    (Right (srcDiff, dstDiff)) -> do
      putStrLn $ T.pack $ unlines $ map show $ Map.assocs srcDiff
      putStrLn $ T.pack $ unlines $ map show $ Map.assocs dstDiff

diffProjections src dst = do
  let projectionIds = projectionIntKeys [src, dst]
  let q = encode $ object [
             "src" .= projectionJson projectionIds src,
             "dst" .= projectionJson projectionIds dst
          ]
  response <- ntries 5 $ post "http://localhost:4754/" q
  let mappingsJson = eitherDecode (response ^. responseBody) :: Either String Array
  let mappings = parseMappings projectionIds =<< mappingsJson
  return mappings

ntries n action = action `E.catch` (handler n)
  where handler 0 e = E.throwIO e
        handler n NoResponseDataReceived = do putStrLn "NoResponseDataReceived, normally just a gumtree hissyfit, retrying"
                                              threadDelay 2000000 -- 2s in microseconds
                                              ntries (n - 1) action
        handler _ e = E.throwIO e

parseMappings :: Bimap CodeDbId Integer -> Array -> Either String (Map CodeDbId SrcMapping, Map CodeDbId DstMapping)
parseMappings projectionIdInts  = foldM parseMapping (Map.empty, Map.empty)
  where parseMapping (srcMappings, dstMappings) mapping = do
          flip parseEither mapping $ withObject "mapping was not an object" $ \obj -> do
            ty <- obj .: "ty"
            let getSrc = obj .: "src" >>= (`Bimap.lookupR` projectionIdInts)
            let getDst = obj .: "dst" >>= (`Bimap.lookupR` projectionIdInts)
            case (ty :: Text) of
              "change" -> do
                src <- getSrc
                dst <- getDst
                return (Map.insert src (SrcChange dst) srcMappings
                       ,Map.insert dst (DstChange src) dstMappings)
              "move" -> do
                src <- getSrc
                dst <- getDst
                return (Map.insert src (SrcMove dst) srcMappings
                       ,Map.insert dst (DstMove src) dstMappings)
              "add" -> do
                dst <- getDst
                return (srcMappings
                       ,Map.insert dst Add dstMappings)
              "delete" -> do
                src <- getSrc
                return (Map.insert src Delete srcMappings
                       ,dstMappings)
  
  

projectionIntKeys :: [Projection] -> Bimap CodeDbId Integer
projectionIntKeys projections = let allKeys = Set.unions $ map (projectionCodeDbIds . projectionCode) projections
                                    assocs = zip (Set.toList allKeys) [0..] 
                                 in Bimap.fromList assocs

projectionCodeDbIds :: ProjectionCode -> Set CodeDbId
projectionCodeDbIds (ProjectionCode id _ children) = id `Set.insert` (Set.unions $ map projectionCodeDbIds children)

projectionJson projectionIdInts Projection{..} = projectionJson' projectionCode
  where
    projectionJson' (ProjectionCode id ty children) = object [
                                                        "id" .= fromJustNote ("projectionJson' " ++ show id) (Bimap.lookup id projectionIdInts),
                                                        "label" .= fromJustDef "" (Map.lookup id projectionNames),
                                                        "typeLabel" .= codeDbTypeText ty,
                                                        "children" .= map projectionJson' children
                                                      ]

ingest :: CodeTree -> CodeDbIdGen Projection
ingest (CodeTree name ty children) = do
  id <- nextCodeDbId
  childProjections <- mapM ingest children
  let childrenProjectionCode = map projectionCode childProjections
  let projectionCode = ProjectionCode id (CodeDbType ty) childrenProjectionCode

  let childNameMaps = map projectionNames childProjections
  let childNamesMap = Map.unions childNameMaps
  let projectionNames = maybe childNamesMap (\name -> Map.insert id name childNamesMap) name
  return $ Projection { projectionCode, projectionNames }

printProjection :: Projection -> IO ()
printProjection Projection{..} = printProjection' " " projectionCode where
  printProjection' prefix (ProjectionCode id ty children) = do
    let name = Map.lookup id projectionNames
    putStrLn $ codeDbIdText id `T.append` prefix `T.append` codeDbTypeText ty `T.append` " " `T.append` fromJustDef "" name
    forM_ children (printProjection' (prefix `T.append` "  "))

