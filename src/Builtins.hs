module Builtins (functionIds, allArgIds, EvalError(..), Builtin, env) where

import Debug.Trace

import Control.Lens
import Control.Monad ((>=>))
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import Data.Either (partitionEithers)
import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T

import Eval (Env, Val(..), EvalError(..), Thunk(..), Trail(..), Trailing, noTrail)
import CodeDb (CodeDbId (..), codeDbIdText)
import qualified CodeDbIdGen
import qualified Lam
import MonoidMap (unMonoidMap)
import qualified Prim

type Name = T.Text
type ArgName = T.Text
type Id = T.Text

data Builtin = Builtin { _name :: Name
                       , _argNames :: [ArgName]
                       , _body :: BuiltinBody
                       }

type BuiltinEnv = Env IO IORef CodeDbId
type BuiltinVal = Val IO IORef CodeDbId
type BuiltinThunk = Thunk IO IORef CodeDbId
type BuiltinTrailing = Trailing IO IORef CodeDbId

data BuiltinBody = FnBody (CodeDbId -> BuiltinEnv -> Either [EvalError CodeDbId] BuiltinVal)
                 | ResolvedFnBody (Integer -> CodeDbId -> BuiltinEnv -> Changeable IO IORef (Lam.Resolved CodeDbId) -> Changeable IO IORef (Either [EvalError CodeDbId] (BuiltinTrailing BuiltinVal)))
                 | TrailFnBody (CodeDbId -> Trail IO IORef CodeDbId -> BuiltinEnv -> Either [EvalError CodeDbId] BuiltinVal)
                 | EvalFnBody ( CodeDbId
                              -> BuiltinEnv
                              -> (BuiltinVal -> 
                                  Changeable IO IORef (Either [EvalError CodeDbId] (BuiltinTrailing BuiltinVal)))
                              -> Changeable IO IORef (Either [EvalError CodeDbId] (BuiltinTrailing BuiltinVal)))

makeLenses ''Builtin

get :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] BuiltinVal
get i argId env = case Map.lookup (CodeDbId argId) env of
                    Just v -> Right v
                    Nothing -> Left [UndefinedVar i argId]

getList :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] [BuiltinVal]
getList i argId env = get i argId env >>= asList i

asList i v = case v of
  (ValList list) -> pure list 
  _ -> Left [TypeError i $ T.pack $ show v ++ " is not a list"]

getSuspension :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] (CodeDbId, Map CodeDbId BuiltinVal)
getSuspension i argId env = get i argId env >>= \v ->
  case v of
    (Suspension resolvedId env) -> Right (resolvedId, env)
    _ -> Left [TypeError i $ T.pack $ show v ++ " is not a suspension"]

getPrim :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Prim.Prim
getPrim i argId env = get i argId env >>= asPrim i

getFrame :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] (CodeDbId, Map CodeDbId BuiltinVal, BuiltinVal)
getFrame i argId env = get i argId env >>= \v ->
  case v of
    ValFrame i env args -> Right (i, env, args)
    _ -> Left [TypeError i $ (T.pack $ show v) `T.append` " is not a frame"]

asPrim i v = case v of
  Primitive p -> Right p
  _ -> Left [TypeError i $ (T.pack $ show v) `T.append` " is not a prim"]

asNumber i v = case v of
                 (Prim.Number num) -> pure num
                 p -> Left [TypeError i $ T.pack $ show p ++ " is not a number"]

getNumber :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Integer
getNumber i argId env = getPrim i argId env >>= asNumber i

getText :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] T.Text
getText i argId env = getPrim i argId env >>= \v ->
  case v of
    (Prim.Text text) -> pure text
    p -> Left [TypeError i $ T.pack $ show p ++ " is not text"]




type Thunkable = (Set CodeDbId, BuiltinEnv, BuiltinThunk)
getThunkable :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Thunkable
getThunkable i argId env = get i argId env >>= \v ->
  case v of
    Thunk needed env body -> Right (needed, env, body)
    _ -> Left [TypeError i $ T.pack $ show v ++ " is not a thunk"]

thunkableApp :: Thunkable -> CodeDbId -> BuiltinVal -> Thunkable
thunkableApp (needed, env, body) argName argVal = ( Set.delete argName needed
                                                  , Map.insert argName argVal env
                                                  , body
                                                  )

thunkableVal :: Thunkable -> BuiltinVal
thunkableVal (needed, env, body) = Thunk needed env body


numberToText :: Builtin
numberToText = Builtin "numberToText" ["numberToText_number"] . FnBody $ \i e -> do
  n <- getNumber i "builtin-numberToText-numberToText_number" e
  pure $ Primitive $ Prim.Text $ T.pack $ show n


-- this could be something like:
--plus = builtin "+" $ do
--         arg1 <- arg "1"
--         arg2 <- arg "2"
--         pure $ 
--           a <- askInt arg1
--           b <- askInt arg2
--           pure $ a + b
plus :: Builtin
plus = Builtin "+" ["+_1", "+_2"] . FnBody $ \i e -> do
  n <- getNumber i "builtin-+-+_1" e
  m <- getNumber i "builtin-+-+_2" e
  pure $ Primitive $ Prim.Number $ n + m

minus :: Builtin
minus = Builtin "-" ["-_1", "-_2"] . FnBody $ \i e -> do
  n <- getNumber i "builtin----_1" e
  m <- getNumber i "builtin----_2" e
  pure $ Primitive $ Prim.Number $ n - m



listConcat :: Builtin
listConcat = Builtin "listConcat" ["listConcat_1", "listConcat_2"] . FnBody $ \i e -> do
  n <- getList i "builtin-listConcat-listConcat_1" e
  m <- getList i "builtin-listConcat-listConcat_2" e
  pure $ ValList $ n ++ m

listAdd :: Builtin
listAdd = Builtin "listAdd" ["listAdd_elem", "listAdd_list"] . FnBody $ \i e -> do
  n <- get i "builtin-listAdd-listAdd_elem" e
  m <- getList i "builtin-listAdd-listAdd_list" e
  pure $ ValList $ n:m

listMap :: Builtin
listMap = Builtin "listMap" ["listMap_f", "listMap_list"] . EvalFnBody $ \i e eval -> do
  let thunks = do (thunkNeeded, thunkEnv, thunkBody) <- getThunkable i "builtin-listMap-listMap_f" e
                  list <- getList i "builtin-listMap-listMap_list" e
                  case Set.toList thunkNeeded of
                    [argId] -> pure [ Thunk Set.empty (Map.insert argId elem thunkEnv) thunkBody
                                    | elem <- list]
                    [] -> Left [TypeError i "No arg for listMap_f"]
                    _ -> Left [TypeError i "Too many args for listMap_f"]
  case thunks of
    Left e -> pure $ Left e
    Right thunkList -> do eitherTrails <- mapM eval thunkList
                          let (errors, successes) = partitionEithers eitherTrails
                          case errors of
                            [] -> pure $ Right $ ValList <$> sequenceA successes
                            es -> pure $ Left $ concat es


listEmpty :: Builtin
listEmpty = Builtin "listEmpty" [] . FnBody $ \i e -> do
  pure $ ValList []

listLength :: Builtin
listLength = Builtin "listLength" ["listLength_list"] . FnBody $ \i e -> do
  list <- getList i "builtin-listLength-listLength_list" e
  pure $ Primitive $ Prim.Number $ fromIntegral $ length list

listSum :: Builtin
listSum = Builtin "listSum" ["listSum_list"] . FnBody $ \i e -> do
  list <- getList i "builtin-listSum-listSum_list" e
  case partitionEithers ((asPrim i >=> asNumber i) <$> list) of
    ([], ints) -> pure $ Primitive $ Prim.Number $ fromIntegral $ sum ints
    (errs, _) -> Left $ concat errs


frameArg :: Builtin
frameArg = Builtin "frameArg" ["frameArg_frame", "frameArg_arg"] . ResolvedFnBody $ \_ i e ch_resolved -> do
  resolved <- ch_resolved
  pure $ do
    (frameId, frameEnv, _) <- getFrame i "builtin-frameArg-frameArg_frame" e
    (argId, _) <- getSuspension i "builtin-frameArg-frameArg_arg" e
    maybe (Left [UndefinedVar i (codeDbIdText argId)]) (Right . noTrail) $ Map.lookup argId frameEnv

frameResult :: Builtin
frameResult = Builtin "frameResult" ["frameResult_frame"] . ResolvedFnBody $ \_ i e ch_resolved -> do
  resolved <- ch_resolved
  pure $ do
    (_, _, result) <- getFrame i "builtin-frameResult-frameResult_frame" e
    pure $ noTrail $ result


htmlElementEvents :: Builtin
htmlElementEvents = Builtin "htmlElementEvents" ["htmlElementEvents_element"] . FnBody $ \i e -> do
  element <- getList i "builtin-htmlElementEvents-htmlElementEvents_element" e
  case element of
    [Primitive (Prim.Text "button"), _, Primitive (Prim.Text token)] -> trace ("LOOKUP OF events-" ++ T.unpack token) $
                                                                        pure $ case Map.lookup (CodeDbId $ "events-" `T.append` token) e of
                                                                                 Just v -> v
                                                                                 Nothing -> ValList []
    _ -> Left [TypeError i $ T.pack $ show element ++ " is not an element with events"]


htmlText :: Builtin
htmlText = Builtin "htmlText" ["htmlText_text"] . FnBody $ \i e -> do
  text <- getText i "builtin-htmlText-htmlText_text" e
  pure $ ValList [Primitive $ Prim.Text "text", Primitive $ Prim.Text text]

htmlButton :: Builtin
htmlButton = Builtin "htmlButton" ["htmlButton_text"] . ResolvedFnBody $ \magic i e ch_resolved -> do
  let buttonToken = T.pack . show $ magic
  pure $ do
    text <- getText i "builtin-htmlButton-htmlButton_text" e
    pure $ noTrail $ ValList [Primitive $ Prim.Text "button", Primitive $ Prim.Text text, Primitive $ Prim.Text buttonToken]


suspensionFrameList :: Builtin
suspensionFrameList = Builtin "suspensionFrameList" ["suspensionFrameList_suspension"] . TrailFnBody $ \i trail e -> do
  (suspensionResolvedId, suspensionEnv) <- getSuspension i "builtin-suspensionFrameList-suspensionFrameList_suspension" e
  let trailElements = Map.assocs $ fromMaybe Map.empty (Map.lookup suspensionResolvedId . unMonoidMap $ trail)
  
  pure $ ValList [ ValFrame suspensionResolvedId trailElementEnv trailElementVal
                 | (trailElementEnv, trailElementVal) <- trailElements
                 , Map.isSubmapOf suspensionEnv trailElementEnv ]

builtins :: [Builtin]
builtins = [ plus, minus
           , listConcat, listAdd, listEmpty, listMap, listLength, listSum
           , numberToText
           , suspensionFrameList, frameArg, frameResult
           , htmlText, htmlButton, htmlElementEvents]

builtinId :: Builtin -> Id
builtinId builtin = "builtin-" `T.append` (builtin ^. name)


functionIds :: Map Name CodeDbId
functionIds = Map.fromList [(builtin ^. name, CodeDbId $ builtinId builtin) | builtin <- builtins]


builtinArgsIds :: Builtin -> [(Name, CodeDbId)]
builtinArgsIds builtin = [(argName, CodeDbId $ argId builtin argName)| argName <- builtin ^. argNames]
  where argId :: Builtin -> ArgName -> Id
        argId builtin argName = builtinId builtin `T.append` "-" `T.append` argName

builtinArgsIdsSet :: Builtin -> Set CodeDbId
builtinArgsIdsSet = Set.fromList . map snd . builtinArgsIds

allArgIds :: Map Name CodeDbId
allArgIds = Map.fromList $ foldMap builtinArgsIds builtins

env :: Env IO IORef CodeDbId
env = Map.fromList 
        [ (CodeDbId $ builtinId builtin
          , Thunk (builtinArgsIdsSet builtin) Map.empty $ case builtin ^. body of
                                                            FnBody fn -> ThunkFn (CodeDbId $ builtinId builtin) $ \env -> do fn (CodeDbId $ builtinId builtin) env
                                                            ResolvedFnBody fn -> ThunkResolvedFn (CodeDbId $ builtinId builtin) $ \magic env resolved -> do fn magic (CodeDbId $ builtinId builtin) env resolved
                                                            EvalFnBody fn -> ThunkEvalFn (CodeDbId $ builtinId builtin) $ \env eval -> fn (CodeDbId $ builtinId builtin) env eval
                                                            TrailFnBody fn -> ThunkTrailFn (CodeDbId $ builtinId builtin) $ \trail env -> do fn (CodeDbId $ builtinId builtin) trail env
          )
        | builtin <- builtins]
