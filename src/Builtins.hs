module Builtins (functionIds, allArgIds, EvalError(..), Builtin, env) where

import Control.Lens
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
import qualified Lam
import MonoidMap (unMonoidMap)
import qualified Prim

type Name = T.Text
type ArgName = T.Text
type Id = T.Text

data Builtin m r = Builtin { _name :: Name
                           , _argNames :: [ArgName]
                           , _body :: BuiltinBody m r
                           }
data BuiltinBody m r = FnBody (CodeDbId -> Env m r CodeDbId -> Either [EvalError CodeDbId] (Val m r CodeDbId))
                     | ResolvedFnBody (CodeDbId -> Env m r CodeDbId -> Changeable m r (Lam.Resolved CodeDbId) -> Changeable m r (Either [EvalError CodeDbId] (Trailing m r CodeDbId (Val m r CodeDbId))))
                     | TrailFnBody (CodeDbId -> Trail m r CodeDbId -> Env m r CodeDbId -> Either [EvalError CodeDbId] (Val m r CodeDbId))
                     | EvalFnBody ( CodeDbId
                                  -> Env m r CodeDbId
                                  -> (Val m r CodeDbId -> 
                                      Changeable m r (Either [EvalError CodeDbId] (Trailing m r CodeDbId (Val m r CodeDbId))))
                                  -> Changeable m r (Either [EvalError CodeDbId] (Trailing m r CodeDbId (Val m r CodeDbId))))

makeLenses ''Builtin

get :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] (Val m r CodeDbId)
get i argId env = case Map.lookup (CodeDbId argId) env of
                    Just v -> Right v
                    Nothing -> Left [UndefinedVar i argId]

getList :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] [Val m r CodeDbId]
getList i argId env = get i argId env >>= \v ->
  case v of
    (ValList list) -> pure list 
    _ -> Left [TypeError i $ T.pack $ show v ++ " is not a list"]

getSuspension :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] (CodeDbId, Map CodeDbId (Val m r CodeDbId))
getSuspension i argId env = get i argId env >>= \v ->
  case v of
    (Suspension resolvedId env) -> Right (resolvedId, env)
    _ -> Left [TypeError i $ T.pack $ show v ++ " is not a suspension"]

getPrim :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] Prim.Prim
getPrim i argId env = get i argId env >>= \v ->
  case v of
    Primitive p -> Right p
    _ -> Left [TypeError i $ (T.pack $ show v) `T.append` " is not a prim"]


getFrame :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] (CodeDbId, Map CodeDbId (Val m r CodeDbId), Val m r CodeDbId)
getFrame i argId env = get i argId env >>= \v ->
  case v of
    ValFrame i env args -> Right (i, env, args)
    _ -> Left [TypeError i $ (T.pack $ show v) `T.append` " is not a frame"]



getInt :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] Integer
getInt i argId env = getPrim i argId env >>= \v ->
  case v of
    (Prim.Number num) -> pure num
    p -> Left [TypeError i $ T.pack $ show p ++ " is not a number"]

getText :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] T.Text
getText i argId env = getPrim i argId env >>= \v ->
  case v of
    (Prim.Text text) -> pure text
    p -> Left [TypeError i $ T.pack $ show p ++ " is not text"]




type Thunkable m r i = (Set i, Env m r i, Thunk m r i)
getThunkable :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] (Thunkable m r CodeDbId)
getThunkable i argId env = get i argId env >>= \v ->
  case v of
    Thunk needed env body -> Right (needed, env, body)
    _ -> Left [TypeError i $ T.pack $ show v ++ " is not a thunk"]

thunkableApp :: (Ord i) => Thunkable m r i -> i -> Val m r i -> Thunkable m r i
thunkableApp (needed, env, body) argName argVal = ( Set.delete argName needed
                                                  , Map.insert argName argVal env
                                                  , body
                                                  )

thunkableVal :: Thunkable m r i -> Val m r i
thunkableVal (needed, env, body) = Thunk needed env body

-- this could be something like:
--plus = builtin "+" $ do
--         arg1 <- arg "1"
--         arg2 <- arg "2"
--         pure $ 
--           a <- askInt arg1
--           b <- askInt arg2
--           pure $ a + b
plus :: Builtin m r
plus = Builtin "+" ["+_1", "+_2"] . FnBody $ \i e -> do
  n <- getInt i "builtin-+-+_1" e
  m <- getInt i "builtin-+-+_2" e
  pure $ Primitive $ Prim.Number $ n + m

listConcat :: Builtin m r
listConcat = Builtin "listConcat" ["listConcat_1", "listConcat_2"] . FnBody $ \i e -> do
  n <- getList i "builtin-listConcat-listConcat_1" e
  m <- getList i "builtin-listConcat-listConcat_2" e
  pure $ ValList $ n ++ m

listAdd :: Builtin m r
listAdd = Builtin "listAdd" ["listAdd_elem", "listAdd_list"] . FnBody $ \i e -> do
  n <- get i "builtin-listAdd-listAdd_elem" e
  m <- getList i "builtin-listAdd-listAdd_list" e
  pure $ ValList $ n:m

listMap :: (Ref m r) => Builtin m r
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


listEmpty :: Builtin m r
listEmpty = Builtin "listEmpty" [] . FnBody $ \i e -> do
  pure $ ValList []

frameArg :: (Ref m r) => Builtin m r
frameArg = Builtin "frameArg" ["frameArg_frame", "frameArg_arg"] . ResolvedFnBody $ \i e ch_resolved -> do
  resolved <- ch_resolved
  pure $ do
    (frameId, frameEnv, _) <- getFrame i "builtin-frameArg-frameArg_frame" e
    (argId, _) <- getSuspension i "builtin-frameArg-frameArg_arg" e
    maybe (Left [UndefinedVar i (codeDbIdText argId)]) (Right . noTrail) $ Map.lookup argId frameEnv

suspensionFrameList :: (EqRef r) => Builtin m r
suspensionFrameList = Builtin "suspensionFrameList" ["suspensionFrameList_suspension"] . TrailFnBody $ \i trail e -> do
  (suspensionResolvedId, suspensionEnv) <- getSuspension i "builtin-suspensionFrameList-suspensionFrameList_suspension" e
  let trailElements = Set.toList $ fromMaybe Set.empty (Map.lookup suspensionResolvedId . unMonoidMap $ trail)
  
  pure $ ValList [ ValFrame suspensionResolvedId trailElementEnv trailElementVal
                 | (trailElementEnv, trailElementVal) <- trailElements
                 , Map.isSubmapOf suspensionEnv trailElementEnv ]

builtins :: [Builtin IO IORef]
builtins = [plus, listConcat, listAdd, listEmpty, listMap, suspensionFrameList, frameArg]

builtinId :: Builtin m r -> Id
builtinId builtin = "builtin-" `T.append` (builtin ^. name)


functionIds :: Map Name CodeDbId
functionIds = Map.fromList [(builtin ^. name, CodeDbId $ builtinId builtin) | builtin <- builtins]


builtinArgsIds :: Builtin m r -> [(Name, CodeDbId)]
builtinArgsIds builtin = [(argName, CodeDbId $ argId builtin argName)| argName <- builtin ^. argNames]
  where argId :: Builtin m r -> ArgName -> Id
        argId builtin argName = builtinId builtin `T.append` "-" `T.append` argName

builtinArgsIdsSet :: Builtin m r -> Set CodeDbId
builtinArgsIdsSet = Set.fromList . map snd . builtinArgsIds

allArgIds :: Map Name CodeDbId
allArgIds = Map.fromList $ foldMap builtinArgsIds builtins

env :: Env IO IORef CodeDbId
env = Map.fromList 
        [ (CodeDbId $ builtinId builtin
          , Thunk (builtinArgsIdsSet builtin) Map.empty $ case builtin ^. body of
                                                            FnBody fn -> ThunkFn (CodeDbId $ builtinId builtin) $ \env -> do fn (CodeDbId $ builtinId builtin) env
                                                            ResolvedFnBody fn -> ThunkResolvedFn (CodeDbId $ builtinId builtin) $ \env resolved -> do fn (CodeDbId $ builtinId builtin) env resolved
                                                            EvalFnBody fn -> ThunkEvalFn (CodeDbId $ builtinId builtin) $ \env eval -> fn (CodeDbId $ builtinId builtin) env eval
                                                            TrailFnBody fn -> ThunkTrailFn (CodeDbId $ builtinId builtin) $ \trail env -> do fn (CodeDbId $ builtinId builtin) trail env
          )
        | builtin <- builtins]
