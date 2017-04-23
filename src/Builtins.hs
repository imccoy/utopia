module Builtins (functionIds, allArgIds, EvalError(..), Builtin, env) where

import Debug.Trace

import Control.Lens
import Control.Monad ((>=>))
import Data.Either (partitionEithers)
import Data.IORef
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T


import qualified Id
import Eval (Env, Val(..), EvalError(..), Thunk(..), Trail(..), Trailing, noTrail, Suspension(..), Frame, frameCodeDbId, eitherList)
import qualified Eval as Eval
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

type BuiltinEnv = Env CodeDbId
type BuiltinGlobalEnv = Eval.GlobalEnv CodeDbId
type BuiltinVal = Val CodeDbId
type BuiltinThunk = Id.WithId CodeDbId Identity (Thunk CodeDbId)
type BuiltinSuspension = Suspension CodeDbId
type BuiltinTrail = Trail CodeDbId
type BuiltinFrame = Frame CodeDbId
type BuiltinTrailing = Trailing CodeDbId

data BuiltinBody = FnBody (CodeDbId -> BuiltinEnv -> BuiltinGlobalEnv -> Either [EvalError CodeDbId] BuiltinVal)
                 | ResolvedFnBody (BuiltinFrame -> CodeDbId -> BuiltinEnv -> BuiltinGlobalEnv -> Lam.Resolved CodeDbId -> Either [EvalError CodeDbId] (BuiltinTrailing BuiltinVal))
                 | TrailFnBody (CodeDbId -> Trail CodeDbId -> BuiltinEnv -> BuiltinGlobalEnv -> Either [EvalError CodeDbId] BuiltinVal)
                 | EvalFnBody ( CodeDbId
                              -> BuiltinEnv
                              -> BuiltinGlobalEnv
                              -> (BuiltinVal -> 
                                  Either [EvalError CodeDbId] (BuiltinTrailing BuiltinVal))
                              -> Either [EvalError CodeDbId] (BuiltinTrailing BuiltinVal))

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

getSuspension :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] BuiltinSuspension
getSuspension i argId env = get i argId env >>= \v ->
  case v of
    (ValSuspension suspension) -> Right suspension
    _ -> Left [TypeError i $ T.pack $ show v ++ " is not a suspension"]

getPrim :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Prim.Prim
getPrim i argId env = get i argId env >>= asPrim i

getFrame :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] (Frame CodeDbId)
getFrame i argId env = get i argId env >>= \v ->
  case v of
    ValFrame frame -> Right frame
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
numberToText = Builtin "numberToText" ["numberToText_number"] . FnBody $ \i e g -> do
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
plus = Builtin "+" ["+_1", "+_2"] . FnBody $ \i e g -> do
  n <- getNumber i "builtin-+-+_1" e
  m <- getNumber i "builtin-+-+_2" e
  pure $ Primitive $ Prim.Number $ n + m

minus :: Builtin
minus = Builtin "-" ["-_1", "-_2"] . FnBody $ \i e g -> do
  n <- getNumber i "builtin----_1" e
  m <- getNumber i "builtin----_2" e
  pure $ Primitive $ Prim.Number $ n - m


boolNot :: Builtin
boolNot = Builtin "not" ["not_1"] . FnBody $ \i e g -> do
  n <- getNumber i "builtin-+-+_1" e
  pure $ Primitive $ Prim.Number $ if n == 0 then 1 else 0

listConcat :: Builtin
listConcat = Builtin "listConcat" ["listConcat_1", "listConcat_2"] . FnBody $ \i e g -> do
  n <- getList i "builtin-listConcat-listConcat_1" e
  m <- getList i "builtin-listConcat-listConcat_2" e
  pure $ ValList $ n ++ m

listAdd :: Builtin
listAdd = Builtin "listAdd" ["listAdd_elem", "listAdd_list"] . FnBody $ \i e g -> do
  n <- get i "builtin-listAdd-listAdd_elem" e
  m <- getList i "builtin-listAdd-listAdd_list" e
  pure $ ValList $ n:m

listMap :: Builtin
listMap = Builtin "listMap" ["listMap_f", "listMap_list"] . EvalFnBody $ \i e g eval -> do
  thunks <- do (thunkNeeded, thunkEnv, thunkBody) <- getThunkable i "builtin-listMap-listMap_f" e
               list <- getList i "builtin-listMap-listMap_list" e
               case Set.toList thunkNeeded of
                 [argId] -> trace ("Mapping into " ++ show argId ++ " of " ++ (show $ thunkBody ^. Id.id)) $
                            pure [ Thunk Set.empty (Map.insert argId elem thunkEnv) thunkBody
                                 | elem <- list]
                 [] -> Left [TypeError i "No arg for listMap_f"]
                 _ -> Left [TypeError i "Too many args for listMap_f"]
  vals <- mapM eval thunks
  pure $ ValList <$> sequenceA vals

listFilter :: Builtin
listFilter = Builtin "listFilter" ["listFilter_f", "listFilter_list"] . EvalFnBody $ \i e g eval -> do
  list <- getList i "builtin-listFilter-listFilter_list" e
  thunks <- do (thunkNeeded, thunkEnv, thunkBody) <- getThunkable i "builtin-listFilter-listFilter_f" e
               case Set.toList thunkNeeded of
                 [argId] -> pure [ Thunk Set.empty (Map.insert argId elem thunkEnv) thunkBody
                                 | elem <- list]
                 [] -> Left [TypeError i "No arg for listFilter_f"]
                 _ -> Left [TypeError i "Too many args for listFilter_f"]
  ts <- sequenceA <$> mapM eval thunks
  let matchListVs vs = zipWith (\l v -> if v == Primitive (Prim.Number 0) then Nothing else Just l) list vs
  let trail_filtered = (\vs -> catMaybes . matchListVs $ vs) <$> ts
  pure $ ValList <$> trail_filtered


listEmpty :: Builtin
listEmpty = Builtin "listEmpty" [] . FnBody $ \i e g -> do
  pure $ ValList []

listLength :: Builtin
listLength = Builtin "listLength" ["listLength_list"] . FnBody $ \i e g -> do
  list <- getList i "builtin-listLength-listLength_list" e
  pure $ Primitive $ Prim.Number $ fromIntegral $ length list

listSum :: Builtin
listSum = Builtin "listSum" ["listSum_list"] . FnBody $ \i e g -> do
  list <- getList i "builtin-listSum-listSum_list" e
  case partitionEithers ((asPrim i >=> asNumber i) <$> list) of
    ([], ints) -> pure $ Primitive $ Prim.Number $ fromIntegral $ sum ints
    (errs, _) -> Left $ concat errs


frameArg :: Builtin
frameArg = Builtin "frameArg" ["frameArg_frame", "frameArg_arg"] . ResolvedFnBody $ \_ i e g resolved -> do
  frame <- getFrame i "builtin-frameArg-frameArg_frame" e
  (Suspension argId _ _) <- getSuspension i "builtin-frameArg-frameArg_arg" e
  maybe (Left [UndefinedVar i (codeDbIdText argId)]) (Right . noTrail) $ Map.lookup argId (frame ^. Eval.frameEnv)

htmlElementEvents :: Builtin
htmlElementEvents = Builtin "htmlElementEvents" ["htmlElementEvents_element"] . ResolvedFnBody $ \_ i e g _ -> do
  let eventsByToken frame = case Eval.lookupVarByResolvedId e g (CodeDbId $ "events-" `T.append` (T.pack . show $ frame)) of
                              Just (ValList v) -> Right . noTrail $ ValList v
                              Nothing -> Right . noTrail . ValList $ []
                              Just v -> Left [TypeError i $ T.pack $ "non-events " ++ show v ++ " at " ++ show frame]
  case getList i "builtin-htmlElementEvents-htmlElementEvents_element" e of
    Left e -> Left e
    Right element -> case element of
                       [Primitive (Prim.Text "button"), _, ValFrame frame] -> eventsByToken frame
                       [Primitive (Prim.Text "textInput"), ValFrame frame] -> eventsByToken frame
                       _ -> Left [TypeError i $ T.pack $ show element ++ " is not an element with events"]


htmlText :: Builtin
htmlText = Builtin "htmlText" ["htmlText_text"] . FnBody $ \i e g -> do
  text <- getText i "builtin-htmlText-htmlText_text" e
  pure $ ValList [Primitive $ Prim.Text "text", Primitive $ Prim.Text text]

htmlButton :: Builtin
htmlButton = Builtin "htmlButton" ["htmlButton_text"] . ResolvedFnBody $ \frame i e g resolved -> do
  text <- getText i "builtin-htmlButton-htmlButton_text" e
  pure $ noTrail $ ValList [Primitive $ Prim.Text "button", Primitive $ Prim.Text text, ValFrame frame]

htmlTextInput :: Builtin
htmlTextInput = Builtin "htmlTextInput" [] . ResolvedFnBody $ \frame i e g resolved -> do
  pure $ noTrail $ ValList [Primitive $ Prim.Text "textInput", ValFrame frame]


htmlTextInputText :: Builtin
htmlTextInputText = Builtin "htmlTextInput" [] . ResolvedFnBody $ \magic i e g resolved -> do
  let textToken = T.pack . show $ magic
  pure $ noTrail $ ValList [Primitive $ Prim.Text "textInput", Primitive $ Prim.Text textToken]



suspensionFrameList :: Builtin
suspensionFrameList = Builtin "suspensionFrameList" ["suspensionFrameList_suspension"] . TrailFnBody $ \i trail e g -> do
  suspension <- getSuspension i "builtin-suspensionFrameList-suspensionFrameList_suspension" e
  pure $ ValList [ ValFrame frame
                 | frame <- suspensionFrames trail suspension]

iterateUntilNothing :: (a -> Maybe a) -> a -> [a]
iterateUntilNothing f e = e:go e
  where go e = case f e of
                 Nothing -> []
                 (Just e') -> e':go e'

iterateUntilNothing' :: (Show a) => (a -> Maybe a) -> Maybe a -> [a]
iterateUntilNothing' f = fromMaybe [] . fmap (iterateUntilNothing f)


suspensionFrames :: BuiltinTrail -> BuiltinSuspension -> [BuiltinFrame]
suspensionFrames trail (Suspension suspensionResolvedId suspensionEnv suspensionParents) = 
  [ frame
  | (frame, result) <- Set.toList trail
  , frame ^. frameCodeDbId == suspensionResolvedId
  , Map.isSubmapOf suspensionEnv (frame ^. Eval.frameEnv)
  , suspensionParentsMatchFrameAncestors (frame ^. Eval.frameParent) suspensionParents
  ]
  where suspensionParentsMatchFrameAncestors parentFrame suspensionParents = all (suspensionMatchesAFrame (iterateUntilNothing' (^. Eval.frameParent) parentFrame)) suspensionParents

        suspensionMatchesAFrame parentFrames suspension = not . null $ List.intersect (suspensionFrames trail suspension) parentFrames


frameResult :: Builtin
frameResult = Builtin "frameResult" ["frameResult_frame"] . TrailFnBody $ \i trail e g -> do
  targetFrame <- getFrame i "builtin-frameResult-frameResult_frame" e
  let results = [ result
                | (frame, result) <- Set.toList trail
                , frame == targetFrame
                ]
  case results of
    [] -> Left [UndefinedVar i "NO RESULT"]
    xs -> Right $ last xs



builtins :: [Builtin]
builtins = [ plus, minus
           , listConcat, listAdd, listEmpty, listMap, listFilter, listLength, listSum
           , numberToText
           , suspensionFrameList, frameArg, frameResult
           , htmlText, htmlButton, htmlTextInput, htmlElementEvents]

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

env :: Env CodeDbId
env = Map.fromList 
        [ (CodeDbId $ builtinId builtin
          , Thunk (builtinArgsIdsSet builtin) Map.empty . Id.withId (CodeDbId $ builtinId builtin) $ case builtin ^. body of
                                                            FnBody fn -> ThunkFn $ \env globalEnv -> do fn (CodeDbId $ builtinId builtin) env globalEnv
                                                            ResolvedFnBody fn -> ThunkResolvedFn $ \magic env globalEnv resolved -> do fn magic (CodeDbId $ builtinId builtin) env globalEnv resolved
                                                            EvalFnBody fn -> ThunkEvalFn $ \env globalEnv eval -> fn (CodeDbId $ builtinId builtin) env globalEnv eval
                                                            TrailFnBody fn -> ThunkTrailFn $ \trail env globalEnv -> do fn (CodeDbId $ builtinId builtin) trail env globalEnv
          )
        | builtin <- builtins]
