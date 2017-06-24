module Builtins (functionIds, allArgIds, EvalError(..), Builtin, builtinsEnv, unionVal) where

import Control.Lens
import Control.Monad ((>=>))
import Data.Either (partitionEithers)
import Data.Foldable (foldrM)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T


import qualified Id
import Eval (Env, Val(..), EvalError(..), Thunk(..), Trail, Trailing, noTrail, Suspension(..), Frame, frameCodeDbId, eitherList)
import qualified Eval as Eval
import CodeDb (CodeDbId (..), codeDbIdText)
import qualified Lam
import qualified Primitives

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
                 | Record

makeLenses ''Builtin

get :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] BuiltinVal
get i argId env = case Map.lookup (CodeDbId argId) env of
                    Just v -> Right v
                    Nothing -> Left [UndefinedVar i argId]

getList :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] [BuiltinVal]
getList i argId env = get i argId env >>= asList i

getListOf :: CodeDbId -> T.Text -> BuiltinEnv -> (CodeDbId -> BuiltinVal -> Either [EvalError CodeDbId] a) -> Either [EvalError CodeDbId] [a]
getListOf i argId env f = (fmap (f i) <$> getList i argId env) >>= eitherList

asList :: i -> Val i -> Either [EvalError i] [Val i]
asList i v = case v of
  (ValList list) -> pure list 
  _ -> Left [TypeError i v $ " is not a list"]

getSuspension :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] BuiltinSuspension
getSuspension i argId env = get i argId env >>= \v ->
  case v of
    (ValSuspension suspension) -> Right suspension
    _ -> Left [TypeError i v " is not a suspension"]

getPrimitives :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Primitives.Prim
getPrimitives i argId env = get i argId env >>= asPrimitives i

getFrame :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] (Frame CodeDbId)
getFrame i argId env = get i argId env >>= \v ->
  case v of
    ValFrame frame -> Right frame
    _ -> Left [TypeError i  v "is not a frame"]

asPrimitives :: i -> Val i -> Either [EvalError i] Primitives.Prim
asPrimitives i v = case v of
  Primitive p -> Right p
  _ -> Left [TypeError i v "is not a prim"]

asNumber :: i -> Primitives.Prim -> Either [EvalError i] Integer
asNumber i v = case v of
                 (Primitives.Number num) -> pure num
                 p -> Left [TypeError i (Primitive p) " is not a number"]

asValNumber :: i -> Val i -> Either [EvalError i] Integer
asValNumber i v = asPrimitives i v >>= asNumber i

getNumber :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Integer
getNumber i argId env = getPrimitives i argId env >>= asNumber i

getText :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] T.Text
getText i argId env = getPrimitives i argId env >>= asText i

asValText :: i -> Val i -> Either [EvalError i] T.Text
asValText i v = asPrimitives i v >>= asText i



asText :: i -> Primitives.Prim -> Either [EvalError i] T.Text
asText i v = case v of
                 (Primitives.Text t) -> pure t
                 p -> Left [TypeError i (Primitive p) " is not text"]




type Thunkable = ([[CodeDbId]], Set CodeDbId, BuiltinEnv, BuiltinThunk)
getThunkable :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Thunkable
getThunkable i argId env = get i argId env >>= \v ->
  case v of
    Thunk sus needed thunkEnv thunkBody -> Right (sus, needed, thunkEnv, thunkBody)
    _ -> Left [TypeError i v " is not a thunk"]

numberToText :: Builtin
numberToText = Builtin "numberToText" ["numberToText_number"] . FnBody $ \i e _ -> do
  n <- getNumber i "builtin-numberToText-numberToText_number" e
  pure $ Primitive $ Primitives.Text $ T.pack $ show n

instantToText :: Builtin
instantToText = Builtin "instantToText" ["instantToText_instant"] . FnBody $ \i e _ -> do
  instantPieces <- getList i "builtin-instantToText-instantToText_instant" e
  case instantPieces of
    [_,_,_,_] -> pure $ Primitive $ Primitives.Text $ T.pack $ show instantPieces
    _ -> Left [TypeError i (ValList $ instantPieces) "is not an instant"]



-- this could be something like:
--plus = builtin "+" $ do
--         arg1 <- arg "1"
--         arg2 <- arg "2"
--         pure $ 
--           a <- askInt arg1
--           b <- askInt arg2
--           pure $ a + b
plus :: Builtin
plus = Builtin "+" ["+_1", "+_2"] . FnBody $ \i e _ -> do
  n <- getNumber i "builtin-+-+_1" e
  m <- getNumber i "builtin-+-+_2" e
  pure $ Primitive $ Primitives.Number $ n + m

minus :: Builtin
minus = Builtin "-" ["-_1", "-_2"] . FnBody $ \i e _ -> do
  n <- getNumber i "builtin----_1" e
  m <- getNumber i "builtin----_2" e
  pure $ Primitive $ Primitives.Number $ n - m

numberCompare :: Builtin
numberCompare = Builtin "numberCompare" ["numberCompare_1", "numberCompare_2"] . FnBody $ \i e _ -> do
  n <- getNumber i "builtin-numberCompare-numberCompare_1" e
  m <- getNumber i "builtin-numberCompare-numberCompare_2" e
  pure $ makeCompareResult e $ n `compare` m

listAppend :: Builtin
listAppend = Builtin "listAppend" ["listAppend_1", "listAppend_2"] . FnBody $ \i e _ -> do
  n <- getList i "builtin-listAppend-listAppend_1" e
  m <- getList i "builtin-listAppend-listAppend_2" e
  pure $ ValList $ n ++ m

listConcat :: Builtin
listConcat = Builtin "listConcat" ["listConcat_lists"] . FnBody $ \i e _ -> do
  listVals <- getList i "builtin-listConcat-listConcat_lists" e
  lists <- eitherList $ asList i <$> listVals
  pure $ ValList $ concat lists



listAdd :: Builtin
listAdd = Builtin "listAdd" ["listAdd_elem", "listAdd_list"] . FnBody $ \i e _ -> do
  n <- get i "builtin-listAdd-listAdd_elem" e
  m <- getList i "builtin-listAdd-listAdd_list" e
  pure $ ValList $ n:m

listMap :: Builtin
listMap = Builtin "listMap" ["listMap_f", "listMap_list"] . EvalFnBody $ \i e _ eval -> do
  thunks <- do thunkable <- getThunkable i "builtin-listMap-listMap_f" e
               list <- getList i "builtin-listMap-listMap_list" e
               eitherList $ callOneArgThunkable i "listMap_f" thunkable <$> list
  vals <- mapM eval thunks
  pure $ ValList <$> sequenceA vals

callOneArgThunkable :: Ord i => i -> T.Text -> ([[i]], Set i, Map i (Val i), Eval.ThunkWithId i) -> Val i -> Either [EvalError i] (Val i)
callOneArgThunkable i label (thunkSus, thunkNeeded, thunkEnv, thunkBody) argValue = case Set.toList thunkNeeded of
  [argId] -> Right $ Thunk thunkSus Set.empty (Map.insert argId argValue thunkEnv) thunkBody
  [] -> Left [TypeError i (Thunk thunkSus thunkNeeded thunkEnv thunkBody) $ "No arg for " `T.append` label]
  _ -> Left [TypeError i (Thunk thunkSus thunkNeeded thunkEnv thunkBody) $ "Too many args for " `T.append` label]

listFilter :: Builtin
listFilter = Builtin "listFilter" ["listFilter_f", "listFilter_list"] . EvalFnBody $ \i e _ eval -> do
  list <- getList i "builtin-listFilter-listFilter_list" e
  thunks <- do (thunkSus, thunkNeeded, thunkEnv, thunkBody) <- getThunkable i "builtin-listFilter-listFilter_f" e
               case Set.toList thunkNeeded of
                 [argId] -> pure [ Thunk thunkSus Set.empty (Map.insert argId listElem thunkEnv) thunkBody
                                 | listElem <- list]
                 [] -> Left [TypeError i (Thunk thunkSus thunkNeeded thunkEnv thunkBody) "No arg for listFilter_f"]
                 _ -> Left [TypeError i (Thunk thunkSus thunkNeeded thunkEnv thunkBody) "Too many args for listFilter_f"]
  ts <- sequenceA <$> mapM eval thunks
  let matchListVs vs = zipWith (\l v -> if v == Primitive (Primitives.Number 0) then Nothing else Just l) list vs
  let trail_filtered = (\vs -> catMaybes . matchListVs $ vs) <$> ts
  pure $ ValList <$> trail_filtered


listEmpty :: Builtin
listEmpty = Builtin "listEmpty" [] . FnBody $ \_ _ _ -> do
  pure $ ValList []

listLength :: Builtin
listLength = Builtin "listLength" ["listLength_list"] . FnBody $ \i e _ -> do
  list <- getList i "builtin-listLength-listLength_list" e
  pure $ Primitive $ Primitives.Number $ fromIntegral $ length list

listSum :: Builtin
listSum = Builtin "listSum" ["listSum_list"] . FnBody $ \i e _ -> do
  list <- getList i "builtin-listSum-listSum_list" e
  case partitionEithers ((asPrimitives i >=> asNumber i) <$> list) of
    ([], ints) -> pure $ Primitive $ Primitives.Number $ fromIntegral $ sum ints
    (errs, _) -> Left $ concat errs

listReduce :: Builtin
listReduce = Builtin "listReduce" ["listReduce_list", "listReduce_base", "listReduce_merge", "listReduce_merge_curr", "listReduce_merge_next"] . EvalFnBody $ \i e _ eval -> do
  list <- getList i "builtin-listReduce-listReduce_list" e
  base <- get i "builtin-listReduce-listReduce_base" e
  merge <- getThunkable i "builtin-listReduce-listReduce_merge" e
  (Suspension argCurr _ _) <- getSuspension i "builtin-listReduce-listReduce_merge_curr" e
  (Suspension argNext _ _) <- getSuspension i "builtin-listReduce-listReduce_merge_next" e
  let f trail_next trail_curr = Eval.withEitherTrail eval $ Eval.invertTrailingEither $ ((\next curr -> appThunkable i "listReduce" merge [(argCurr, curr), (argNext, next)]) <$> trail_next <*> trail_curr)
  foldrM f (noTrail base) (noTrail <$> list)
  
appThunkable :: CodeDbId -> T.Text -> Thunkable -> [(CodeDbId, BuiltinVal)] -> Either [EvalError CodeDbId] BuiltinVal
appThunkable i n (thunkSus, thunkNeeded, thunkEnv, thunkBody) args | thunkNeeded == (Set.fromList . fmap fst $ args) = Right $ Thunk thunkSus Set.empty (Map.union (Map.fromList args) thunkEnv) thunkBody
                                                                   | otherwise                                       = Left [TypeError i (Thunk thunkSus thunkNeeded thunkEnv thunkBody) $ "Wrong args " `T.append` (T.pack . show . fmap fst $ args) `T.append` " for " `T.append` n]


frameArg :: Builtin
frameArg = Builtin "frameArg" ["frameArg_frame", "frameArg_arg"] . ResolvedFnBody $ \_ i e _ _ -> do
  frame <- getFrame i "builtin-frameArg-frameArg_frame" e
  (Suspension argId _ _) <- getSuspension i "builtin-frameArg-frameArg_arg" e
  maybe (Left [UndefinedMember i frame (codeDbIdText argId)]) (Right . noTrail) $ Map.lookup argId (frame ^. Eval.frameEnv)

htmlElementEvents :: Builtin
htmlElementEvents = Builtin "htmlElementEvents" ["htmlElementEvents_element"] . ResolvedFnBody $ \_ i e g _ -> do
  let eventsByToken frame = case Eval.lookupVarByResolvedId e g (CodeDbId $ "events-" `T.append` (T.pack . show $ frame)) of
                              Just (ValList v) -> Right . noTrail $ ValList v
                              Nothing -> Right . noTrail . ValList $ []
                              Just v -> Left [TypeError i v $ "non-events at " `T.append` (T.pack . show $ frame)]
  getList i "builtin-htmlElementEvents-htmlElementEvents_element" e >>= \case
    [Primitive (Primitives.Text "button"), _, ValFrame frame] -> eventsByToken frame
    [Primitive (Primitives.Text "textInput"), ValFrame frame] -> eventsByToken frame
    fields -> Left [TypeError i (ValList fields) $ " is not an element with events"]


instantCompare :: Builtin
instantCompare = Builtin "instantCompare" ["instantCompare_1", "instantCompare_2"] . FnBody $ \i e _ -> do
  n <- getNumber i "builtin-instantCompare-instantCompare_1" e
  m <- getNumber i "builtin-instantCompare-instantCompare_2" e
  pure $ makeCompareResult e $ n `compare` m


htmlText :: Builtin
htmlText = Builtin "htmlText" ["htmlText_text"] . FnBody $ \i e _ -> do
  text <- getText i "builtin-htmlText-htmlText_text" e
  pure $ ValList [Primitive $ Primitives.Text "text", Primitive $ Primitives.Text text]

htmlTextInputValue :: Builtin
htmlTextInputValue = Builtin "htmlTextInputValue" ["htmlTextInputValue_htmlInput", "htmlTextInputValue_instant"] . ResolvedFnBody $ \_ i e g _ -> do
  frame <- getList i "builtin-htmlTextInputValue-htmlTextInputValue_htmlInput" e >>= \case
             [Primitive (Primitives.Text "textInput"), ValFrame f] -> Right f
             fields -> Left [TypeError i (ValList fields) $ " is not a text input"]
  instant <- getListOf i "builtin-htmlTextInputValue-htmlTextInputValue_instant" e asValNumber

  let val = Eval.lookupVarByResolvedId e g (CodeDbId $ "props-" `T.append` (T.pack . show $ frame) `T.append` "-" `T.append` (T.pack . show $ instant) `T.append` "-TextValue")
  case val of
    Just v -> pure . Primitive . Primitives.Text <$> (asValText i $ v)
    Nothing -> pure . pure . Primitive . Primitives.Text $ ""


htmlButton :: Builtin
htmlButton = Builtin "htmlButton" ["htmlButton_text"] . ResolvedFnBody $ \frame i e _ _ -> do
  text <- getText i "builtin-htmlButton-htmlButton_text" e
  pure $ noTrail $ ValList [Primitive $ Primitives.Text "button", Primitive $ Primitives.Text text, ValFrame frame]

htmlTextInput :: Builtin
htmlTextInput = Builtin "htmlTextInput" [] . ResolvedFnBody $ \frame _ _ _ _ -> do
  pure $ noTrail $ ValList [Primitive $ Primitives.Text "textInput", ValFrame frame]


suspensionFrameList :: Builtin
suspensionFrameList = Builtin "suspensionFrameList" ["suspensionFrameList_suspension"] . TrailFnBody $ \i trail e _ -> do
  suspension <- getSuspension i "builtin-suspensionFrameList-suspensionFrameList_suspension" e
  pure $ ValList [ ValFrame frame
                 | frame <- suspensionFrames trail suspension]

iterateUntilNothing :: (a -> Maybe a) -> a -> [a]
iterateUntilNothing f = catMaybes . takeWhile isJust . iterate (>>= f) . Just

iterateUntilNothing' :: (Show a) => (a -> Maybe a) -> Maybe a -> [a]
iterateUntilNothing' f = fromMaybe [] . fmap (iterateUntilNothing f)


suspensionFrames :: BuiltinTrail -> BuiltinSuspension -> [BuiltinFrame]
suspensionFrames trail (Suspension suspensionResolvedId suspensionEnv suspensionParents) = 
  [ frame
  | (frame, _) <- Set.toList trail
  , frame ^. frameCodeDbId == suspensionResolvedId
  , Map.isSubmapOf suspensionEnv (frame ^. Eval.frameEnv)
  , suspensionParentsMatchFrameAncestors (frame ^. Eval.frameParent) suspensionParents
  ]
  where suspensionParentsMatchFrameAncestors parentFrame suspensionParents' = all (suspensionMatchesAFrame (iterateUntilNothing' (^. Eval.frameParent) parentFrame)) suspensionParents'

        suspensionMatchesAFrame parentFrames suspension = not . null $ List.intersect (suspensionFrames trail suspension) parentFrames


frameResult :: Builtin
frameResult = Builtin "frameResult" ["frameResult_frame"] . TrailFnBody $ \i trail e _ -> do
  targetFrame <- getFrame i "builtin-frameResult-frameResult_frame" e
  let results = [ result
                | (frame, result) <- Set.toList trail
                , frame == targetFrame
                ]
  case results of
    [] -> Left [UndefinedVar i "NO RESULT"]
    xs -> Right $ last xs

event :: Builtin
event = Builtin "event" ["event_instant", "event_details"] Record

construct :: Builtin
construct = Builtin "construct" ["construct_with", "construct_payload"] $ FnBody $ \i e _ -> do
  (Suspension constructorId _ _) <- getSuspension i "builtin-construct-construct_with" e
  payload <- get i "builtin-construct-construct_payload" e
  Right $ unionVal e constructorId payload


unionVal :: BuiltinEnv -> CodeDbId -> BuiltinVal -> BuiltinVal
unionVal env constructorId payload = Thunk [] (Set.fromList [constructorId]) env $ Id.WithId dubiousId $ Identity $ ThunkFn $ \e _ -> do
    caseThunkable <- getThunkable dubiousId (codeDbIdText constructorId) e
    callOneArgThunkable dubiousId  "construct" caseThunkable payload
  where dubiousId = CodeDbId "builtin-construct-unionVal"

builtins :: [Builtin]
builtins = [ event, construct
           , plus, minus, numberCompare
           , listAppend, listAdd, listConcat, listEmpty, listMap, listFilter, listReduce, listLength, listSum
           , numberToText, instantToText
           , suspensionFrameList, frameArg, frameResult
           , htmlText, htmlButton, htmlTextInput, htmlTextInputValue, htmlElementEvents, instantCompare
           ]

makeCompareResult :: BuiltinEnv -> Ordering -> BuiltinVal
makeCompareResult env c = unionVal env (CodeDbId . con $ c) (Primitive . Primitives.Text . T.pack . show $ c)
  where con LT = "builtin-compareResult-compareResult_1_smaller"
        con EQ = "builtin-compareResult-compareResult_equal"
        con GT = "builtin-compareResult-compareResult_1_greater"

data BuiltinUnion = BuiltinUnion Name [Name]

htmlEventDetails :: BuiltinUnion
htmlEventDetails = BuiltinUnion "htmlEventDetails" ["htmlEventDetails_textChange", "htmlEventDetails_click"]

compareResult :: BuiltinUnion
compareResult = BuiltinUnion "compareResult" ["compareResult_1_greater", "compareResult_1_smaller", "compareResult_equal"]

maybeResult :: BuiltinUnion
maybeResult = BuiltinUnion "maybeResult" ["maybeResult_just", "maybeResult_nothing"]

builtinUnions :: [BuiltinUnion]
builtinUnions = [ compareResult, maybeResult
                , htmlEventDetails ]

builtinId :: Builtin -> Id
builtinId builtin = builtinIdFromName (builtin ^. name)

builtinIdFromName :: T.Text -> Id
builtinIdFromName builtinName = "builtin-" `T.append` builtinName


functionIds :: Map Name CodeDbId
functionIds = Map.fromList [(builtin ^. name, CodeDbId $ builtinId builtin) | builtin <- builtins]

argsIdsFromPrefix :: T.Text -> [T.Text] -> [(Name, CodeDbId)]
argsIdsFromPrefix prefix names = [(argName, CodeDbId $ argId argName)| argName <- names]
  where argId :: ArgName -> Id
        argId argName = prefix `T.append` "-" `T.append` argName



builtinArgsIds :: Builtin -> [(Name, CodeDbId)]
builtinArgsIds builtin = argsIdsFromPrefix (builtinId builtin) (builtin ^. argNames)

builtinArgsIdsSet :: Builtin -> Set CodeDbId
builtinArgsIdsSet = Set.fromList . map snd . builtinArgsIds

builtinUnionsArgsIds :: BuiltinUnion -> [(Name, CodeDbId)]
builtinUnionsArgsIds (BuiltinUnion unionName constructors) = argsIdsFromPrefix (builtinIdFromName unionName) constructors

allArgIds :: Map Name CodeDbId
allArgIds = Map.fromList $ foldMap builtinArgsIds builtins ++ foldMap builtinUnionsArgsIds builtinUnions

builtinsEnv :: Env CodeDbId
builtinsEnv = Map.fromList 
        [ (CodeDbId $ builtinId builtin
          , Thunk [] (builtinArgsIdsSet builtin) Map.empty . Id.withId (CodeDbId $ builtinId builtin) $ case builtin ^. body of
                                                            FnBody fn -> ThunkFn $ \env globalEnv -> do fn (CodeDbId $ builtinId builtin) env globalEnv
                                                            ResolvedFnBody fn -> ThunkResolvedFn $ \magic env globalEnv resolved -> do fn magic (CodeDbId $ builtinId builtin) env globalEnv resolved
                                                            EvalFnBody fn -> ThunkEvalFn $ \env globalEnv eval -> fn (CodeDbId $ builtinId builtin) env globalEnv eval
                                                            TrailFnBody fn -> ThunkTrailFn $ \trail env globalEnv -> do fn (CodeDbId $ builtinId builtin) trail env globalEnv
                                                            Record -> ThunkRecord
          )
        | builtin <- builtins]
