module Main where

import Prelude hiding (id)

import Debug.Trace

import           Control.Monad (void)
import qualified Control.Monad.Adaptive as Adaptive
import           Control.Monad.Adaptive (inM)
import qualified Data.IORef as IORef
import           Data.IORef (IORef)
import qualified Data.JSString as JSS
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Text as T
import           Data.Text (Text)

import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Event as Ev
import qualified GHCJS.VDOM.Attribute as A

import           GHCJS.Foreign.Callback
import           GHCJS.Foreign.QQ
import           GHCJS.Types

import qualified Code
import CodeDb (CodeDbId(..))
import Prim (Prim(..))
import qualified Eval (Val(..))
import Eval (Val(..))
import qualified Run

textDiv :: String -> VNode
textDiv x = E.div () [ch|c|]
  where
    c = E.text . JSS.pack $ x

data Event = ClickEvent
  deriving (Eq, Show)

type AllEvents = Map Text [Event]

addEvent :: Event -> Text -> AllEvents -> AllEvents
addEvent event text allEvents = let result = addEvent' event text allEvents
                                 in trace ("ADDED EVENT " ++ show result) result
addEvent' event = Map.alter (\events -> (event:) <$> (events `mappend` (Just []))) 

renderVal :: (Text -> Event -> IO ()) -> Val m r i -> VNode
renderVal onEvent (ValList [ Primitive (Text "text")
                           , Primitive (Text t)
                           ]) = textDiv . T.unpack $ t
renderVal onEvent (ValList [ Primitive (Text "button")
                           , Primitive (Text label)
                           , Primitive (Text token)
                           ]) = E.button ( A.name . JSS.pack . T.unpack $ token
                                         , Ev.click (\e -> onEvent token ClickEvent)
                                         )
                                         [E.text . JSS.pack . T.unpack $ label]
renderVal onEvent (ValList elems) = E.div () $ map (renderVal onEvent) elems

envFromEvents :: Map Text [Event] -> Map CodeDbId (Val IO IORef CodeDbId)
envFromEvents = Map.fromList . map envFromEvent . Map.assocs
  where envFromEvent (token, events) = (CodeDbId token, ValList $ eventVal <$> events)
        eventVal event = Eval.Primitive . Prim.Text . T.pack $ show event


runWeb :: VMount -> IO ()
runWeb mountPoint = do (bindingsWithIds, projection) <- Run.projectCode Code.web
                       Adaptive.run $ do
                         events <- Adaptive.newMod $ inM $ pure $ Map.empty
                         let initialEnv = envFromEvents <$> Adaptive.readMod events
                         (m_bindingsWithIds, ch_evaluated) <- Run.runProjectionWithEnv bindingsWithIds initialEnv
                         let rerender token event = Adaptive.run $ do currentEvents <- Adaptive.inCh $ Adaptive.readMod events
                                                                      Adaptive.change events $ addEvent event ("events-" `T.append` token) currentEvents
                                                                      Adaptive.propagate
                                                                      (inM . render rerender mountPoint) =<< ch_evaluated
                                                                      
                         (inM . render rerender mountPoint) =<< ch_evaluated

render :: (Text -> Event -> IO ()) -> VMount -> Either [Run.Error] (Val IO IORef CodeDbId) -> IO ()
render onEvent mountPoint val = do let vdom = either (textDiv . show) (renderVal onEvent) val
                                   p <- diff mountPoint vdom
                                   void $ patch mountPoint p

main :: IO ()
main = do
  Ev.initEventDelegation Ev.defaultEvents
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  m <- mount root (E.div () ())
  runWeb m
