module CodeDbIdGen where

import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
--import qualified Data.UUID as UUID
--import qualified Data.UUID.V1
--import qualified Data.UUID.V4

import System.Random

import CodeDb (CodeDbId(..))

type CodeDbIdGen a = ReaderT Text IO a

chars :: Text
chars = T.pack $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- TODO will this ever pick the last char in the chars?
randomAlphaNumChar :: IO Text
randomAlphaNumChar = do n <-randomRIO (0::Int, T.length chars - 1)
                        pure $ (T.take 1 $ T.drop n chars)

manyBits :: IO Text
manyBits = T.concat <$> sequence [randomAlphaNumChar | _ <- [(0::Int)..16] ]

runCodeDbIdGen :: CodeDbIdGen a -> IO a
runCodeDbIdGen f = do
  baseId <- manyBits
  runReaderT f baseId

nextCodeDbId :: CodeDbIdGen CodeDbId
nextCodeDbId = do
  baseId <- ask
  tailId <- liftIO manyBits
  return $ CodeDbId $ baseId `T.append` "-" `T.append` tailId

-- It turns out that ghcjs and uuid are incompatible right now.
--
--runCodeDbIdGen :: CodeDbIdGen a -> IO a
--runCodeDbIdGen f = do
--  baseUUID <- getBaseUUID
--  runReaderT f (UUID.toText baseUUID)
--
--getBaseUUID :: IO UUID.UUID
--getBaseUUID = do
--  baseUUID <- Data.UUID.V1.nextUUID
--  case baseUUID of
--    Just u -> return u
--    Nothing -> do
--      Data.UUID.V4.nextRandom

--nextCodeDbId :: CodeDbIdGen CodeDbId
--nextCodeDbId = do
--  baseUUID <- ask
--  tailUUID <- liftIO Data.UUID.V4.nextRandom
--  return $ CodeDbId $ baseUUID `T.append` "-" `T.append` UUID.toText tailUUID


