module Main where

import qualified Brick.BChan as BC
import qualified Brick as B
import qualified Control.Concurrent.STM.TVar as TV
import qualified Graphics.Vty as V

import qualified Snife.Util as SU
import qualified Snife.Types as ST
import qualified Snife.Init as SI

import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
  chan <- BC.newBChan 10
  tvar <- TV.newTVarIO SU.initialSpeed
  void $ forkIO $ forever $ do
    BC.writeBChan chan ST.Tick
    TV.readTVarIO tvar  >>= threadDelay

  let mVty = V.mkVty V.defaultConfig
  vty <- mVty
  void $ B.customMain vty mVty (Just chan) SI.app SI.initialGame
