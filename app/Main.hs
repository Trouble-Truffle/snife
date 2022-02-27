module Main where

import qualified Brick as B
import qualified Brick.BChan as BC
import qualified Snife.Init as SI
import qualified Snife.AppTypes as SA
import qualified Snife.Logic.EventHandler as SLE
import qualified Snife.Draw as SD
import qualified Graphics.Vty as V

import Control.Concurrent
import Control.Monad

app :: B.App SA.Game SA.Tick SA.Name
app = B.App {
      B.appDraw = SD.drawUI
   ,  B.appChooseCursor = B.neverShowCursor
   ,  B.appHandleEvent = SLE.eventHandler
   ,  B.appStartEvent = return
   ,  B.appAttrMap = SD.attrMap
   }


main :: IO ()
main = do   
   chan <- BC.newBChan 10
   void $ forkIO $ forever $ do
      BC.writeBChan chan SA.Tick
      threadDelay 1000000 -- Decides game tick speed
      
   let buildVty = V.mkVty V.defaultConfig
   initialVty <- buildVty
  
   initGame <- SI.initialGame

   void $ B.customMain initialVty buildVty (Just chan) app initGame

