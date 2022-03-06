module Snife.Init where

import Brick


app = App {
    appDraw = undefined
  , appChooseCursor = neverShowCursor
  , appHandleEvent = undefined
  , appStartEvent = return
  , appAttrMap = undefined
  }


