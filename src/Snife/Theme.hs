module Snife.Theme where

import Brick
import Graphics.Vty.Attributes

import Snife.Types


themeGenerator :: Game -> AttrMap
themeGenerator game = attrMap defAttr [
    (show' Dead, bg black)
  , (show' Alive, bg white)
  , (show' Transition, bg red)
  , (show' Snake, bg green)
  ]
  where show' = attrName . show
