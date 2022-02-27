{-# LANGUAGE OverloadedStrings #-}
-- | Controls the general look of the program
module Snife.Theming where

import           Brick.AttrMap
import           Brick.Themes
import           Brick.Util
import           Graphics.Vty.Attributes.Color


attr1 :: AttrName
attr1 = "attr1"



theme1 :: Theme
theme1 = newTheme (brightWhite `on` black) [("transitionAttr", fg white)]
