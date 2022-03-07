module Snife.Util where

-- | Multiply initial speed by a float value
floatToDelay :: Float -> Int
floatToDelay = floor . (* fromIntegral initialSpeed)

-- | Equal to 1 second in `threadDelay`
initialSpeed :: Int
initialSpeed = 1000000

