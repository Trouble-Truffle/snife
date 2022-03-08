module Snife.Util where

-- | Multiply initial speed by a float value
floatToDelay :: Float -> Int
floatToDelay = floor . (* fromIntegral initialSpeed)

-- | Equal to 1 second in `threadDelay`
mainSpeed :: Int
mainSpeed = 1000000

-- | equal to 100ms
initialSpeed :: Int
initialSpeed = 100000

-- | The increment/decrement when  changing speeds
speedIncrement :: Float
speedIncrement = 0.01

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

composeN :: Int -> (a -> a) -> (a -> a)
composeN n = compose . replicate n



