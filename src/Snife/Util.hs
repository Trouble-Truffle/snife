module Snife.Util where

-- | Multiply initial speed by a float value
floatToDelay :: Float -> Int
floatToDelay = floor . (* fromIntegral initialSpeed)

-- | Equal to 1 second in `threadDelay`
initialSpeed :: Int
initialSpeed = 1000000

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

composeN :: Int -> (a -> a) -> (a -> a)
composeN n = compose . replicate n

