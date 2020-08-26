module Common.Utils
  ( modByFrom
  , rightToMaybe
  , uncurry4
  , uncurry5
  , getIndices
  ) where

modByFrom :: Int -> Int -> Int -> Int
modByFrom m y x = ((x - y) `mod` m) + y

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right b) = Just b

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> g) -> (a, b, c, d, e) -> g
uncurry5 f (a, b, c, d, e) = f a b c d e

getIndices :: [Int] -> [a] -> [a]
getIndices indices xs = map (xs !!) indices