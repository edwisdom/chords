module Common.Utils
  ( modByFrom
  , rightToMaybe
  ) where

modByFrom :: Int -> Int -> Int -> Int
modByFrom m y x = ((x - y) `mod` m) + y

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right b) = Just b
