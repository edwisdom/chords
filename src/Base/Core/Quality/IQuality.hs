module Base.Core.Quality.IQuality
  ( Quality(..)
  , baseQuality
  ) where

data Quality
 = Major
 | Perfect
 | Minor
 | Diminished Int
 | Augmented Int
 deriving Show

baseQuality :: Int -> Quality
baseQuality n
  | canonicalized `elem` [1, 4, 5]    = Perfect
  | canonicalized `elem` [2, 3, 6, 7] = Major
  where
    canonicalized = ((n - 1) `mod` 7) + 1
