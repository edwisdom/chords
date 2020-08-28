module Base.Core.Quality.IQuality
  ( Quality(..)
  , baseQuality
  ) where

import Common.Utils (modByFrom)

data Quality
 = Major
 | Perfect
 | Minor
 | Diminished Int
 | Augmented Int

instance Show Quality where
  show Major = "M"
  show Perfect = "P"
  show Minor = "m"
  show (Diminished i) = show i ++ "dim"
  show (Augmented i) = show i ++ "aug"

baseQuality :: Int -> Quality
baseQuality n
  | canonicalized `elem` [1, 4, 5]    = Perfect
  | canonicalized `elem` [2, 3, 6, 7] = Major
  where
    canonicalized = modByFrom 7 1 n
