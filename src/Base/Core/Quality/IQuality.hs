module Base.Core.Quality.IQuality
  ( Quality(..)
  ) where

data Quality
 = Major
 | Perfect
 | Minor
 | Diminished Int
 | Augmented Int
 deriving Show
