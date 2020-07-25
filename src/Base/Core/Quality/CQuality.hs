module Base.Core.Quality.CQuality
  ( Quality(..)
  ) where

data Quality
 = Major
 | Minor
 | Dominant
 | Diminished
 | Augmented
 deriving Show
