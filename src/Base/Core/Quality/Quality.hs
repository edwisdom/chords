module Base.Core.Quality.Quality
  ( Quality(..)
  ) where

data Quality
 = Major
 | Minor
 | Dominant
 | Diminished
 | Augmented
 deriving Show
