module Base.Core.Quality
  ( Quality(..)
  ) where

data Quality
 = Major
 | Minor
 | Dominant
 | Diminished Int
 | Augmented Int
 deriving Show
