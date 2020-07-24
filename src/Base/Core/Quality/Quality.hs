module Base.Core.Quality.Quality
  ( Quality(..)
  ) where

data Quality
 = QMajor
 | QMinor
 | QDominant
 | QDiminished
 | QAugmented
 deriving Show
