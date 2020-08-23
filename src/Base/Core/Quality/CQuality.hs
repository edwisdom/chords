module Base.Core.Quality.CQuality
  ( Quality(..)
  ) where


instance Show Quality where
  show qual =
    case qual of
      Major -> "M"
      Minor -> "m"
      Dominant -> ""
      Diminished -> "dim"
      Augmented -> "aug"


data Quality
  = Major
  | Minor
  | Dominant
  | Diminished
  | Augmented
  deriving(Eq)