{-|
Module      : Base.Core.Quality.CQuality
Description : Implements the chord quality datatype
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the (chord) Quality datatype.
-}
module Base.Core.Quality.CQuality
  ( Quality(..)
  ) where

-- | This function implements certain reasonable defaults
-- for showing chord qualities. Other options are possible,
-- such as ^ for Major and - for Minor but while the parser
-- can read these, we don't generally use them to show chord qualities.
instance Show Quality where
  show qual =
    case qual of
      Major -> "M"
      Minor -> "m"
      Dominant -> ""
      Diminished -> "dim"
      Augmented -> "aug"

-- | Chord qualities can be one of:
-- 1. Major
-- 2. Minor
-- 3. Dominant
-- 4. Diminished
-- 5. Augmented
data Quality
  = Major
  | Minor
  | Dominant
  | Diminished
  | Augmented
  deriving(Eq)