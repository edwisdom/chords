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
  , canonicalizeQuality
  ) where

import Base.Chord.HighestNatural

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

-- | Given the user-provided quality (which may be Nothing) and the
-- HighestNatural, this infers the quality of the chord and returns it.
canonicalizeQuality :: Maybe Quality -> HighestNatural -> Quality
canonicalizeQuality Nothing  hn = if getDegree hn < 7 then Major else Dominant
canonicalizeQuality (Just q) _  = q
