{-|
Module      : Base.PitchClass
Description : Abstract representation of a pitch class
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the PitchClass datatype, its smart constructor,
and functions to compute with pitch classes.
-}
module Base.PitchClass
  ( pitchClass
  , PitchClass
  , letterToPitchClass
  , getPitchClass
  , (@-@)
  , (@+@)
  ) where

import Base.Core.Letter
import Base.Core.Accidental


-- | A pitch class is just an integer mod 12.
newtype PitchClass = PitchClass { getPitchClass :: Int }
  deriving (Show, Eq, Ord)

-- | This smart constructor ensures that values are [0,11].
pitchClass :: Int -> PitchClass
pitchClass i = PitchClass $ i `mod` 12

-- | Given two pitch classes, this computes the difference in semitones
-- between them and returns an integer.
infixl 6 @-@
(@-@) :: PitchClass -> PitchClass -> Int
(PitchClass p1) @-@ (PitchClass p2) = (p1 - p2) `mod` 12

-- | Given an integer and a pitch class, this shifts the pitch
-- class up by that many semitones and returns a new pitch class.
infixl 6 @+@
(@+@) :: PitchClass -> Int -> PitchClass
(PitchClass p) @+@ i = pitchClass $ p + i


-- | Convert a letter to a pitch class. The default is set for C to be 0
-- and to work upwards from there.
letterToPitchClass :: Letter -> PitchClass
letterToPitchClass C = pitchClass 0
letterToPitchClass D = pitchClass 2
letterToPitchClass E = pitchClass 4
letterToPitchClass F = pitchClass 5
letterToPitchClass G = pitchClass 7
letterToPitchClass A = pitchClass 9
letterToPitchClass B = pitchClass 11



