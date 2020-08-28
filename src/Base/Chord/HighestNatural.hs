{-|
Module      : Base.Chord.HighestNatural
Description : Implements HighestNatural datatype (for chords)
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the HighestNatural datatype and its smart constructors.
-}
module Base.Chord.HighestNatural
  ( HighestNatural
  , isMajor
  , getDegree
  , majorNatural
  , nonMajorNatural
  ) where

-- The highest natural number of a chord is either major or not.
data HighestNatural =
  HighestNatural { isMajor :: Bool
                 , getDegree :: Int
                 }
  deriving(Eq)

-- Show highest naturals by printing an M if major.
instance Show HighestNatural where
  show highNat =
    case (isMajor highNat, getDegree highNat) of
      (True, _) -> "M" ++ show (getDegree highNat)
      (False, 5) -> ""
      (False, _) -> show (getDegree highNat)

-- Create a major variant of the HighestNatural given a degree
majorNatural :: Int -> HighestNatural
majorNatural deg = HighestNatural { isMajor = True, getDegree = deg }

-- Create a nonmajor variant of the HighestNatural given a degree
nonMajorNatural :: Int -> HighestNatural
nonMajorNatural deg = HighestNatural { isMajor = False, getDegree = deg }
