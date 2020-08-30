{-|
Module      : Base.Core.Note
Description : Implements Note datatype and its utility functions
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the Note datatype, its smart constructors and
accessors, and some basic functions on Notes.
-}
module Base.Core.Note
  ( Note
  , getLetter
  , getAcc
  , noteFrom
  , noteToPitchClass
  , respell
  ) where

import Base.Core.Accidental
import Base.Core.Letter
import Base.Core.PitchClass

import Data.Function(on)

-- | A Note consists of a Letter and an Accidental
data Note = Note { getLetter :: Letter
                 , getAcc :: Accidental
                 }

-- | Two Notes are equal if they're enharmonically equivalent
instance Eq Note where
  n1 == n2 = ((==) `on` noteToPitchClass) n1 n2

-- | Smart constructor for notes
noteFrom :: Letter -> Accidental -> Note
noteFrom letter acc = Note { getLetter = letter, getAcc = acc }

-- | Show notes by concatenating the letter and accidental
instance Show Note where
  show (Note letter acc) = show letter ++ show acc

-- Convert notes to a pitch class
noteToPitchClass :: Note -> PitchClass
noteToPitchClass r =
  letterToPitchClass (getLetter r) @+@ impliedShift (getAcc r)

-- | Respell a note to its simplest form, i.e. with the fewest accidentals.
-- If there are multiple equally simple respellings, this will return the one
-- with the same type of accidentals as the original.

-- This function is idempotent.
-- prop> respell note == respell $ respell note
respell :: Note -> Note
respell (Note letter acc) =
  let
    accInt = impliedShift acc
    nextLetterDist = letterToPitchClass (nextNthLetter letter 1) @-@ letterToPitchClass letter
    prevLetterDist = letterToPitchClass letter @-@ letterToPitchClass (prevNthLetter letter 1)
  in
    if accInt > 0 && accInt >= nextLetterDist then
      respell $ Note (nextNthLetter letter 1) (shiftToAcc (accInt - nextLetterDist))
    else if accInt < 0 && -accInt >= prevLetterDist then
      respell $ Note (prevNthLetter letter 1) (shiftToAcc (accInt + prevLetterDist))
    else
      Note letter acc
