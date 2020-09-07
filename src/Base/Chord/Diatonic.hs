{-|
Module      : Base.Chord.Diatonic
Description : Provides functions related to diatonic function
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module exports functions that check for diatonicity
and generate diatonic chords given some basic constraints,
such as the key, degree/function, number of notes, and
interval structure (e.g. triadic, quartal, quintal).
-}
module Base.Chord.Diatonic
  ( diatonicChord
  , isDiatonicTo
  , tonic
  , supertonic
  , mediant
  , subdominant
  , dominant
  , submediant
  , subtonic
  ) where

import Base.Scale.BaseMode
import Base.Scale.Mode
import Base.Scale.Scale
import Base.Chord.Chord
import Base.Core.Note
import Base.Class.Rooted
import Data.Maybe(fromJust)
import Common.Utils
import Data.List(find)

-- | Returns True if a given chord is diatonic to a given scale, i.e.
-- the notes of the chord are all part of the scale.
isDiatonicTo :: Chord -> Scale -> Bool
isDiatonicTo chord scale = all (`elem` scaleToNotes scale) (toNotes chord)

{-| Generates a diatonic chord given a scale and some constraints.

  * Properties :

  1. The resulting chord will be diatonic to the given scale

  2. In the resulting ExpChord(chord, notes), the length of notes equals numNotes

  3. The root of the resulting chord will be equal to (scale !! (degree-1))

  Returns Nothing if constraints are invalid.

  * Invalid Constraints :

  1. The number of notes requested is not between 2 and the scale length

  2. The scale degree is not between 1 and the scale length

  3. The number of notes to skip per jump is not between 1 and (scale length - 1)
-}
diatonicChord :: Scale -- ^ The scale, i.e. a list of notes
              -> Int   -- ^ The number of notes in the chord
              -> Int   -- ^ The scale degree that's the chord root
              -> Int   -- ^ The number of notes to skip ahead from the root on each jump
              -> Maybe Chord -- ^ The resulting chord name and list of notes, or Nothing
diatonicChord scale@(Scale note mode) numNotes degree jumpSize =
  let
    notes = scaleToNotes scale
    sLength = length notes
    indices = map (\x -> x `mod` length notes) [degree-1, (degree-1) + jumpSize.. (degree-1) + (jumpSize * (numNotes-1))]
    chordTones = getIndices indices notes
  in
    if numNotes < 2 || numNotes > sLength
    || degree   < 1 || degree   > sLength
    || jumpSize < 1 || jumpSize > (sLength - 1)
    then
      Nothing
    else
      Just $ fromJust (find (\ c -> root c == notes !! (degree - 1)) (notesToChord chordTones))

-- | Given a degree, returns a function that creates a diatonic triadic major chord
-- from a key (i.e. a Note) and the number of notes.
getMajorFuncChord :: Int -> Note -> Int -> Maybe Chord
getMajorFuncChord deg key numNotes = diatonicChord (Scale key (modeFrom Ionian [])) numNotes deg 2

-- | Given a key and a number of notes, this returns a triadic I chord.
-- If number of notes is not between 2 and 7, this returns Nothing.
tonic :: Note -> Int -> Maybe Chord
tonic = getMajorFuncChord 1

-- | Given a key and a number of notes, this returns a triadic IIm chord.
-- If number of notes is not between 2 and 7, this returns Nothing.
supertonic :: Note -> Int -> Maybe Chord
supertonic = getMajorFuncChord 2

-- | Given a key and a number of notes, this returns a triadic IIIm chord.
-- If number of notes is not between 2 and 7, this returns Nothing.
mediant :: Note -> Int -> Maybe Chord
mediant = getMajorFuncChord 3

-- | Given a key and a number of notes, this returns a triadic IV chord.
-- If number of notes is not between 2 and 7, this returns Nothing.
subdominant :: Note -> Int -> Maybe Chord
subdominant = getMajorFuncChord 4

-- | Given a key and a number of notes, this returns a triadic V chord.
-- If number of notes is not between 2 and 7, this returns Nothing.
dominant :: Note -> Int -> Maybe Chord
dominant = getMajorFuncChord 5

-- | Given a key and a number of notes, this returns a triadic VIm chord.
-- If number of notes is not between 2 and 7, this returns Nothing.
submediant :: Note -> Int -> Maybe Chord
submediant = getMajorFuncChord 6

-- | Given a key and a number of notes, this returns a triadic VII0 chord.
-- If number of notes is not between 2 and 7, this returns Nothing.
subtonic :: Note -> Int -> Maybe Chord
subtonic = getMajorFuncChord 7
