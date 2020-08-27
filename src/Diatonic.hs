module Diatonic
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

import Scale
import Lib
import Base.Chord.Chord
import Base.Chord.Note
import Data.Maybe(fromJust)
import Common.Utils
import Data.List(find)

isDiatonicTo :: Chord -> Scale -> Bool
isDiatonicTo chord scale = all (`elem` scaleToNotes scale) (chordToNotes chord)

diatonicChord :: Scale -> Int -> Int -> Int -> ExpChord
diatonicChord scale@(Scale note mode) numNotes degree jumpSize =
  let
    notes = scaleToNotes scale
    indices = map (\x -> x `mod` length notes) [degree-1, (degree-1) + jumpSize.. (degree-1) + (jumpSize * (numNotes-1))]
    chordTones = getIndices indices notes
  in
    (fromJust (find (\ c -> getChordRoot c == notes !! (degree - 1)) (notesToChord chordTones)), chordTones)

tonic :: Note -> Int -> ExpChord
tonic key numNotes =
  diatonicChord (Scale key (Mode Ionian [])) numNotes 1 2

supertonic :: Note -> Int -> ExpChord
supertonic key numNotes =
  diatonicChord (Scale key (Mode Ionian [])) numNotes 2 2

mediant :: Note -> Int -> ExpChord
mediant key numNotes =
  diatonicChord (Scale key (Mode Ionian [])) numNotes 3 2

subdominant :: Note -> Int -> ExpChord
subdominant key numNotes =
  diatonicChord (Scale key (Mode Ionian [])) numNotes 4 2

dominant :: Note -> Int -> ExpChord
dominant key numNotes =
  diatonicChord (Scale key (Mode Ionian [])) numNotes 5 2

submediant :: Note -> Int -> ExpChord
submediant key numNotes =
  diatonicChord (Scale key (Mode Ionian [])) numNotes 6 2

subtonic :: Note -> Int -> ExpChord
subtonic key numNotes =
  diatonicChord (Scale key (Mode Ionian [])) numNotes 7 2

