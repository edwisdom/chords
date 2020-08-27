module ChordSubs
  ( remove5
  , extend1
  , negativeNote
  , negative
  , dimFamilySub
  , diatonicFuncSub
  , parallelSub
  , alteredDominantSub
  ) where



import Base.Chord
import Base.Chord.Chord as C
import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Note
import Base.Chord.Sus

import Base.Core.Quality.CQuality as CQ
import Base.Core.Quality.IQuality as IQ

import Base.Interval hiding (getQuality)
import qualified Base.Interval as I(getQuality)

import Data.List(sortBy, delete, zip4, zip5, find, elemIndex)
import Lib(chordToIntervals, chordToNotes, qualityToIntervals, notesToChord)
import Data.Set(Set(..), toList, member, isSubsetOf, fromList)
import qualified Data.Set as S (delete)
import Data.Maybe(fromJust, catMaybes, isJust)
import Base.PitchClass(pitchClass)
import qualified Data.Map.Strict as M (lookup, elems)
import Scale
import Diatonic



remove5 :: ExpChord -> ExpChord
remove5 (chord, _) = (chord, toNotes (getChordRoot chord) (S.delete (fromJust(intervalFrom Perfect 5)) (chordToIntervals chord)))
  where
    toNotes :: Note -> Set Interval -> [Note]
    toNotes root intSet = flip jumpIntervalFromNote root <$> toList intSet


extend1 :: ExpChord -> ExpChord
extend1 (chord, _) =  (newChord, chordToNotes newChord)
  where
    newChord = chordFrom (getChordRoot chord) (C.getQuality chord)
               (extendHighestNat (getHighestNatural chord))
               (getExtensions chord) (getSus chord)
    extendHighestNat :: HighestNatural -> HighestNatural
    extendHighestNat highNat =
      (if isMajor highNat then majorNatural else nonMajorNatural) $ getDegree highNat + 2

negativeNote :: Note -> Note -> Note
negativeNote key note =
  let
    origInt = intervalBetweenNotes key note
    newNum = normalizeIntervalSize $ 6 - getSize origInt
    newDist = 7 - fromJust (intervalToDistance origInt) `mod` 12
    baseInt = fromJust $ intervalFrom (baseQuality newNum) newNum
    newInt = baseInt <+> (newDist - fromJust (intervalToDistance baseInt))
  in
    jumpIntervalFromNote newInt key


negative :: Note -> ExpChord -> ExpChord
negative key (chord, notes) = (newChord, newNotes)
  where
    newNotes = negativeNote key <$> notes
    newChord = head (notesToChord newNotes)


transposeExpChord :: ExpChord -> IQ.Quality -> Int -> ExpChord
transposeExpChord (chord, notes) iQual i =
  let
    newChord = transposeToRoot chord $ respell $ jumpIntervalFromNote (fromJust (intervalFrom iQual i)) $ getChordRoot chord
  in
    (newChord, chordToNotes newChord)

dimFamilySub :: ExpChord -> [ExpChord]
dimFamilySub eChord@(chord, notes)
  |  getQuality chord == CQ.Dominant ||
    (getQuality chord == CQ.Minor && (fromJust (intervalFrom IQ.Major 6) `member` chordToIntervals chord))
    = [transposeExpChord eChord IQ.Minor 3, transposeExpChord eChord (IQ.Diminished 1) 5, transposeExpChord eChord IQ.Major 6]
  | otherwise = []


tritoneSub :: ExpChord -> ExpChord
tritoneSub eChord = transposeExpChord eChord (IQ.Augmented 1) 4

diatonicFuncSub :: Note -> ExpChord -> [ExpChord]
diatonicFuncSub key (chord, notes)
  | validSub && degree == 1 = [mediant key numNotes, submediant key numNotes]
  | validSub && degree == 2 = [subdominant key numNotes]
  | validSub && degree == 3 = [tonic key numNotes, submediant key numNotes]
  | validSub && degree == 4 = [supertonic key numNotes]
  | validSub && degree == 5 = [subtonic key numNotes]
  | validSub && degree == 6 = [tonic key numNotes, mediant key numNotes]
  | validSub && degree == 7 = [dominant key numNotes]
  where
    validSub = chord `isDiatonicTo` major key
    degree = 1 + fromJust (elemIndex (getChordRoot chord) (scaleToNotes (major key)))
    numNotes = length notes


parallelSub :: ExpChord -> [ExpChord]
parallelSub (chord, notes) =
  let
    flippedChord
      | getQuality chord == CQ.Major = Just (chord {getQuality = CQ.Minor})
      | getQuality chord == CQ.Minor = Just (chord {getQuality = CQ.Major})
      | otherwise = Nothing
  in
    ([(fromJust flippedChord, chordToNotes (fromJust flippedChord)) | isJust flippedChord])

alteredDominantSub :: ExpChord -> [ExpChord]
alteredDominantSub (chord, notes) = zip chords (chordToNotes <$> chords)
  where
    chords = delete chord
      [ chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 7) [flat 9] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 7) [sharp 9] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 7) [flat 5] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 7) [sharp 9] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 7) [flat 9, flat 5] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 7) [sharp 9, sharp 5] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 7) [flat 13, flat 9] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 7) [sharp 9, sharp 11] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 9) [flat 5] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 9) [sharp 5] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 13) [flat 9] noSus
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 13) [] susNoNum
      , chordFrom (getChordRoot chord) CQ.Dominant (nonMajorNatural 7) [flat 9] (sus 4)
      ]
