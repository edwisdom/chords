module ChordSubs
  ( remove5
  , extend1
  , negativeNote
  , notesToChord
  , negative
  , dimFamilySub
  , diatonicChord
  , isDiatonicTo
  , tonic
  , supertonic
  , mediant
  , subdominant
  , dominant
  , submediant
  , subtonic
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
import Data.Function(on)
import Lib(chordToIntervals, chordToNotes, qualityToIntervals)
import Data.Set(Set(..), toList, member, isSubsetOf, fromList)
import qualified Data.Set as S (delete)
import Data.Maybe(fromJust, catMaybes, isJust)
import Base.PitchClass(pitchClass)
import qualified Data.Map.Strict as M (lookup, elems)
import Common.Utils
import Scale


type ExpChord = (Chord, [Note])


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

notesToChord :: [Note] -> [Chord]
notesToChord notes =
  let
    roots = sortBy (compare `on` noteToPitchClass) notes
    findQuality :: Note -> CQ.Quality
    findQuality root
      | hasInterval IQ.Major 3 && hasInterval IQ.Minor 7 = CQ.Dominant
      | hasInterval IQ.Major 3 && hasInterval (IQ.Augmented 1) 5 = CQ.Augmented
      | hasInterval IQ.Major 3 = CQ.Major
      | hasInterval IQ.Minor 3 && hasInterval IQ.Minor 7 = CQ.Minor
      | hasInterval IQ.Minor 3 && hasInterval (IQ.Diminished 1) 5 = CQ.Diminished
      | hasInterval IQ.Minor 3 = CQ.Minor
      | hasInterval IQ.Minor 7 = CQ.Dominant
      | otherwise = CQ.Major
      where
        notesContainIntervalFromNote :: [Note] -> Note -> Interval -> Bool
        notesContainIntervalFromNote notes key interval =
          jumpIntervalFromNote interval key `elem` notes
        hasInterval iQual iSize = notesContainIntervalFromNote notes root $ fromJust (intervalFrom iQual iSize)
    qualities = findQuality <$> roots
    findHighNat :: Note -> CQ.Quality -> HighestNatural
    findHighNat root quality
      |      has 7  &&     (has 9  ||      has 11) &&       has 13 = majorOrNot 13
      |      has 7  &&      has 9  &&      has 11  && not (has 13) = majorOrNot 11
      |      has 7  &&      has 9  && not (has 11) && not (has 13) = majorOrNot 9
      |      has 7                                                 = majorOrNot 7
      | not (has 7) && not (has 9) && not (has 11) &&       has 13 = majorOrNot 6
      |                                                  otherwise = majorOrNot 5
      where
        qInts = catMaybes $ ($ qualityToIntervals quality) <$> (M.lookup <$> [7, 2, 4, 6])
        cInts = intervalBetweenNotes root <$> roots
        majorOrNot = if (quality /= CQ.Major) && fromJust (intervalFrom IQ.Major 7) `elem` cInts
                     then majorNatural
                     else nonMajorNatural
        has :: Int -> Bool
        has int =
          let i = normalizeIntervalSize int
          in i `elem` (getSize <$> cInts) && (getIntWithSize i cInts == getIntWithSize i qInts)
    highNats = uncurry findHighNat <$> zip roots qualities
    findSus :: Note -> HighestNatural -> Sus
    findSus root highNat
      | containsThird || getDegree highNat == 6                = noSus
      | getDegree highNat `elem` [9, 11, 13] || (has2 && has4) = susNoNum
      | has2                                                   = sus 2
      | has4                                                   = sus 4
      | otherwise                                              = noSus
      where
        cInts = intervalBetweenNotes root <$> roots
        containsThird = 3 `elem` (getSize <$> cInts)
        has2 = fromJust (intervalFrom IQ.Major 2) `elem` cInts
        has4 = fromJust (intervalFrom IQ.Perfect 4) `elem` cInts
    chordSuses = uncurry findSus <$> zip roots highNats
    findExts :: Note -> CQ.Quality -> HighestNatural -> Sus -> [Extension]
    findExts root quality highNat chordSus =
      let
        qInts = M.elems $ qualityToIntervals quality
        cInts = intervalBetweenNotes root <$> roots
        numHighNat = getDegree highNat
        removableDegs = map normalizeIntervalSize [1, 3 .. numHighNat] ++ ([6 | numHighNat == 6])
        noNatInts = filter (\x -> not $ (x `elem` qInts) && getSize x `elem` removableDegs) cInts
        noSusInts
          | chordSus == sus 2 = delete (fromJust (intervalFrom IQ.Major 2)) noNatInts
          | chordSus == sus 4 = delete (fromJust (intervalFrom IQ.Perfect 4)) noNatInts
          | (chordSus == susNoNum && numHighNat < 9) = delete (fromJust (intervalFrom IQ.Major 2))
                                                     $ delete (fromJust (intervalFrom IQ.Perfect 4)) noNatInts
          | otherwise = noNatInts
        intToExt :: Interval -> Extension
        intToExt int
          | getSize int == 5 = distToExt (fromJust $ intervalToDistance $ int |-| getIntWithSize (getSize int) qInts) (getSize int)
          | otherwise = distToExt (fromJust $ intervalToDistance $ int |-| getIntWithSize (getSize int) qInts) (getSize int + 7)
      in
        intToExt <$> noSusInts
    exts = uncurry4 findExts <$> zip4 roots qualities highNats chordSuses
    sortByExtLen :: [Chord] -> [Chord]
    sortByExtLen = sortBy (compare `on` length . getExtensions)
  in
    sortByExtLen $ uncurry5 chordFrom <$> zip5 roots qualities highNats exts chordSuses



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


isDiatonicTo :: Chord -> Scale -> Bool
isDiatonicTo chord scale = all (`elem` scaleToNotes scale) (chordToNotes chord)

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
