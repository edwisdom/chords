module ChordSubs
  ( remove5
  , extend1
  , negativeNote
  , notesToChord
  , negative
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

import Data.List(sortBy, delete, zip4, zip5)
import Data.Function(on)
import Lib(chordToIntervals, chordToNotes, qualityToIntervals)
import Data.Set(Set(..), toList)
import qualified Data.Set as S (delete)
import Data.Maybe(fromJust, catMaybes)
import Base.PitchClass(pitchClass)
import qualified Data.Map.Strict as M (lookup, elems)
import Common.Utils


type ExpChord = (Chord, [Note])


remove5 :: ExpChord -> ExpChord
remove5 (chord, _) = (chord, (toNotes (getChordRoot chord) (S.delete (intervalFrom Perfect 5) (chordToIntervals chord))))
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
      if isMajor highNat then
        majorNatural (getDegree highNat + 2)
      else
        nonMajorNatural (getDegree highNat + 2)

negativeNote :: Note -> Note -> Note
negativeNote key note =
  let
    origInt = intervalBetweenNotes key note
    newNum = normalizeIntervalSize $ 6 - getSize origInt
    newDist = (7 - (fromJust $ intervalToDistance origInt)) `mod` 12
    baseInt = intervalFrom (baseQuality newNum) newNum
    newInt = baseInt <+> (newDist - (fromJust (intervalToDistance baseInt)))
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
      | hasInterval IQ.Minor 3 && hasInterval (IQ.Diminished 1) 5 = CQ.Diminished
      | hasInterval IQ.Minor 3 = CQ.Minor
      | hasInterval IQ.Minor 7 = CQ.Dominant
      | otherwise = CQ.Major
      where
        notesContainIntervalFromNote :: [Note] -> Note -> Interval -> Bool
        notesContainIntervalFromNote notes key interval =
          (jumpIntervalFromNote interval key) `elem` notes
        hasInterval iQual iSize = notesContainIntervalFromNote notes root (intervalFrom iQual iSize)
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
        qInts = catMaybes $ ($ (qualityToIntervals quality)) <$> (M.lookup <$> [7, 2, 4, 6])
        cInts = intervalBetweenNotes root <$> roots
        majorOrNot = if (quality /= CQ.Major) && (intervalFrom IQ.Major 7) `elem` cInts
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
        has2 = (intervalFrom IQ.Major 2) `elem` cInts
        has4 = (intervalFrom IQ.Perfect 4) `elem` cInts
    chordSuses = uncurry findSus <$> zip roots highNats
    findExts :: Note -> CQ.Quality -> HighestNatural -> Sus -> [Extension]
    findExts root quality highNat chordSus =
      let
        qInts = M.elems $ qualityToIntervals quality
        cInts = intervalBetweenNotes root <$> roots
        numHighNat = getDegree highNat
        removableDegs = [1,3..numHighNat] ++ (if numHighNat == 6 then [6] else [])
        noNatInts = filter (\x -> not $ (x `elem` qInts) && (getSize x) `elem` removableDegs) cInts
        noSusInts = if chordSus == sus 2 then delete (intervalFrom IQ.Major 2) noNatInts
                    else if chordSus == sus 4 then delete (intervalFrom IQ.Perfect 4) noNatInts
                    else if (chordSus == susNoNum && numHighNat < 9)
                         then delete (intervalFrom IQ.Major 2) $ delete (intervalFrom IQ.Perfect 4) noNatInts
                    else noNatInts
        intToExt :: Interval -> Extension
        intToExt int = distToExt (fromJust $ intervalToDistance $ int |-| (getIntWithSize (getSize int) qInts)) (getSize int + 7)
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
    newChord = (notesToChord newNotes) !! 0

-- dimFamilySub :: ExpChord -> [ExpChord]
-- dimFamilySub (chord, notes)
--   |  getQuality chord == CQ.Dominant ||
--     (getQuality chord == CQ.Minor && (intervalFrom IQ.Major 6) `member` (chordToIntervals chord))
--     =