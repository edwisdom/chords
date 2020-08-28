{-|
Module      : ChordSubs
Description : Provides functions to perform common chord substitutions
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module exports functions that, when given a chord (and
sometimes other information such as key or interval), return
possible chord substitutions.
-}
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



import Base.ChordSymbol
import Base.Chord.Chord as C
import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Note
import Base.Chord.Sus

import Base.Class.Chordal
import Base.Class.Rooted

import Base.Core.Quality.CQuality as CQ
import Base.Core.Quality.IQuality as IQ

import Base.Interval hiding (getQuality)
import qualified Base.Interval as I(getQuality)

import Common.Utils (uncurry3)

import Data.List(sortBy, delete, zip4, zip5, find, elemIndex)
import Data.Set(Set(..), toList, member, isSubsetOf, fromList)
import qualified Data.Set as S (delete)
import Data.Maybe(fromJust, catMaybes, isJust)
import Base.PitchClass(pitchClass)
import qualified Data.Map.Strict as M (lookup, elems)
import Scale
import Diatonic
import Language.Parser

-- | Removes the P5 interval from a given chord's note if it exists
-- and returns the same chord symbol.
--
-- prop> remove5 (chord, _) == chord
--
-- >>> c = canonicalizeChord $ fromJust $ parseChord "CM7"
-- >>> notes = chordToNotes c
-- >>> remove5 (c, notes)
-- (CM7,[C,E,B])
remove5 :: Chord -> Chord
remove5 chord = updateNotes chord $ toNotes (root chord) $ S.delete (fromJust(intervalFrom Perfect 5)) (toIntervals chord)
  where
    toNotes :: Note -> Set Interval -> [Note]
    toNotes root intSet = flip jumpIntervalFromNote root <$> toList intSet

-- | Extends a chord triadically (e.g. CM7 becomes CM9). If it's a
-- 6 chord, then it's extended to a 7add13 chord. If it's already a
-- 13 chord, then this function returns Nothing.
extend1 :: Chord -> Maybe Chord
extend1 chord
  | isJust newChord = Just $ updateNotes (fromJust newChord) (toNotes $ fromJust newChord)
  | otherwise = Nothing
  where
    newChord :: Maybe Chord
    newChord
      | getDegree (highestNatural chord) == 6
        = Just (chordFrom (root chord) (quality chord)
                (nonMajorNatural 7) (extensions chord ++ [add 13]) (suspension chord))
      | getDegree (highestNatural chord) == 13 = Nothing
      | otherwise
        = Just (chordFrom (root chord) (quality chord)
          (extendHighestNat (highestNatural chord)) (extensions chord) (suspension chord))

    extendHighestNat :: HighestNatural -> HighestNatural
    extendHighestNat highNat =
      (if isMajor highNat then majorNatural else nonMajorNatural) $ getDegree highNat + 2

-- | Given a key and another note, this function flips the note over
-- the axis of the key, as done in Ernst Levy's idea of negative harmony.
--
-- Flipping a note twice over the same key should return the same note:
-- prop> negativeNote key (negativeNote key note) == note
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

-- | Given a key and a chord, this function flips the chord over
-- the axis of the key, as done in Ernst Levy's idea of negative harmony.
--
-- Like with a single note, flipping a chord twice should give the same
-- set of notes back.
-- prop> negative key (negative key (_, notes)) == notes
negative :: Note -> Chord -> Chord
negative key chord = head $ notesToChord newNotes
  where
    newNotes = negativeNote key <$> toNotes chord

-- | Given a chord, an interval quality, and an interval size,
-- this transposes the chord by that interval. Because this function
-- is only used internally by the module, it assumes the intervals are
-- validly constructed.
-- TODO: Make this a safe, general-purpose, exportable function
transposeChord :: Chord -> IQ.Quality -> Int -> Chord
transposeChord chord iQual i =
  let
    newChord = transposeToRoot (getSymbol chord )$ respell $ jumpIntervalFromNote (fromJust (intervalFrom iQual i)) $ root chord
  in
    chordFromSymbol newChord

-- | Given a dominant chord or a minor 6 chord, this returns
-- a list of chord substitutions from its diminished family.
--
-- >>> c = canonicalizeChord $ fromJust $ parseChord "C7"
-- >>> notes = chordToNotes c
-- >>> dimFamilySub (c, notes)
-- [(Eb7,[Eb,G,Bb,Db]),(Gb7,[Gb,Bb,Db,Fb]),(A7,[A,C#,E,G])]
dimFamilySub :: Chord -> [Chord]
dimFamilySub eChord
  |  quality eChord == CQ.Dominant ||
    (quality eChord == CQ.Minor && (fromJust (intervalFrom IQ.Major 6) `member` toIntervals eChord))
    = [transposeChord eChord IQ.Minor 3, transposeChord eChord (IQ.Diminished 1) 5, transposeChord eChord IQ.Major 6]
  | otherwise = []

-- | Returns the same chord, but a tritone away.
-- If applied twice in a row, returns the same chord:
-- prop> tritoneSub $ tritoneSub (chord, notes) == (_, notes)
tritoneSub :: Chord -> Chord
tritoneSub eChord = transposeChord eChord (IQ.Augmented 1) 4

-- | Given a key, and a chord in the key, this returns
-- a list of chords that are diatonic functional substitutes.
--
-- If the chord is not diatonic to the key, then this returns [].
diatonicFuncSub :: Note -> Chord -> [Chord]
diatonicFuncSub key chord
  | validSub && degree == 1 = [fromJust (mediant key numNotes), fromJust (submediant key numNotes)]
  | validSub && degree == 2 = [fromJust (subdominant key numNotes)]
  | validSub && degree == 3 = [fromJust (tonic key numNotes), fromJust (submediant key numNotes)]
  | validSub && degree == 4 = [fromJust (supertonic key numNotes)]
  | validSub && degree == 5 = [fromJust (subtonic key numNotes)]
  | validSub && degree == 6 = [fromJust (tonic key numNotes), fromJust (mediant key numNotes)]
  | validSub && degree == 7 = [fromJust (dominant key numNotes)]
  where
    validSub :: Bool
    validSub = chord `isDiatonicTo` major key

    degree :: Int
    degree = 1 + fromJust (elemIndex (root chord) (scaleToNotes (major key)))

    numNotes :: Int
    numNotes = length $ toNotes chord

-- | Given a major quality chord, this returns a minor chord, and vice versa.
-- If the quality is neither, then this returns Nothing.
parallelSub :: Chord -> Maybe Chord
parallelSub chord = flippedChord
  where
    flippedChord :: Maybe Chord
    flippedChord
      | quality chord == CQ.Major =
        Just $ chordFrom (root chord) CQ.Minor (highestNatural chord) (extensions chord) (suspension chord)
      | quality chord == CQ.Minor =
        Just $ chordFrom (root chord) CQ.Major (highestNatural chord) (extensions chord) (suspension chord)
      | otherwise = Nothing

{-| Given a dominant chord, this returns a list of common
altered dominant substitutions. If the chord is not dominant,
this returns an empty list.

* List of Common Altered Dominants
1. 7b9
2. 7#9
3. 7b5
4. 7#5
5. 7b9b5
6. 7#9#5
7. 7b13b9
8. 7#11#9
9. 9b5
10. 9#5
11. 13b9
12. 13sus
13. 7b9sus4
-}
alteredDominantSub :: Chord -> [Chord]
alteredDominantSub chord
  | quality chord == CQ.Dominant = chords
  | otherwise = []
  where
    chords = delete chord $ map (uncurry3 $ chordFrom (root chord) CQ.Dominant)
      [ (nonMajorNatural 7, [flat 9], noSus)
      , (nonMajorNatural 7, [sharp 9], noSus)
      , (nonMajorNatural 7, [flat 5], noSus)
      , (nonMajorNatural 7, [sharp 5], noSus)
      , (nonMajorNatural 7, [flat 9, flat 5], noSus)
      , (nonMajorNatural 7, [sharp 9, sharp 5], noSus)
      , (nonMajorNatural 7, [flat 13, flat 9], noSus)
      , (nonMajorNatural 7, [sharp 9, sharp 11], noSus)
      , (nonMajorNatural 9, [flat 5], noSus)
      , (nonMajorNatural 9, [sharp 5], noSus)
      , (nonMajorNatural 13, [flat 9], noSus)
      , (nonMajorNatural 13, [], susNoNum)
      , (nonMajorNatural 7, [flat 9], sus 4)
      ]
