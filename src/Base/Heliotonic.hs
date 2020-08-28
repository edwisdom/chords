{-|
Module      : Base.Heliotonic
Description : Construction of heliotonic scales from basic components.
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

Create and modify heliotonic scales, which form the basis of turning chords and
other constructs into sets of intervals/sets of notes.
-}

module Base.Heliotonic
  ( HeliotonicScale
  , qualityToIntervals
  , susIntervals
  , extendIntervals
  , highestNaturalToIntervals
  ) where

import qualified Base.Core.Quality.CQuality as CQ
import qualified Base.Core.Quality.IQuality as IQ

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Sus

import Base.Interval

import Scale (BaseMode(..), baseModeIntervals)

import Data.Map.Strict (Map, insert, fromList, toList, (!), delete, (!?))
import Data.Maybe (fromJust)
import qualified Data.Set as S

type HeliotonicScale = Map Int Interval

qualityToIntervals :: CQ.Quality -> HeliotonicScale
qualityToIntervals qual =
  fromList $ zip [1 .. 7] $ S.toList $ baseModeIntervals $ qualityToScale qual
  where
    qualityToScale :: CQ.Quality -> BaseMode
    qualityToScale CQ.Major = Ionian
    qualityToScale CQ.Minor = Dorian
    qualityToScale CQ.Dominant = Mixolydian
    qualityToScale CQ.Augmented = AugmentedQuality
    qualityToScale CQ.Diminished = DiminishedQuality

susIntervals :: HeliotonicScale -> Sus -> HeliotonicScale
susIntervals scale s
  | isSus s = maybe scale' (\i -> insert i (fromJust $ intervalFrom (baseQuality i) i) scale') $ getMaybeDeg s
  | otherwise = scale
  where
    scale' :: HeliotonicScale
    scale' = delete 3 scale


extendIntervals :: HeliotonicScale -> [Extension] -> HeliotonicScale
extendIntervals = foldr $ flip extendInterval
  where
    extendInterval :: HeliotonicScale -> Extension -> HeliotonicScale
    extendInterval scale ext =
      insert deg (fromJust (intervalFrom (baseQuality deg) deg) <+> shift) scale
      where
        deg   = degree ext
        shift = sign ext

highestNaturalToIntervals :: HeliotonicScale -> HighestNatural -> HeliotonicScale
highestNaturalToIntervals scale hn =
  let
    scaleInts = getIntervals subset scale
  in
    if isMajor hn then
      insertMajorSeven scaleInts
    else
      scaleInts
  where
    subset =
      let
        deg = getDegree hn
      in
        if even deg then
          [1, 3, 5, deg]
        else
          [1, 3 .. deg]

    getIntervals :: [Int] -> HeliotonicScale -> HeliotonicScale
    getIntervals ints hts = fromList $ map ($ hts) $ getInterval <$> ints

    getInterval :: Int -> HeliotonicScale -> (Int, Interval)
    getInterval int hts =
      let
        interval = hts !? ((int-1) `mod` 7 + 1)
      in
        (int, fromJust interval)

    insertMajorSeven :: HeliotonicScale -> HeliotonicScale
    insertMajorSeven hts = insert 7 (fromJust $ intervalFrom IQ.Major 7) hts

notesToChord :: [Note] -> [Chord]
notesToChord notes =
  let
    roots :: [Note]
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

        hasInterval :: IQ.Quality -> Int -> Bool
        hasInterval iQual iSize = notesContainIntervalFromNote notes root $ fromJust $ intervalFrom iQual iSize

    qualities :: [CQ.Quality]
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
        qInts :: [Interval]
        qInts = catMaybes $ ($ qualityToIntervals quality) <$> (M.lookup <$> [7, 2, 4, 6])

        cInts :: [Interval]
        cInts = intervalBetweenNotes root <$> roots

        majorOrNot :: Int -> HighestNatural
        majorOrNot = if (quality /= CQ.Major) && fromJust (intervalFrom IQ.Major 7) `elem` cInts
                     then majorNatural
                     else nonMajorNatural

        has :: Int -> Bool
        has int =
          let i = normalizeIntervalSize int
          in i `elem` (getSize <$> cInts) && (getIntWithSize i cInts == getIntWithSize i qInts)

    highNats :: [HighestNatural]
    highNats = uncurry findHighNat <$> zip roots qualities

    findSus :: Note -> HighestNatural -> Sus
    findSus root highNat
      | containsThird || getDegree highNat == 6                = noSus
      | getDegree highNat `elem` [9, 11, 13] || (has2 && has4) = susNoNum
      | has2                                                   = sus 2
      | has4                                                   = sus 4
      | otherwise                                              = noSus
      where
        cInts :: [Interval]
        cInts = intervalBetweenNotes root <$> roots

        containsThird :: Bool
        containsThird = 3 `elem` (getSize <$> cInts)

        has2 :: Bool
        has2 = fromJust (intervalFrom IQ.Major 2) `elem` cInts

        has4 :: Bool
        has4 = fromJust (intervalFrom IQ.Perfect 4) `elem` cInts

    chordSuses :: [Sus]
    chordSuses = uncurry findSus <$> zip roots highNats

    findExts :: Note -> CQ.Quality -> HighestNatural -> Sus -> [Extension]
    findExts root quality highNat chordSus =
      let
        qInts :: [Interval]
        qInts = M.elems $ qualityToIntervals quality

        cInts :: [Interval]
        cInts = intervalBetweenNotes root <$> roots

        numHighNat :: Int
        numHighNat = getDegree highNat

        removableDegs :: [Int]
        removableDegs = map normalizeIntervalSize [1, 3 .. numHighNat] ++ ([6 | numHighNat == 6])

        noNatInts :: [Interval]
        noNatInts = filter (\x -> not $ (x `elem` qInts) && getSize x `elem` removableDegs) cInts

        noSusInts :: [Interval]
        noSusInts
          | chordSus == sus 2 = L.delete (fromJust (intervalFrom IQ.Major 2)) noNatInts
          | chordSus == sus 4 = L.delete (fromJust (intervalFrom IQ.Perfect 4)) noNatInts
          | (chordSus == susNoNum && numHighNat < 9) = L.delete (fromJust (intervalFrom IQ.Major 2))
                                                     $ L.delete (fromJust (intervalFrom IQ.Perfect 4)) noNatInts
          | otherwise = noNatInts

        intToExt :: Interval -> Extension
        intToExt int
          | getSize int == 5 = distToExt (fromJust $ intervalToDistance $ int |-| getIntWithSize (getSize int) qInts) (getSize int)
          | otherwise = distToExt (fromJust $ intervalToDistance $ int |-| getIntWithSize (getSize int) qInts) (getSize int + 7)
      in
        intToExt <$> noSusInts

    extss :: [[Extension]]
    extss = uncurry4 findExts <$> zip4 roots qualities highNats chordSuses
    sortByExtLen :: [Chord] -> [Chord]
    sortByExtLen = sortBy (compare `on` length . getExtensions)
  in
    sortByExtLen $ uncurry5 chordFrom <$> zip5 roots qualities highNats extss chordSuses
