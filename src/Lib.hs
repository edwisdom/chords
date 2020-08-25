module Lib
  ( chordToIntervals
  , chordToNotes
  , qualityToIntervals
  , HeliotonicScale
  ) where

import Base.Core.Quality.CQuality as CQ
import Base.Core.Quality.IQuality as IQ

import Base.Chord
import Base.Chord.Chord as C

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Note
import Base.Chord.Sus

import Base.Interval as I
    (Interval
    , intervalFrom
    , (<+>)
    , (<->)
    , jumpIntervalFromNote
    , intervalToDistance
    )
import Scale (Scale(..), BaseMode(..), baseModeIntervals)
import Data.Set(Set(..))
import qualified Data.Set as S
import Data.Map.Strict (Map, insert, fromList, toList, (!), delete, (!?))
import Data.Maybe (fromJust)


chordToNotes :: Chord -> [Note]
chordToNotes chord =
  flip jumpIntervalFromNote (getChordRoot chord) <$> S.toList (chordToIntervals chord)


chordToIntervals :: Chord -> Set Interval
chordToIntervals chord =
  let
    baseScale = highestNaturalToIntervals (getHighestNatural chord) $ qualityToIntervals $ C.getQuality chord
    intervals = susIntervals (extendIntervals baseScale $ getExtensions chord) $ getSus chord
  in
    foldr S.insert S.empty intervals


type HeliotonicScale = Map Int Interval


qualityToIntervals :: CQ.Quality -> HeliotonicScale
qualityToIntervals qual = fromList $ zip [1..7] $ S.toList $ baseModeIntervals $ qualityToScale qual
  where
    qualityToScale :: CQ.Quality -> BaseMode
    qualityToScale CQ.Major = Ionian
    qualityToScale CQ.Minor = Dorian
    qualityToScale CQ.Dominant = Mixolydian
    qualityToScale CQ.Augmented = AugmentedQuality
    qualityToScale CQ.Diminished = DiminishedQuality


susIntervals :: HeliotonicScale -> Sus -> HeliotonicScale
susIntervals scale s
  | isSus s = maybe (delete 3 scale) (\i -> insert i (intervalFrom (baseQuality i) i) $ delete 3 scale) (getMaybeDeg s)
  | otherwise = scale


extendIntervals :: HeliotonicScale -> [Extension] -> HeliotonicScale
extendIntervals = foldr $ flip extendInterval
  where
  extendInterval :: HeliotonicScale -> Extension -> HeliotonicScale
  extendInterval scale ext = insert deg (intervalFrom (baseQuality deg) deg <+> shift) scale
    where
      deg   = degree ext
      shift = sign ext

highestNaturalToIntervals :: HighestNatural -> HeliotonicScale -> HeliotonicScale
highestNaturalToIntervals hn scale =
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
        if even $ getDegree hn then
          [1, 3, 5, deg]
        else
          [1, 3 .. deg]

    getIntervals :: [Int] -> HeliotonicScale -> HeliotonicScale
    getIntervals ints hts = fromList $ map ($ hts) (getInterval <$> ints)

    getInterval :: Int -> HeliotonicScale -> (Int, Interval)
    getInterval int hts =
      let
        interval = hts !? ((int-1) `mod` 7 + 1)
      in
        (int, fromJust interval)

    insertMajorSeven :: HeliotonicScale -> HeliotonicScale
    insertMajorSeven hts = insert 7 (intervalFrom IQ.Major 7) hts

