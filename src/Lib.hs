module Lib
  ( chordToIntervals
  , chordToNotes
  ) where

import Base.Chord
import Base.Extension
import Base.Quality
import Interval
    ( Interval(..),
      IQuality(..),
      (<+>),
      (<->),
      defaultIQuality,
      jumpIntervalFromNote,
      intervalToDistance )
import Scale (Scale(..), scaleToIntervals)
import Data.Set(Set(..))
import qualified Data.Set as S
import Data.Map.Strict (Map, insert, fromList, toList, (!), delete, (!?))
import Data.Maybe (fromJust)


chordToNotes :: Chord -> [Root]
chordToNotes chord@(Chord root _ _ _ _) =
  flip jumpIntervalFromNote root <$> S.toList (chordToIntervals chord)


chordToIntervals :: Chord -> Set Interval
chordToIntervals (Chord root quality highNat exts sus) =
  let
    baseScale = highestNaturalToIntervals highNat $ qualityToIntervals quality
    intervals = susIntervals (extendIntervals baseScale exts) sus
  in
    foldr S.insert S.empty intervals


type HeliotonicScale = Map Int Interval


qualityToIntervals :: Quality -> HeliotonicScale
qualityToIntervals qual = fromList $ zip [1..7] $ scaleToIntervals $ qualityToScale qual
  where
    qualityToScale :: Quality -> Scale
    qualityToScale QMajor = SLydian
    qualityToScale QMinor = SDorian
    qualityToScale QDominant = SMixolydian
    qualityToScale QAugmented = SAugmentedQuality
    qualityToScale QDiminished = SDiminishedQuality


susIntervals :: HeliotonicScale -> Sus -> HeliotonicScale
susIntervals scale NoSus = scale
susIntervals scale (Sus i) = insert i (Interval (defaultIQuality i) i) $ delete 3 scale


extendIntervals :: HeliotonicScale -> [Extension] -> HeliotonicScale
extendIntervals = foldr $ flip extendInterval
  where
  extendInterval :: HeliotonicScale -> Extension -> HeliotonicScale
  extendInterval scale ext = insert deg (Interval (defaultIQuality deg) deg <+> shift) scale
    where
      deg = degree ext
      shift = extSign ext

highestNaturalToIntervals :: HighestNatural -> HeliotonicScale -> HeliotonicScale
highestNaturalToIntervals (HighestNatural major i) scale =
  insertMajor major $ getIntervals subset scale
  where
    subset =
      if i `mod` 2 == 0 then
        [1, 3, 5, i]
      else
        [1,3..i]
    getIntervals :: [Int] -> HeliotonicScale -> HeliotonicScale
    getIntervals ints hts = fromList $ map ($ hts) (getInterval <$> ints)
    getInterval :: Int -> HeliotonicScale -> (Int, Interval)
    getInterval int hts =
      let interval = hts !? ((int-1) `mod` 7 + 1)
      in
        (int, fromJust $ interval)
    insertMajor :: MajorOrNot -> HeliotonicScale -> HeliotonicScale
    insertMajor Major hts = insert 7 (Interval IMajor 7) hts
    insertMajor notMajor hts = hts
