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
import Base.Chord.Note
import Base.Chord.Sus

import Base.Interval

import Base.Scale (BaseMode(..), baseModeIntervals)

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
  | isSus s = maybe scale' (\i -> insert i (fromJust $ intervalFrom (IQ.baseQuality i) i) scale') $ getMaybeDeg s
  | otherwise = scale
  where
    scale' :: HeliotonicScale
    scale' = delete 3 scale


extendIntervals :: HeliotonicScale -> [Extension] -> HeliotonicScale
extendIntervals = foldr $ flip extendInterval
  where
    extendInterval :: HeliotonicScale -> Extension -> HeliotonicScale
    extendInterval scale ext =
      insert deg (fromJust (intervalFrom (IQ.baseQuality deg) deg) <+> shift) scale
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
