module Lib
  ( chordToIntervals
  , chordToNotes
  ) where

import Base.Core.Quality.CQuality as CQ
import Base.Core.Quality.IQuality as IQ

import Base.Chord

import Base.Chord.Chord as C
import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Root
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

import Control.Monad (foldM)
import Data.Set (Set(..))
import qualified Data.Set as S
import Data.Map.Strict (Map, insert, fromList, toList, (!), delete, (!?))
import Data.Maybe (fromJust)


chordToNotes :: Chord -> Maybe [Root]
chordToNotes chord =
  do chordInts <- chordToIntervals chord
     return $ flip jumpIntervalFromNote (getChordRoot chord) <$> S.toList chordInts


chordToIntervals :: Chord -> Maybe (Set Interval)
chordToIntervals chord =
  do qualInts <- qualityToIntervals $ C.getQuality chord
     baseScale <- highestNaturalToIntervals (getHighestNatural chord) qualInts
     extendedScale <- extendIntervals baseScale $ getExtensions chord
     intervals <- susIntervals extendedScale $ getSus chord
     return $ foldr S.insert S.empty intervals


type HeliotonicScale = Map Int Interval


qualityToIntervals :: CQ.Quality -> Maybe HeliotonicScale
qualityToIntervals qual =
  do bmInts <- baseModeIntervals $ qualityToScale qual
     return $ fromList $ zip [1 .. 7] $ S.toList bmInts
  where
    qualityToScale :: CQ.Quality -> BaseMode
    qualityToScale CQ.Major = Lydian
    qualityToScale CQ.Minor = Dorian
    qualityToScale CQ.Dominant = Mixolydian
    qualityToScale CQ.Augmented = AugmentedQuality
    qualityToScale CQ.Diminished = DiminishedQuality


susIntervals :: HeliotonicScale -> Sus -> Maybe HeliotonicScale
susIntervals scale s = maybe (return scale) addSus (getMaybeDeg s)
  where
    addSus :: Int -> Maybe HeliotonicScale
    addSus i = do int <- intervalFrom (baseQuality i) i
                  return $ insert i int $ delete 3 scale


extendIntervals :: HeliotonicScale -> [Extension] -> Maybe HeliotonicScale
extendIntervals = foldM extendInterval
  where
  extendInterval :: HeliotonicScale -> Extension -> Maybe HeliotonicScale
  extendInterval scale ext =
    do int <- intervalFrom (baseQuality deg) deg
       return $ insert deg (int <+> shift) scale
    -- insert deg (intervalFrom (baseQuality deg) deg <+> shift) scale
    where
      deg   = degree ext
      shift = sign ext

highestNaturalToIntervals :: HighestNatural -> HeliotonicScale -> Maybe HeliotonicScale
highestNaturalToIntervals hn scale =
  let
    scaleInts = getIntervals subset scale
  in
    if isMajor hn then
      insertMajorSeven scaleInts
    else
      return scaleInts
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

    insertMajorSeven :: HeliotonicScale -> Maybe HeliotonicScale
    insertMajorSeven hts =
      do int <- intervalFrom IQ.Major 7
         return $ insert 7 int hts
