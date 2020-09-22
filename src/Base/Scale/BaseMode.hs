{-|
Module      : Base.Scale.BaseMode
Description : Representation of a base mode
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the representation for a base mode
and a function to convert base modes to a set of intervals.
-}
module Base.Scale.BaseMode
  ( BaseMode(..)
  , baseModeIntervals
  ) where

import Base.Core.Interval
import Base.Core.Quality.IQuality

import Data.Maybe (fromJust)
import Data.Set hiding (filter)
import qualified Data.Set as S(filter)

-- | A BaseMode must be one of these names.
data BaseMode
  = Lydian
  | Dorian
  | Mixolydian
  | AugmentedQuality
  | DiminishedQuality
  | Ionian
  | Aeolian
  | Phrygian
  | Locrian
  | MelodicMinor
  | LydianAug
  | LydianDom
  | Altered
  | HarmonicMinor
  | PhrygianDom
  | DoubleHarmonicMinor
  | HarmonicMajor
  | DoubleHarmonicMajor
  | Blues
  | MajorPentatonic
  | MinorPentatonic
  | Bebop
  | WholeTone
  | Hirajoshi
  | HalfWhole
  | WholeHalf
  deriving (Show, Enum, Eq)

-- | Given a base mode, this returns a set of intervals. Whereas
-- some base modes are created from scratch (like Ionian), others
-- (e.g. Dorian, Phrygian, etc.) can be derived by shifting those modes.
baseModeIntervals :: BaseMode -> Set Interval
baseModeIntervals bm
  | fromScratch = zipToIntervalSet bmQualities [1 .. 7]
  | nonHeptatonic = nonHepInts
  | otherwise
  = let (mode, shift) = modeAndShift
    in nthDegreeIntervals (baseModeIntervals mode) shift
  where
    -- Discriminate between BaseModes for which we build the intervals from
    -- scratch and those that are computed from some other interval set
    fromScratch :: Bool
    fromScratch = bm `elem` [ Ionian
                            , AugmentedQuality
                            , DiminishedQuality
                            , MelodicMinor
                            , HarmonicMinor
                            , DoubleHarmonicMinor
                            , HarmonicMajor
                            , DoubleHarmonicMajor
                            ]

    -- Discriminate between BaseModes that have scale degrees 1-7 and those
    -- that do not.
    nonHeptatonic :: Bool
    nonHeptatonic = bm `elem` [ Blues
                              , MajorPentatonic
                              , MinorPentatonic
                              , Bebop
                              , WholeTone
                              , Hirajoshi
                              , HalfWhole
                              , WholeHalf
                              ]

    -- The interval qualities for the modal interval sets built from scratch
    bmQualities :: [Quality]
    bmQualities =
      case bm of
        Ionian ->
          [perfect, major, major, perfect, perfect, major, major]
        AugmentedQuality ->
          [perfect, major, major, fromJust $ augmented 1, fromJust $ augmented 1, major, minor]
        DiminishedQuality ->
          [perfect, major, minor, perfect, fromJust $ diminished 1, minor, fromJust $ diminished 1]
        MelodicMinor ->
          [perfect, major, minor, perfect, perfect, major, major]
        HarmonicMinor ->
          [perfect, major, minor, perfect, perfect, minor, major]
        DoubleHarmonicMinor ->
          [perfect, major, minor, fromJust $ augmented 1, perfect, minor, major]
        HarmonicMajor ->
          [perfect, major, major, perfect, perfect, minor, major]
        DoubleHarmonicMajor ->
          [perfect, minor, major, perfect, perfect, minor, major]

    -- The interval sets for the non-heptatonic modes
    nonHepInts :: Set Interval
    nonHepInts = zipToIntervalSet quals iSizes
      where
        (quals, iSizes) =
          case bm of
            Blues ->
              ([perfect, minor, perfect, fromJust $ diminished 1, perfect, minor],
               [1, 3, 4, 5, 5, 7])
            MajorPentatonic ->
              ([perfect, major, major, perfect, major],
               [1, 2, 3, 5, 6])
            MinorPentatonic ->
              ([perfect, minor, perfect, perfect, minor],
               [1, 3, 4, 5, 7])
            Bebop ->
              ([perfect, major, major, perfect, perfect, major, minor, major],
               [1, 2, 3, 4, 5, 6, 7, 7])
            WholeTone ->
              ([perfect, major, major, fromJust $ diminished 1, minor, minor],
               [1, 2, 3, 5, 6, 7])
            Hirajoshi ->
              ([perfect, major, minor, perfect, minor],
               [1, 2, 3, 5, 6])
            HalfWhole ->
              ([perfect, minor, minor, major, fromJust $ augmented 1, perfect, major, minor],
               [1, 2, 3, 3, 4, 5, 6, 7])
            WholeHalf ->
              ([perfect, major, minor, perfect, fromJust $ diminished 1, minor, major, major],
               [1, 2, 3, 4, 5, 6, 6, 7])

    -- The starting mode and shift for modal interval sets built from other
    -- interval sets
    modeAndShift :: (BaseMode, Int)
    modeAndShift =
      case bm of
        Dorian      -> (Ionian, 2)
        Phrygian    -> (Ionian, 3)
        Lydian      -> (Ionian, 4)
        Mixolydian  -> (Ionian, 5)
        Aeolian     -> (Ionian, 6)
        Locrian     -> (Ionian, 7)
        LydianAug   -> (MelodicMinor, 3)
        LydianDom   -> (MelodicMinor, 4)
        Altered     -> (MelodicMinor, 7)
        PhrygianDom -> (HarmonicMinor, 5)

