module Base.Scale.BaseMode
  ( BaseMode(..)
  , baseModeIntervals
  ) where

import Base.Core.Interval
import Base.Core.Quality.IQuality

import Data.Maybe (fromJust)
import Data.Set

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
  deriving (Show, Enum, Eq)

baseModeIntervals :: BaseMode -> Set Interval
baseModeIntervals bm = if fromScratch then
                         fromJust $ zipToIntervalSet bmQualities [1 .. 7]
                       else
                         let
                           (mode, shift) = modeAndShift
                         in
                           nthDegreeIntervals (baseModeIntervals mode) shift
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

    -- The interval qualities for the modal interval sets built from scratch
    bmQualities :: [Quality]
    bmQualities =
      case bm of
        Ionian ->
          [Perfect, Major, Major, Perfect, Perfect, Major, Major]
        AugmentedQuality ->
          [Perfect, Major, Major, Augmented 1, Augmented 1, Major, Minor]
        DiminishedQuality ->
          [Perfect, Major, Minor, Perfect, Diminished 1, Minor, Diminished 1]
        MelodicMinor ->
          [Perfect, Major, Minor, Perfect, Perfect, Major, Major]
        HarmonicMinor ->
          [Perfect, Major, Minor, Perfect, Perfect, Minor, Major]
        DoubleHarmonicMinor ->
          [Perfect, Major, Minor, Augmented 1, Perfect, Minor, Major]
        HarmonicMajor ->
          [Perfect, Major, Major, Perfect, Perfect, Minor, Major]
        DoubleHarmonicMajor ->
          [Perfect, Minor, Major, Perfect, Perfect, Minor, Major]

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
