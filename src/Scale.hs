module Scale
  ( Scale(..)
  , BaseMode(..)
  , baseModeIntervals
  ) where

import Base.Core.Quality.IQuality
import Base.Chord.Root
import Base.Chord.Extension
import Base.Interval (Interval, intervalFrom, (|+|), (|-|))
import Base.Core.Accidental
import Data.List (sort)
import Data.Set(Set(..), fromList, toAscList)


data Scale = Root Mode


data Mode = BaseMode [ScaleExt]


data ScaleExt = Accidental Int


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
  deriving Show

nthDegreeIntervals :: Set Interval -> Int -> Set Interval
nthDegreeIntervals ints n = fromList $ (|-| rootInterval) <$> toAscList ints
  where
    rootInterval = toAscList ints !! (n - 1)


zipToIntervalSet :: [Quality] -> [Int] -> Set Interval
zipToIntervalSet qualities ints = fromList $ uncurry intervalFrom <$> zip qualities ints


baseModeIntervals :: BaseMode -> Set Interval
baseModeIntervals Ionian =
  zipToIntervalSet
  [Perfect, Major, Major, Perfect, Perfect, Major, Major] [1..7]
baseModeIntervals Dorian =
  nthDegreeIntervals (baseModeIntervals Ionian) 2
baseModeIntervals Phrygian =
  nthDegreeIntervals (baseModeIntervals Ionian) 3
baseModeIntervals Lydian =
  nthDegreeIntervals (baseModeIntervals Ionian) 4
baseModeIntervals Mixolydian =
  nthDegreeIntervals (baseModeIntervals Ionian) 5
baseModeIntervals Aeolian =
  nthDegreeIntervals (baseModeIntervals Ionian) 6
baseModeIntervals Locrian =
  nthDegreeIntervals (baseModeIntervals Ionian) 7
baseModeIntervals AugmentedQuality =
  zipToIntervalSet
  [Perfect, Major, Major, Augmented 1, Augmented 1, Major, Minor] [1..7]
baseModeIntervals DiminishedQuality =
  zipToIntervalSet
  [Perfect, Major, Minor, Perfect, Diminished 1, Minor, Diminished 1] [1..7]
baseModeIntervals MelodicMinor =
  zipToIntervalSet
  [Perfect, Major, Minor, Perfect, Perfect, Major, Major] [1..7]
baseModeIntervals LydianAug =
  nthDegreeIntervals (baseModeIntervals MelodicMinor) 3
baseModeIntervals LydianDom =
  nthDegreeIntervals (baseModeIntervals MelodicMinor) 4
baseModeIntervals Altered =
  nthDegreeIntervals (baseModeIntervals MelodicMinor) 7
baseModeIntervals HarmonicMinor =
  zipToIntervalSet
  [Perfect, Major, Minor, Perfect, Perfect, Minor, Major] [1..7]
baseModeIntervals PhrygianDom =
  nthDegreeIntervals (baseModeIntervals HarmonicMinor) 5
baseModeIntervals DoubleHarmonicMinor =
  zipToIntervalSet
  [Perfect, Major, Minor, Augmented 1, Perfect, Minor, Major] [1..7]
baseModeIntervals HarmonicMajor =
  zipToIntervalSet
  [Perfect, Major, Major, Perfect, Perfect, Minor, Major] [1..7]
baseModeIntervals DoubleHarmonicMajor =
  zipToIntervalSet
  [Perfect, Minor, Major, Perfect, Perfect, Minor, Major] [1..7]


-- modeToIntervals :: Mode -> Set Interval
-- modeToIntervals baseMode exts = 
--   where 
--     baseIntervals = baseModeIntervals baseMode
--     extIntervals :: [Extension] -> Set Interval -> Set Interval 
--     <+>
     