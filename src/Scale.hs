module Scale
  ( Scale(..)
  , scaleToIntervals
  ) where


import Interval (Interval(..), IQuality(..), (|+|), (|-|))
import Data.List (sort)


data Scale
  = SLydian
  | SDorian
  | SMixolydian
  | SAugmentedQuality
  | SDiminishedQuality
  | SMajor
  | SMinor
  | SPhrygian
  | SLocrian
  | SMelodicMinor
  | SDorianb2
  | SLydianAug
  | SLydianDom
  | SMixolydianb6
  | SLocrianSharp2
  | SAltered
  | SHarmonicMinor
  | SLocrianNat6
  | SIonianSharp5
  | SDorianSharp4
  | SPhrygianDom
  | SLydianSharp2
  | SSuperLocrianbb7


nthDegreeIntervals :: [Interval] -> Int -> [Interval]
nthDegreeIntervals ints n = sort $ (|-| rootInterval) <$> ints
  where
    rootInterval = ints !! (n - 1)


listIntervals :: [IQuality] -> [Int] -> [Interval]
listIntervals qualities ints = uncurry Interval <$> zip qualities ints


scaleToIntervals :: Scale -> [Interval]
scaleToIntervals SMajor =
  listIntervals
  [IPerfect, IMajor, IMajor, IPerfect, IPerfect, IMajor, IMajor] [1..7]
scaleToIntervals SDorian =
  nthDegreeIntervals (scaleToIntervals SMajor) 2
scaleToIntervals SPhrygian =
  nthDegreeIntervals (scaleToIntervals SMajor) 3
scaleToIntervals SLydian =
  nthDegreeIntervals (scaleToIntervals SMajor) 4
scaleToIntervals SMixolydian =
  nthDegreeIntervals (scaleToIntervals SMajor) 5
scaleToIntervals SMinor =
  nthDegreeIntervals (scaleToIntervals SMajor) 6
scaleToIntervals SLocrian =
  nthDegreeIntervals (scaleToIntervals SMajor) 7
scaleToIntervals SAugmentedQuality =
  listIntervals
  [IPerfect, IMajor, IMajor, IAugmented 1, IAugmented 1, IMajor, IMinor] [1..7]
scaleToIntervals SDiminishedQuality =
  listIntervals
  [IPerfect, IMajor, IMinor, IPerfect, IDiminished 1, IMinor, IDiminished 1] [1..7]
scaleToIntervals SMelodicMinor =
  listIntervals
  [IPerfect, IMajor, IMinor, IPerfect, IPerfect, IMajor, IMajor] [1..7]
scaleToIntervals SDorianb2 =
  nthDegreeIntervals (scaleToIntervals SMelodicMinor) 2
scaleToIntervals SLydianAug =
  nthDegreeIntervals (scaleToIntervals SMelodicMinor) 3
scaleToIntervals SLydianDom =
  nthDegreeIntervals (scaleToIntervals SMelodicMinor) 4
scaleToIntervals SMixolydianb6 =
  nthDegreeIntervals (scaleToIntervals SMelodicMinor) 5
scaleToIntervals SLocrianSharp2 =
  nthDegreeIntervals (scaleToIntervals SMelodicMinor) 6
scaleToIntervals SAltered =
  nthDegreeIntervals (scaleToIntervals SMelodicMinor) 7
scaleToIntervals SHarmonicMinor =
  listIntervals
  [IPerfect, IMajor, IMinor, IPerfect, IPerfect, IMinor, IMajor] [1..7]
scaleToIntervals SLocrianNat6 =
  nthDegreeIntervals (scaleToIntervals SHarmonicMinor) 2
scaleToIntervals SIonianSharp5 =
  nthDegreeIntervals (scaleToIntervals SHarmonicMinor) 3
scaleToIntervals SDorianSharp4 =
  nthDegreeIntervals (scaleToIntervals SHarmonicMinor) 4
scaleToIntervals SPhrygianDom =
  nthDegreeIntervals (scaleToIntervals SHarmonicMinor) 5
scaleToIntervals SLydianSharp2 =
  nthDegreeIntervals (scaleToIntervals SHarmonicMinor) 6
scaleToIntervals SSuperLocrianbb7 =
  nthDegreeIntervals (scaleToIntervals SHarmonicMinor) 7
