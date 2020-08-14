module Scale
  ( Scale(..)
  , BaseMode(..)
  , Mode(..)
  , ScaleExt(..)
  , baseModeIntervals
  , modeToIntervals
  , scaleToNotes
  , modalDistance
  ) where


import Base.Core.Quality.IQuality
import Base.Chord.Root
import Base.Interval
import Base.Core.Accidental(Accidental(..), impliedShift)
import Data.List (sort)
import Data.Set(Set(..), fromList, toAscList, elemAt, insert, delete, mapMonotonic)
import qualified Data.Set as S(filter, map)
import Data.Maybe(fromJust)



data Scale = Scale Root Mode


data Mode = Mode BaseMode [ScaleExt]


data ScaleExt = ScaleExt { acc :: Accidental
                         , deg :: Int
                         }


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


modeToIntervals :: Mode -> Set Interval
modeToIntervals (Mode baseMode exts) = foldr extIntervals (baseModeIntervals baseMode) exts
  where 
    extIntervals :: ScaleExt -> Set Interval -> Set Interval
    extIntervals ext intSet = insert (oldInt <+> (impliedShift $ acc ext)) (delete oldInt intSet)
      where
        -- TODO: If there isn't only one interval of a certain degree, the mode is
        -- ambiguously constructed and we should give a warning.
        oldInt = elemAt 0 (S.filter (\a -> getSize a == deg ext) intSet)    


scaleToNotes :: Scale -> Set Root
scaleToNotes (Scale root mode) = mapMonotonic (flip jumpIntervalFromNote root) (modeToIntervals mode)


modalDistance :: Set Interval -> Set Interval -> Int
modalDistance mode1 mode2 = sum $ intDistance <$> zip (toAscList mode1) (toAscList mode2)
  where 
    intDistance :: (Interval, Interval) -> Int
    intDistance (i1, i2) = abs $ fromJust $ intervalToDistance (i1 |-| i2) 


-- intervalsToMode :: Set Interval -> [Mode]
-- intervalsToMode intSet = 
--   let 
--     sameDegreeModes = filter (\a -> S.map getSize baseModeIntervals a 
--                                  == S.map getSize baseModeIntervals intSet) 
--                       [Lydian ..]
--     dists = modalDistance intSet <$> sameDegreeModes


