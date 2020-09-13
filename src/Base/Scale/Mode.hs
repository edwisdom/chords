{-|
Module      : Base.Scale.Mode
Description : Representation of a mode
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the representation for a mode,
functions to go between modes and sets of intervals,
and basic computations with modes (e.g. modal distance).
-}
module Base.Scale.Mode
  ( Mode
  , modeFrom
  , modeToIntervals
  , numAlteredDegsInMode
  , intervalsToMode
  ) where

import Base.Core.Accidental
import Base.Core.Interval

import Base.Scale.BaseMode
import Base.Scale.Extension

import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Maybe (fromJust)
import Data.Set (Set(..), insert, delete, elemAt, toAscList)
import qualified Data.Set as S(filter, map)

-- | A mode is constructed from a base mode and scale extensions.
data Mode = Mode BaseMode [Extension]

instance Show Mode where
  show (Mode base exts) = show base
                       --Add a space if there are extensions...
                       ++ if null exts then "" else " "
                       --Add extensions separated by a comma...
                       ++ intercalate ", " (show <$> exts)

-- | Smart constructor for a mode
modeFrom :: BaseMode -> [Extension] -> Mode
modeFrom = Mode

-- | Converts a mode into a set of intervals.
modeToIntervals :: Mode -> Set Interval
modeToIntervals (Mode baseMode exts) =
  foldr extIntervals (baseModeIntervals baseMode) exts
  where
    extIntervals :: Extension -> Set Interval -> Set Interval
    extIntervals ext intSet = insert (oldInt <+> impliedShift (getAccidental ext)) (delete oldInt intSet)
      where
        -- TODO: If there isn't only one interval of a certain degree, the mode is
        -- ambiguously constructed and we should give a warning.
        oldInt = elemAt 0 (S.filter (\a -> getSize a == getDegree ext) intSet)

-- | Given a mode, returns the number of scale extensions.
numAlteredDegsInMode :: Mode -> Int
numAlteredDegsInMode (Mode base exts) = length exts

-- | Converts a set of intervals into a list of the simplest
-- possible (i.e. fewest extensions) mode names.
intervalsToMode :: Set Interval -> [Mode]
intervalsToMode intSet =
  let
    -- Checks if a base mode has the same set of interval sizes as the input
    eqOnSize :: BaseMode -> Bool
    eqOnSize bm =
      ((==) `on` S.map getSize) (baseModeIntervals bm) intSet

    -- Finds all the base modes that have the same interval sizes as the input
    sameDegreeModes :: [BaseMode]
    sameDegreeModes = filter eqOnSize [Lydian ..]

    -- Returns the modal distance between an interval set and a given base mode
    distanceFromIntSet :: Set Interval -> BaseMode -> Int
    distanceFromIntSet iSet mode = modalDistance iSet $ baseModeIntervals mode

    -- Sorts base modes by their modal distance from the input
    sortedModes :: [BaseMode]
    sortedModes = sortBy (compare `on` distanceFromIntSet intSet) sameDegreeModes

    -- Returns a list of scale extension lists. Each extension list accompanies
    -- a base mode, i.e. it converts the base mode to the input interval set.
    exts :: [[Extension]]
    exts =
      let
        bmIntss = baseModeIntervals <$> sortedModes
      in
        modesToExts intSet <$> bmIntss
  in
    filter (\mode -> numAlteredDegsInMode mode == minimum (length <$> exts)) $ uncurry Mode <$> zip sortedModes exts

-- | Given two sets of intervals, this returns the modal distance between them.
modalDistance :: Set Interval -> Set Interval -> Int
modalDistance mode1 mode2 = sum $ intDistance <$> (zip `on` toAscList) mode1 mode2
  where
    intDistance :: (Interval, Interval) -> Int
    intDistance (i1, i2) = abs $ fromJust $ intervalToDistance (i1 |-| i2)

-- | Given two sets of intervals, this returns the list of extensions required
-- to convert the first into the second.
modesToExts :: Set Interval -> Set Interval -> [Extension]
modesToExts mode1 mode2 =
  let
    zippedInts = zip (toAscList mode1) (toAscList mode2)
    intervalDiffToAcc :: Interval -> Interval -> Accidental
    intervalDiffToAcc i1 i2 = shiftToAcc $ fromJust $ intervalToDistance $ i2 |-| i1
    accToExtList :: Accidental -> Int -> [Extension] -> [Extension]
    accToExtList accidental degree
      | accidental == natural = id
      | otherwise             = (scaleExtensionFrom accidental degree :)
  in
    foldr (\(i1,i2) exts -> accToExtList (intervalDiffToAcc i2 i1) (getSize i1) exts)
          []
          zippedInts

-- | Given a set of intervals and a set of degrees, this pulls out
-- the intervals at those degrees and returns a subset of the intervals.
getSubsetModeByDegree :: Set Interval -> Set Int -> Set Interval
getSubsetModeByDegree mode degs = S.filter (\i -> getSize i `elem` degs) mode