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

data Mode = Mode BaseMode [Extension]

instance Show Mode where
  show (Mode base exts) = show base
                       --Add a space if there are extensions...
                       ++ if null exts then "" else " "
                       --Add extensions separated by a comma...
                       ++ intercalate ", " (show <$> exts)

modeFrom :: BaseMode -> [Extension] -> Mode
modeFrom = Mode

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

numAlteredDegsInMode :: Mode -> Int
numAlteredDegsInMode (Mode base exts) = length exts

intervalsToMode :: Set Interval -> [Mode]
intervalsToMode intSet =
  let
    eqOnSize :: BaseMode -> Bool
    eqOnSize bm =
      ((==) `on` S.map getSize) (baseModeIntervals bm) intSet

    sameDegreeModes :: [BaseMode]
    sameDegreeModes = filter eqOnSize [Lydian ..]

    distanceFromIntSet :: Set Interval -> BaseMode -> Int
    distanceFromIntSet iSet mode = modalDistance iSet $ baseModeIntervals mode

    sortedModes :: [BaseMode]
    sortedModes = sortBy (compare `on` distanceFromIntSet intSet) sameDegreeModes

    exts :: [[Extension]]
    exts =
      let
        bmIntss = baseModeIntervals <$> sortedModes
      in
        modesToExts intSet <$> bmIntss
  in
    filter (\mode -> numAlteredDegsInMode mode == minimum (length <$> exts)) $ uncurry Mode <$> zip sortedModes exts

modalDistance :: Set Interval -> Set Interval -> Int
modalDistance mode1 mode2 = sum $ intDistance <$> (zip `on` toAscList) mode1 mode2
  where
    intDistance :: (Interval, Interval) -> Int
    intDistance (i1, i2) = abs $ fromJust $ intervalToDistance (i1 |-| i2)


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

getSubsetModeByDegree :: Set Interval -> Set Int -> Set Interval
getSubsetModeByDegree mode degs = S.filter (\i -> getSize i `elem` degs) mode
