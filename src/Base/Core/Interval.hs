{-|
Module      : Base.Core.Interval
Description : Representation of, and functions on, intervals
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the Interval datatype, its accessors and
smart constructors, functions to compute with intervals, and
functions to infer intervals from other information (e.g. notes).
-}
module Base.Core.Interval
  ( Interval
  , getQuality
  , getSize
  , intervalFrom
  , intervalToDistance
  , (<+>)
  , (<->)
  , jumpIntervalFromNote
  , (|+|)
  , (|-|)
  , intervalBetweenNotes
  , modByFrom
  , normalizeIntervalSize
  , getIntWithSize
  , nthDegreeIntervals
  , zipToIntervalSet
  ) where

import Base.Class.Invertible

import Base.Core.Accidental
import Base.Core.Letter
import Base.Core.Quality.IQuality

import Base.Core.Note

import Base.Core.PitchClass

import Common.Utils (modByFrom)

import Control.Monad (zipWithM)

import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Set as S hiding (filter)

-- | Intervals are defined by a quality and a size.
data Interval = Interval { getQuality :: Quality
                         , getSize :: Int
                         }

-- | Two intervals are equal if they're enharmonically equivalent.
instance Eq Interval where
  int1 == int2 = intervalToDistance int1 == intervalToDistance int2

-- | Intervals can be ordered by their distance in semitones.
instance Ord Interval where
  int1 `compare` int2 = intervalToDistance int1 `compare` intervalToDistance int2

-- | Show an interval by showing its constitutent parts
instance Show Interval where
  show (Interval iQual i) = show iQual ++ show i

-- | This converts interval sizes to [1-7] to avoid compound intervals.
normalizeIntervalSize :: Int -> Int
normalizeIntervalSize = modByFrom 7 1

-- | Smart constructor for intervals to check whether the combination of
-- quality and size is valid. If it's invalid, this returns Nothing.
--
-- TODO: This should probably return an Either String Interval (or an error
-- type in place of String) instead of Maybe Interval, in order to facilitate
-- more sophisticated error handling / reporting.
intervalFrom :: Quality -> Int -> Maybe Interval
intervalFrom q s = if normalizedSize `elem` goodSizes then
                     Just Interval { getQuality = q, getSize = normalizedSize }
                   else
                     Nothing
  where
    normalizedSize :: Int
    normalizedSize = normalizeIntervalSize s

    goodSizes :: [Int]
    goodSizes = case q of
                  Perfect -> [1, 4, 5]
                  Major   -> [2, 3, 6, 7]
                  Minor   -> [2, 3, 6, 7]
                  _       -> [1 .. 7]

-- | Normalize an interval's size to [1-7] to avoid compound intervals.
normalizeInterval :: Interval -> Interval
normalizeInterval (Interval iQual i) = Interval iQual $ normalizeIntervalSize i

-- | Convert an interval to a distance in semitones. If the input is an
-- invalid interval, then this returns Nothing.
intervalToDistance :: Interval -> Maybe Int
intervalToDistance interval =
  case normalizeInterval interval of
    -- Base interval distances
    Interval Perfect 1 -> Just 0
    Interval Major   2 -> Just 2
    Interval Major   3 -> Just 4
    Interval Perfect 4 -> Just 5
    Interval Perfect 5 -> Just 7
    Interval Major   6 -> Just 9
    Interval Major   7 -> Just 11

    -- Minor shifts
    Interval Minor   i ->
      case baseQuality i of
        Major   -> subtract 1 <$> intervalToDistance (Interval Major i)
        Perfect -> Nothing

    -- Augmented shifts
    Interval (Augmented x) i  ->
      (x +) <$> intervalToDistance (Interval (baseQuality i) i)

    -- Diminished shifts
    Interval (Diminished x) i ->
      subtract offset <$> intervalToDistance (Interval bq i)
      where
        bq = baseQuality i
        offset = case bq of
                   Major   -> x + 1
                   Perfect -> x

    -- Anything else must be an invalid interval
    _ -> Nothing

-- | This function converts numbers to a range of [-6, 5]. This
-- is primarily used to compute the smallest number of accidentals
-- required to make interval math work out, i.e. we can represent
-- any note with 6 flats and 5 sharps.
lowestAbsValue :: Int -> Int
lowestAbsValue = modByFrom 12 (-6)

-- | Invert an interval, i.e. return the interval that, when added
-- to the original interval, results in a perfect octave.
--
-- prop> invert i |+| i == fromJust $ intervalFrom Perfect 1
instance Invertible Interval where
  invert (Interval iQual i) =
    let
      newI = normalizeIntervalSize $ 9 - normalizeIntervalSize i
      newQual =
        case iQual of
          Major          -> Minor
          Minor          -> Major
          Perfect        -> Perfect
          (Augmented x)  -> Diminished x
          (Diminished x) -> Augmented x
    in
      Interval newQual newI

-- | When adding an integer to an interval with this infix operator,
-- the interval's distance is increased by that integer without
-- changing its size.
--
-- prop> intervalToDistance i + x == intervalToDistance (i <+> x)
-- prop> getSize (i <+> _) == getSize i
infixl 6 <+>
(<+>) :: Interval -> Int -> Interval
Interval iQual i <+> x = Interval (iterate smartMod iQual !! abs x) i
  where
    unwrapMaybeQ :: (Maybe Quality -> Maybe Quality) -> Quality -> Quality
    unwrapMaybeQ f qual = fromMaybe (baseQuality i) (f $ Just qual)
    modFunc =
      case (baseQuality i, signum x) of
        (Perfect, 1)  -> raisePerfect
        (Major, 1)    -> raiseMajor
        (Perfect, -1) -> lowerPerfect
        (Major, -1)   -> lowerMajor
        (_, 0)         -> id
    smartMod = unwrapMaybeQ modFunc
    newQual = iterate smartMod iQual !! abs x

-- | When subtracting an integer from an interval with this infix operator,
-- the interval's distance is decreased by that integer without
-- changing its size.
--
-- This has the same properties as <+> but it's the inverse.
--
-- prop> (i <+> x) <-> x == i
infixl 6 <->
(<->) :: Interval -> Int -> Interval
interval <-> x = interval <+> (-x)

-- | This infix operator adds two intervals to give a new interval. Note that
-- this function may return compund intervals, i.e. getSize i > 7.
infixl 6 |+|
(|+|) :: Interval -> Interval -> Interval
int1@(Interval q1 i1) |+| int2@(Interval q2 i2) =
  let
    newI       = i1 + i2 - 1
    defQual    = baseQuality newI
    currDist   = fromJust $ intervalToDistance (Interval defQual newI)
    wantedDist = fromJust (intervalToDistance int1)
               + fromJust (intervalToDistance int2)
    diff       = lowestAbsValue $ wantedDist - currDist
  in
    Interval defQual newI <+> diff

-- | This infix operator subtracts one interval from another to give a
-- new interval. Note that this function will only return intervals with
-- getSize 0 < i < 8.
--
-- It is the inverse of |+|.
--
-- prop> (i1 |+| i2) |-| i2 == i1
infixl 6 |-|
(|-|) :: Interval -> Interval -> Interval
int1 |-| int2 = normalizeInterval $ int1 |+| invert int2

-- | Given an interval and a note, this returns the note that's
-- __higher__ than the given note by the given interval.
jumpIntervalFromNote :: Interval -> Note -> Note
jumpIntervalFromNote (Interval iQual iNum) r =
  let
    newNote    = nextNthLetter (getLetter r) $ iNum - 1
    currDist   = getPitchClass (noteToPitchClass (noteFrom newNote natural)) - getPitchClass (noteToPitchClass r)
    wantedDist =
      case intervalToDistance $ Interval iQual iNum of
        Just dist -> dist
        Nothing -> error "Invalid interval in jumpIntervalFromNote"
    newAcc     = shiftToAcc $ lowestAbsValue $ wantedDist - currDist
  in noteFrom newNote newAcc

-- | Given two notes, this function returns the interval between them.
intervalBetweenNotes :: Note -> Note -> Interval
intervalBetweenNotes start end =
  let
    letterDist = modByFrom 7 0 (fromEnum (getLetter end) - fromEnum (getLetter start)) + 1
    currInterval = fromJust $ intervalFrom (baseQuality letterDist) letterDist
    newNote = jumpIntervalFromNote currInterval start
    wantedDist = getPitchClass (noteToPitchClass end) - getPitchClass (noteToPitchClass newNote)
  in
    currInterval <+> modByFrom 12 (- 6) wantedDist

-- | Given an interval size and a list of intervals, this returns the
-- first occurrence of that interval size. Note that if the list
-- doesn't contain the interval size, this function will panic.
getIntWithSize :: Int -> [Interval] -> Interval
getIntWithSize i intList = head (filter (\ x -> getSize x == i) intList)

nthDegreeIntervals :: Set Interval -> Int -> Set Interval
nthDegreeIntervals ints n = S.map (|-| noteInterval) ints
  where
   noteInterval = toAscList ints !! (n - 1)

zipToIntervalSet :: [Quality] -> [Int] -> Set Interval
zipToIntervalSet quals sizes = fromList (fromJust . uncurry intervalFrom <$> zip quals sizes)
