{-|
Module      : Base.Core.Quality.IQuality
Description : Implements the interval quality datatype
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the (interval) Quality datatype and a
function to get the base quality of an interval size.
-}
module Base.Core.Quality.IQuality
  ( Quality(..)
  , major
  , isMajor
  , perfect
  , isPerfect
  , minor
  , isMinor
  , diminished
  , isDiminished
  , augmented
  , isAugmented
  , baseQuality
  , raisePerfect
  , raiseMajor
  , lowerPerfect
  , lowerMajor
  ) where

import Common.Utils (modByFrom)

{-| Interval qualities can be one of:
1. Major
2. Perfect
3. Minor
4. Diminished (single, doubly, triply, etc.)
5. Augmented (single, doubly, triply, etc.)
-}
data Quality
 = Major
 | Perfect
 | Minor
 | Diminished Int
 | Augmented Int

-- | Reasonable defaults for showing interval qualities. Note that
-- there is no standard for multiple diminished or augmented intervals
-- so we simply show the integer followed by dim or aug.
instance Show Quality where
  show Major          = "M"
  show Perfect        = "P"
  show Minor          = "m"
  show (Diminished i) = if i == 1 then "dim" else show i ++ "dim"
  show (Augmented i)  = if i == 1 then "aug" else show i ++ "aug"

-- | A smart constructor for the @Major@ interval quality.
major :: Quality
major = Major

-- | Return @True@ if and only if the provided @Quality@ is @Major@.
isMajor :: Quality -> Bool
isMajor Major = True
isMajor _     = False

-- | A smart constructor for the @Perfect@ interval quality.
perfect :: Quality
perfect = Perfect

-- | Return @True@ if and only if the provided @Quality@ is @Perfect@.
isPerfect :: Quality -> Bool
isPerfect Perfect = True
isPerfect _       = False

-- | A smart constructor for the @Minor@ interval quality.
minor :: Quality
minor = Minor

-- | Return @True@ if and only if the provided @Quality@ is @Minor@.
isMinor :: Quality -> Bool
isMinor Minor = True
isMinor _     = False

-- | A smart constructor for the @Diminished@ interval quality. Returns
-- @Nothing@ just in case the given @Int@ is less than or equal to 0 or
-- greater than or equal to 12.
diminished :: Int -> Maybe Quality
diminished x = if x > 0 && x < 12 then Just $ Diminished x else Nothing

-- | Return @True@ if and only if the provided @Quality@ is @Diminished@.
isDiminished :: Quality -> Bool
isDiminished (Diminished _) = True
isDiminished _              = False

-- | A smart constructor for the @Augmented@ interval quality. Returns
-- @Nothing@ just in case the given @Int@ is less than or equal to 0 or
-- greater than or equal to 12.
augmented :: Int -> Maybe Quality
augmented x = if x > 0 && x < 12 then Just $ Augmented x else Nothing

-- | Return @True@ if and only if the provided @Quality@ is @Augmented@.
isAugmented :: Quality -> Bool
isAugmented (Augmented _) = True
isAugmented _             = False

-- | Given an interval size, we return the base quality, either Perfect or Major
baseQuality :: Int -> Quality
baseQuality n
  | canonicalized `elem` [1, 4, 5]    = Perfect
  | canonicalized `elem` [2, 3, 6, 7] = Major
  where
    canonicalized = modByFrom 7 1 n

-- | Given an interval quality, this raises that quality by a semitone
-- assuming that its base quality is Perfect.
raisePerfect :: Quality -> Maybe Quality
raisePerfect Perfect        = Just $ Augmented 1
raisePerfect (Augmented x)  = augmented $ x + 1
raisePerfect (Diminished 1) = Just Perfect
raisePerfect (Diminished x) = diminished $ x - 1

-- | Given an interval quality, this raises that quality by a semitone
-- assuming that its base quality is Major.
raiseMajor :: Quality -> Maybe Quality
raiseMajor Major          = Just $ Augmented 1
raiseMajor (Augmented x)  = augmented $ x + 1
raiseMajor Minor          = Just Major
raiseMajor (Diminished 1) = Just Minor
raiseMajor (Diminished x) = diminished $ x - 1

-- | Given an interval quality, this lowers that quality by a semitone
-- assuming that its base quality is Perfect.
lowerPerfect :: Quality -> Maybe Quality
lowerPerfect Perfect        = Just $ Diminished 1
lowerPerfect (Diminished x) = diminished $ x + 1
lowerPerfect (Augmented 1)  = Just Perfect
lowerPerfect (Augmented x)  = augmented $ x-1

-- | Given an interval quality, this lowers that quality by a semitone
-- assuming that its base quality is Major.
lowerMajor :: Quality -> Maybe Quality
lowerMajor Major          = Just Minor
lowerMajor Minor          = Just $ Diminished 1
lowerMajor (Diminished x) = diminished $ x + 1
lowerMajor (Augmented 1)  = Just Major
lowerMajor (Augmented x)  = augmented $ x - 1