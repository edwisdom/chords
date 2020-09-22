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

{-# LANGUAGE PatternSynonyms #-}

module Base.Core.Quality.IQuality
  ( Quality
  , pattern Major
  , pattern Minor
  , pattern Perfect
  , pattern Diminished
  , pattern Augmented
  , major
  , perfect
  , minor
  , diminished
  , augmented
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
 = RealMajor
 | RealPerfect
 | RealMinor
 | RealDiminished Int
 | RealAugmented Int

pattern Major <- RealMajor
pattern Perfect <- RealPerfect
pattern Minor <- RealMinor
pattern Diminished x <- RealDiminished x
pattern Augmented x <- RealAugmented x

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
major = RealMajor

-- | A smart constructor for the @Perfect@ interval quality.
perfect :: Quality
perfect = RealPerfect

-- | A smart constructor for the @Minor@ interval quality.
minor :: Quality
minor = RealMinor

-- | A smart constructor for the @Diminished@ interval quality. Returns
-- @Nothing@ just in case the given @Int@ is less than or equal to 0 or
-- greater than or equal to 12.
diminished :: Int -> Maybe Quality
diminished x = if x > 0 && x < 12 then Just $ RealDiminished x else Nothing

-- | A smart constructor for the @Augmented@ interval quality. Returns
-- @Nothing@ just in case the given @Int@ is less than or equal to 0 or
-- greater than or equal to 12.
augmented :: Int -> Maybe Quality
augmented x = if x > 0 && x < 12 then Just $ RealAugmented x else Nothing

-- | Given an interval size, we return the base quality, either Perfect or Major
baseQuality :: Int -> Quality
baseQuality n
  | canonicalized `elem` [1, 4, 5]    = RealPerfect
  | canonicalized `elem` [2, 3, 6, 7] = RealMajor
  where
    canonicalized = modByFrom 7 1 n

-- | Given an interval quality, this raises that quality by a semitone
-- assuming that its base quality is Perfect.
raisePerfect :: Quality -> Maybe Quality
raisePerfect Perfect        = Just $ RealAugmented 1
raisePerfect (Augmented x)  = augmented $ x + 1
raisePerfect (Diminished 1) = Just RealPerfect
raisePerfect (Diminished x) = diminished $ x - 1

-- | Given an interval quality, this raises that quality by a semitone
-- assuming that its base quality is Major.
raiseMajor :: Quality -> Maybe Quality
raiseMajor Major          = Just $ RealAugmented 1
raiseMajor (Augmented x)  = augmented $ x + 1
raiseMajor Minor          = Just RealMajor
raiseMajor (Diminished 1) = Just RealMinor
raiseMajor (Diminished x) = diminished $ x - 1

-- | Given an interval quality, this lowers that quality by a semitone
-- assuming that its base quality is Perfect.
lowerPerfect :: Quality -> Maybe Quality
lowerPerfect Perfect        = Just $ RealDiminished 1
lowerPerfect (Diminished x) = diminished $ x + 1
lowerPerfect (Augmented 1)  = Just RealPerfect
lowerPerfect (Augmented x)  = augmented $ x-1

-- | Given an interval quality, this lowers that quality by a semitone
-- assuming that its base quality is Major.
lowerMajor :: Quality -> Maybe Quality
lowerMajor Major          = Just RealMinor
lowerMajor Minor          = Just $ RealDiminished 1
lowerMajor (Diminished x) = diminished $ x + 1
lowerMajor (Augmented 1)  = Just RealMajor
lowerMajor (Augmented x)  = augmented $ x - 1