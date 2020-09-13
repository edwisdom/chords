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
  , iQualFrom
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


iQualFrom :: Quality -> Maybe Quality
iQualFrom (Diminished x) = if x > 0 && x < 12 then Just (Diminished x)
                           else Nothing
iQualFrom (Augmented x) = if x > 0 && x < 12 then Just (Augmented x)
                           else Nothing
iQualFrom q = Just q

-- | Given an interval size, we return the base quality, either Perfect or Major
baseQuality :: Int -> Quality
baseQuality n
  | canonicalized `elem` [1, 4, 5]    = Perfect
  | canonicalized `elem` [2, 3, 6, 7] = Major
  where
    canonicalized = modByFrom 7 1 n

-- | Given an interval quality, this raises that quality by a semitone
-- assuming that its base quality is Perfect.
raisePerfect :: Maybe Quality -> Maybe Quality
raisePerfect (Just Perfect)        = Just $ Augmented 1
raisePerfect (Just (Augmented x))  = iQualFrom $ Augmented $ x + 1
raisePerfect (Just (Diminished 1)) = Just Perfect
raisePerfect (Just (Diminished x)) = iQualFrom $ Diminished $ x - 1
raisePerfect Nothing               = Nothing

-- | Given an interval quality, this raises that quality by a semitone
-- assuming that its base quality is Major.
raiseMajor :: Maybe Quality -> Maybe Quality
raiseMajor (Just Major)          = Just $ Augmented 1
raiseMajor (Just (Augmented x))  = iQualFrom $ Augmented $ x + 1
raiseMajor (Just Minor        )  = Just Major
raiseMajor (Just (Diminished 1)) = Just Minor
raiseMajor (Just (Diminished x)) = iQualFrom $ Diminished $ x - 1
raiseMajor Nothing               = Nothing

-- | Given an interval quality, this lowers that quality by a semitone
-- assuming that its base quality is Perfect.
lowerPerfect :: Maybe Quality -> Maybe Quality
lowerPerfect (Just Perfect)        = Just $ Diminished 1
lowerPerfect (Just (Diminished x)) = iQualFrom $ Diminished $ x + 1
lowerPerfect (Just (Augmented 1))  = Just Perfect
lowerPerfect (Just (Augmented x))  = iQualFrom $ Augmented $ x-1
lowerPerfect Nothing               = Nothing

-- | Given an interval quality, this lowers that quality by a semitone
-- assuming that its base quality is Major.
lowerMajor :: Maybe Quality -> Maybe Quality
lowerMajor (Just Major)          = Just Minor
lowerMajor (Just Minor)          = Just $ Diminished 1
lowerMajor (Just (Diminished x)) = iQualFrom $ Diminished $ x + 1
lowerMajor (Just (Augmented 1))  = Just Major
lowerMajor (Just (Augmented x))  = iQualFrom $ Augmented $ x - 1
lowerMajor Nothing               = Nothing