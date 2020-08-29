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
  , baseQuality
  ) where

import Common.Utils (modByFrom)

{-| Interval qualities can be one of:
1. Major
2. Perfect
3. Minor
4. Diminished (single, doubly, triply, etc.)
5. Augmented (single, doubly, triply, etc.)

TODO: Make sure that interval qualities are using smart constructors
so that diminished and augmented only receive integers >= 1.
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

-- | Given an interval size, we return the base quality, either Perfect or Major
baseQuality :: Int -> Quality
baseQuality n
  | canonicalized `elem` [1, 4, 5]    = Perfect
  | canonicalized `elem` [2, 3, 6, 7] = Major
  where
    canonicalized = modByFrom 7 1 n
