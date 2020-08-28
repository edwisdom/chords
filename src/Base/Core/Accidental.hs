{-|
Module      : Base.Core.Accidental
Description : Implements the Accidental datatype and its basic functions
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the Accidental datatype, its smart constructors,
and functions to convert accidentals to integers and back.
-}
module Base.Core.Accidental
  ( Accidental
  , nSharps
  , nFlats
  , natural
  , impliedShift
  , shiftToAcc
  ) where

-- Accidentals can either be some number of sharps,
-- some number of flats, or natural.
data Accidental
 = AccSharp Int
 | AccFlat Int
 | AccNatural
 deriving Eq

-- Show accidentals as they appear on sheet music (# or b)
instance Show Accidental where
  show (AccSharp i) = concat $ replicate i "#"
  show (AccFlat i)  = concat $ replicate i "b"
  show AccNatural   = ""

-- Smart constructor for sharp accidentals
nSharps :: Int -> Accidental
nSharps i = if res == 0 then AccNatural else AccSharp res
  where
    res = i `mod` 12

-- Smart constructor for flat accidentals
nFlats :: Int -> Accidental
nFlats i = if res == 0 then AccNatural else AccFlat res
  where
    res = i `mod` 12

-- Smart constructor for a natural accidental
natural :: Accidental
natural = AccNatural

-- Computes the number of semitone shift implied by an accidental
impliedShift :: Accidental -> Int
impliedShift (AccSharp i) = i
impliedShift (AccFlat i)  = -i
impliedShift AccNatural   = 0

-- Computes the accidental implied by some number of semitone shift
shiftToAcc :: Int -> Accidental
shiftToAcc i
  | i > 0     = AccSharp i
  | i < 0     = AccFlat (-i)
  | otherwise = AccNatural
