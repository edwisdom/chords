{-|
Module      : Base.Core.Letter
Description : Implements the Letter datatype and its basic functions
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the Letter datatype and functions to get the
nexf or previous letter in a cyclical fashion.
-}
module Base.Core.Letter
  ( Letter(..)
  , nextNthLetter
  , prevNthLetter
  ) where

-- | Musical letter names are one of A-G.
data Letter
 = A
 | B
 | C
 | D
 | E
 | F
 | G
 deriving (Show, Read, Eq)

-- | This instance allows each letter to be converted to a number
-- to allow for easy cyclical sequencing.
instance Enum Letter where
  toEnum 0 = A
  toEnum 1 = B
  toEnum 2 = C
  toEnum 3 = D
  toEnum 4 = E
  toEnum 5 = F
  toEnum 6 = G
  toEnum n = toEnum $ n `mod` 7

  fromEnum A = 0
  fromEnum B = 1
  fromEnum C = 2
  fromEnum D = 3
  fromEnum E = 4
  fromEnum F = 5
  fromEnum G = 6

-- Given a letter and some number of letters to skip ahead, this function
-- returns the resulting letter.
nextNthLetter :: Letter -> Int -> Letter
nextNthLetter letter i = iterate succ letter !! i

-- Given a letter and some number of letters to go backwards, this function
-- returns the resulting letter.
prevNthLetter :: Letter -> Int -> Letter
prevNthLetter letter i = iterate pred letter !! i
