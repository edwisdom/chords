module Base.Core.Letter
  ( Letter(..)
  , nextNthLetter
  , prevNthLetter
  ) where

data Letter
 = A
 | B
 | C
 | D
 | E
 | F
 | G
 deriving (Show, Read, Eq)

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

nextNthLetter :: Letter -> Int -> Letter
nextNthLetter letter i = iterate succ letter !! i

prevNthLetter :: Letter -> Int -> Letter
prevNthLetter letter i = iterate pred letter !! i
