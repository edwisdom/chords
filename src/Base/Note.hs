module Base.Note
  ( Note(..)
  , nextNthNote
  , prevNthNote
  ) where

data Note
 = A
 | B
 | C
 | D
 | E
 | F
 | G
 deriving (Show, Read)

instance Enum Note where
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

nextNthNote :: Note -> Int -> Note
nextNthNote note i = iterate succ note !! i

prevNthNote :: Note -> Int -> Note
prevNthNote note i = iterate pred note !! i
