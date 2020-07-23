module Chord where

data Chord
  = Chord Root (Maybe Quality) HighestNatural [Extension] Sus
 deriving Show

data HighestNatural
  = HighestNatural MajorOrNot Int
  deriving Show

data MajorOrNot
  = Major
  | NonMajor
  deriving Show

data Sus
  = Sus Int
  | NoSus
  deriving Show

data Extension
 = ExtSharp Int
 | ExtFlat Int
 deriving Show

data Quality
 = QMajor
 | QMinor
 | QDominant
 | QDiminished
 | QAugmented
 deriving Show

data Root
  = Root Note Accidental

instance Show Root where
  show (Root note acc) = show note ++ show acc

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


data Accidental
 = AccSharp Int
 | AccFlat Int
 | AccNatural

instance Show Accidental where
  show (AccSharp i) = concat $ replicate i "#"
  show (AccFlat i) = concat $ replicate i "b"
  show AccNatural = ""

nextNthNote :: Note -> Int -> Note
nextNthNote note i = iterate succ note !! i

prevNthNote :: Note -> Int -> Note
prevNthNote note i = iterate pred note !! i
