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
 deriving (Show, Read, Enum, Bounded)


data Accidental
 = AccSharp Int
 | AccFlat Int
 | AccNatural

instance Show Accidental where
  show (AccSharp i) = concat $ replicate i "#"
  show (AccFlat i) = concat $ replicate i "b"
  show AccNatural = ""


nextNote :: Note -> Note
nextNote G = A
nextNote n = succ n

prevNote :: Note -> Note
prevNote A = G
prevNote n = pred n

nextNthNote :: Note -> Int -> Note
nextNthNote note i = iterate nextNote note !! i

prevNthNote :: Note -> Int -> Note
prevNthNote note i = iterate prevNote note !! i
