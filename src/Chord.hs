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
 deriving Show

data Note
 = A
 | B
 | C
 | D
 | E
 | F
 | G
 deriving (Show, Read)

data Accidental
 = AccSharp Int
 | AccFlat Int
 | AccNatural
 deriving Show
