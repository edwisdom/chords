module CanonicalChord where
import Chord (Quality, HighestNatural, Extension, Sus, Note)

data Chord
  = Chord Root Quality HighestNatural [Extension] Sus
 deriving Show

data Root
  = Root Note Accidental
 deriving Show

data Accidental
 = AccSharp
 | AccFlat
 | AccNatural
 deriving Show
