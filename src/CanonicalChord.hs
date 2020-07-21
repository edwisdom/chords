module CanonicalChord where
import Chord (Quality, HighestNatural, Extension, Sus, Note)

data Chord
  = Chord Root Quality HighestNatural [Extension] Sus
 deriving Show

data Root
  = Root Note Accidental

instance Show Root where
  show (Root note acc) = (show note) ++ (show acc)
    
data Accidental
 = AccSharp
 | AccFlat
 | AccNatural

instance Show Accidental where 
  show AccSharp = "#"
  show AccFlat = "b"
  show AccNatural = ""