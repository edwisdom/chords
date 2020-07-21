module CanonicalChord where
import Chord (Root, Quality, HighestNatural, Extension, Sus, Note)

data Chord
  = Chord Root Quality HighestNatural [Extension] Sus
 deriving Show
