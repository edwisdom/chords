module Base.Chord where

import Base.Core.Accidental
import Base.Core.Note
import Base.Core.Quality

import Base.Chord.Extension
import Base.Chord.Root
import Base.Chord.Sus

data RawChord
  = RawChord Root (Maybe Quality) HighestNatural [Extension] Sus
 deriving Show

data HighestNatural
  = HighestNatural MajorOrNot Int
  deriving Show

data MajorOrNot
  = Major
  | NonMajor
  deriving Show

data Chord = Chord Root Quality HighestNatural [Extension] Sus
 deriving Show

canonicalizeQuality :: Maybe Quality -> HighestNatural -> Quality
canonicalizeQuality Nothing  (HighestNatural _ i) = if i < 7 then QMajor else QDominant
canonicalizeQuality (Just q) _                    = q

canonicalizeChord :: RawChord -> Chord
canonicalizeChord (RawChord root mqual highNat exts sus) =
  Chord root (canonicalizeQuality mqual highNat) highNat exts sus
