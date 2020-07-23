module Base.Chord where

import Base.Core.Accidental
import Base.Core.Note
import Base.Core.Quality

import Base.Chord.Chord
import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Root
import Base.Chord.Sus

data RawChord
  = RawChord Root (Maybe Quality) HighestNatural [Extension] Sus
 deriving Show

canonicalizeQuality :: Maybe Quality -> HighestNatural -> Quality
canonicalizeQuality Nothing  hn = if (getDegree hn) < 7 then QMajor else QDominant
canonicalizeQuality (Just q) _                    = q

canonicalizeChord :: RawChord -> Chord
canonicalizeChord (RawChord root mqual highNat exts sus) =
  chordFrom root (canonicalizeQuality mqual highNat) highNat exts sus
