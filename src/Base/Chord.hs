module Base.Chord
  ( canonicalizeChord
  ) where

import Base.Quality

import Base.Core.Quality.CQuality

import Base.Chord.Chord
import Base.Chord.HighestNatural
import Base.Chord.RawChord

canonicalizeChord :: RawChord -> Chord
canonicalizeChord rc =
  chordFrom (getRawChordRoot rc) (canonicalizeQuality (getMaybeQuality rc) hn) hn (getRawExtensions rc) (getRawSus rc)
  where
    hn = getRawHighestNatural rc
