module Base.Chord
  ( canonicalizeChord
  ) where

import Base.Core.Quality

import Base.Chord.Chord
import Base.Chord.HighestNatural
import Base.Chord.RawChord

canonicalizeQuality :: Maybe Quality -> HighestNatural -> Quality
canonicalizeQuality Nothing  hn = if (getDegree hn) < 7 then QMajor else QDominant
canonicalizeQuality (Just q) _                    = q

canonicalizeChord :: RawChord -> Chord
canonicalizeChord rc =
  chordFrom (getRawChordRoot rc) (canonicalizeQuality (getMaybeQuality rc) hn) hn (getRawExtensions rc) (getRawSus rc)
  where
    hn = getRawHighestNatural rc
