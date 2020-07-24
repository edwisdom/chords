module Base.Chord
  ( canonicalizeChord
  ) where

import Base.Quality

import Base.Core.Quality.CQuality

import qualified Base.Chord.Chord as C
import Base.Chord.HighestNatural
import qualified Base.Chord.RawChord as RC

canonicalizeChord :: RC.Chord -> C.Chord
canonicalizeChord rc =
  C.chordFrom (RC.getChordRoot rc) (canonicalizeQuality (RC.getMQuality rc) hn) hn (RC.getExtensions rc) (RC.getSus rc)
  where
    hn = RC.getHighestNatural rc
