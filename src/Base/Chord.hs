module Base.Chord
  ( canonicalizeChord
  , transpose
  ) where

import Base.CQuality

import Base.Core.Quality.CQuality

import Base.Chord.Chord as C
import Base.Chord.HighestNatural
import Base.Chord.RawChord as RC

import Base.Interval

canonicalizeChord :: RC.Chord -> C.Chord
canonicalizeChord rc =
  C.chordFrom (RC.getChordRoot rc) (canonicalizeQuality (RC.getMQuality rc) hn) hn (RC.getExtensions rc) (RC.getSus rc)
  where
    hn = RC.getHighestNatural rc

transpose :: C.Chord -> Interval -> C.Chord
transpose c int =
  C.chordFrom (jumpIntervalFromNote int (C.getChordRoot c)) (C.getQuality c) (C.getHighestNatural c) (C.getExtensions c) (C.getSus c)
