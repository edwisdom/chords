module Base.ChordSymbol
  ( canonicalizeChord
  , transpose
  , transposeToRoot
  ) where

import Base.CQuality

import Base.Core.Quality.CQuality

import Base.Chord.Shape
import Base.Chord.Symbol
import Base.Chord.HighestNatural
import Base.Chord.Note
import Base.Chord.RawChord

import Base.Interval

import Base.Class.Chordal
import Base.Class.Rooted

canonicalizeChord :: Chord -> ChordSymbol
canonicalizeChord rc =
  chordSymbolFrom (getChordRoot rc) shape
  where
    hn = getHighestNatural rc
    shape = chordShapeFrom (canonicalizeQuality (getMQuality rc) hn) hn (getExtensions rc) (getSus rc)

transpose :: ChordSymbol -> Interval -> ChordSymbol
transpose c int =
  chordSymbolFrom (jumpIntervalFromNote int (root c)) shape
  where
    shape = chordShapeFrom (quality c) (highestNatural c) (extensions c) (suspension c)

transposeToRoot :: ChordSymbol -> Note -> ChordSymbol
transposeToRoot c note = chordSymbolFrom note shape
  where
    shape = chordShapeFrom (quality c) (highestNatural c) (extensions c) (suspension c)
