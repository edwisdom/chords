{-|
Module      : Base.ChordSymbol
Description : Basic functions to create and update Chord Symbols
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions to convert from Chord to ChordSymbol
and to transpose ChordSymbols.
-}
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


-- | Canonicalize a raw chord into a chord symbol. This is used
-- as the last step to parse user-inputted chord symbol strings.
canonicalizeChord :: Chord -> ChordSymbol
canonicalizeChord rc =
  chordSymbolFrom (getChordRoot rc) shape
  where
    hn = getHighestNatural rc
    shape = chordShapeFrom (canonicalizeQuality (getMQuality rc) hn) hn (getExtensions rc) (getSus rc)

-- | Transpose a chord symbol __up__ by a given interval, returning a new chord symbol.
-- The chord shape remains unchanged.
-- prop> getShape $ transpose c _ == getShape c
transpose :: ChordSymbol -> Interval -> ChordSymbol
transpose c int =
  chordSymbolFrom (jumpIntervalFromNote int (root c)) shape
  where
    shape = chordShapeFrom (quality c) (highestNatural c) (extensions c) (suspension c)

-- | Transpose a chord symbol to a new root note, keeping the same chord shape.
-- | The chord shape remains unchanged.
-- prop> getShape $ transposeToRoot c _ == getShape c
transposeToRoot :: ChordSymbol -> Note -> ChordSymbol
transposeToRoot c note = chordSymbolFrom note shape
  where
    shape = chordShapeFrom (quality c) (highestNatural c) (extensions c) (suspension c)
