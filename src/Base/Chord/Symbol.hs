{-|
Module      : Base.Chord.Symbol
Description : Abstract representation of a chord symbol
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

Abstractly represent a chord symbol, i.e. a chord shape with a specified root
note.
-}

module Base.Chord.Symbol
  ( ChordSymbol
  , getShape
  , chordSymbolFrom
  ) where

import Base.Chord.Shape
import Base.Chord.Note

import Base.Class.Chordal
import Base.Class.Rooted

import Base.Interval

import Data.Set

data ChordSymbol = ChordSymbol { getChordRoot :: Note
                               , getShape :: ChordShape
                               } deriving (Eq, Show)

instance Chordal ChordSymbol where
  quality = quality . getShape
  highestNatural = highestNatural . getShape
  extensions = extensions . getShape
  suspension = suspension . getShape
  toIntervals = toIntervals . getShape

instance Rooted ChordSymbol where
  root = getChordRoot
  toNotes sym =
    flip jumpIntervalFromNote (getChordRoot sym) <$> toList (toIntervals sym)

chordSymbolFrom :: Note -> ChordShape -> ChordSymbol
chordSymbolFrom = ChordSymbol
