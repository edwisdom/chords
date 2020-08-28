{-|
Module      : Base.Chord.ChordSymbol
Description : Abstract representation of a chord symbol
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

Abstractly represent a chord symbol, i.e. a chord shape with a specified root
note.
-}

module Base.Chord.ChordSymbol
  ( ChordSymbol
  , getShape
  , chordSymbolFrom
  ) where

import Base.Chord.ChordShape
import Base.Chord.Root

import Base.Class.Chordal
import Base.Class.Rooted

import Base.Interval

import Data.Set

data ChordSymbol = ChordSymbol { getChordRoot :: Root
                               , getShape :: ChordShape
                               } deriving Show

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

chordSymbolFrom :: Root -> ChordShape -> ChordSymbol
chordSymbolFrom = ChordSymbol
