{-|
Module      : Base.Chord.Symbol
Description : Abstract representation of a chord symbol
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

Abstractly represent a chord symbol, i.e. a chord shape with a specified root
note, but without the set of actual pitches in the chord.
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

-- | A chord symbol consists of a root note and a ChordShape, e.g.
-- a C and a M7 gives us a CM7.
data ChordSymbol = ChordSymbol { getChordRoot :: Note
                               , getShape :: ChordShape
                               } deriving (Eq, Show)

-- | ChordSymbol has all the properties of the Chordal typeclass,
-- most importantly, that it can be converted to a set of intervals.
instance Chordal ChordSymbol where
  quality = quality . getShape
  highestNatural = highestNatural . getShape
  extensions = extensions . getShape
  suspension = suspension . getShape
  toIntervals = toIntervals . getShape

-- | ChordSymbol has all the properties of the Rooted typeclass,
-- most importantly, that it can be converted to a set of notes.
instance Rooted ChordSymbol where
  root = getChordRoot
  toNotes sym =
    flip jumpIntervalFromNote (getChordRoot sym) <$> toList (toIntervals sym)

-- | Smart constructor for a ChordSymbol
chordSymbolFrom :: Note -> ChordShape -> ChordSymbol
chordSymbolFrom = ChordSymbol
