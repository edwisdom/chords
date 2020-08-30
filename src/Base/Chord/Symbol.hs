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
  , canonicalizeChord
  , transpose
  , transposeToRoot
  ) where

import qualified Base.Chord.Raw as RC
import Base.Chord.Shape

import Base.Core.Note
import Base.Core.Quality.CQuality

import Base.Class.Chordal
import Base.Class.Rooted

import Base.Core.Interval

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

-- | Canonicalize a raw chord into a chord symbol. This is used
-- as the last step to parse user-inputted chord symbol strings.
canonicalizeChord :: RC.Chord -> ChordSymbol
canonicalizeChord rc =
  chordSymbolFrom (RC.getChordRoot rc) shape
  where
    hn = RC.getHighestNatural rc
    shape = chordShapeFrom (canonicalizeQuality (RC.getMQuality rc) hn) hn (RC.getExtensions rc) (RC.getSus rc)

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
