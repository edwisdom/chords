module Base.Chord.Chord
  ( Chord
  , chordFrom
  , ExpChord
  ) where

import Base.Chord.ChordShape
import Base.Chord.ChordSymbol
import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Note
import Base.Chord.Sus

import Base.Core.Quality.CQuality

import Base.Class.Chordal
import Base.Class.Rooted

data Chord = Chord { getSymbol :: ChordSymbol
                   , getNotes :: [Root]
                   } deriving Show

instance Chordal Chord where
  quality = quality . getSymbol
  highestNatural = highestNatural . getSymbol
  extensions = extensions . getSymbol
  suspension = suspension . getSymbol
  toIntervals = toIntervals. getSymbol

instance Rooted Chord where
  root = root . getSymbol
  toNotes = getNotes

chordFrom :: Root -> Quality -> HighestNatural -> [Extension] -> Sus -> Chord
chordFrom r q hn exts s =
  Chord { getSymbol = chordSymbolFrom r $ chordShapeFrom q hn exts s
        , getNotes = toNotes sym
        }
  where
    sym = chordSymbolFrom r $ chordShapeFrom q hn exts s
