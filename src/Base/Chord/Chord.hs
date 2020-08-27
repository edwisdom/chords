module Base.Chord.Chord
  ( Chord
  , getChordRoot
  , getQuality
  , getHighestNatural
  , getExtensions
  , getSus
  , chordFrom
  ) where

import qualified Base.Chord.ChordShape as Shape
import qualified Base.Chord.ChordSymbol as Sym
import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Root
import Base.Chord.Sus

import Base.Core.Note
import Base.Core.Quality.CQuality

data Chord = Chord { getSymbol :: Sym.ChordSymbol
                   , getNotes :: [Note]
                   } deriving Show

getChordRoot :: Chord -> Root
getChordRoot = Sym.getChordRoot . getSymbol

getQuality :: Chord -> Quality
getQuality = Shape.getQuality . Sym.getShape . getSymbol

getHighestNatural :: Chord -> HighestNatural
getHighestNatural = Shape.getHighestNatural . Sym.getShape . getSymbol

getExtensions :: Chord -> [Extension]
getExtensions = Shape.getExtensions . Sym.getShape . getSymbol

getSus :: Chord -> Sus
getSus = Shape.getSus . Sym.getShape . getSymbol

-- TODO: Actually get `chordToNotes` somewhere where we can put the real list
-- of notes here
chordFrom :: Root -> Quality -> HighestNatural -> [Extension] -> Sus -> Chord
chordFrom r q hn exts s =
  Chord { getSymbol = Sym.chordSymbolFrom r $ Shape.chordShapeFrom q hn exts s
        , getNotes = []
        }
