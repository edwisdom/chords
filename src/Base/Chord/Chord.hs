module Base.Chord.Chord
  ( Chord
  , getChordRoot
  , getQuality
  , getHighestNatural
  , getExtensions
  , getSus
  , chordFrom
  ) where

import Base.Core.Quality

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Root
import Base.Chord.Sus

data Chord = Chord { getChordRoot :: Root
                   , getQuality :: Quality
                   , getHighestNatural :: HighestNatural
                   , getExtensions :: [Extension]
                   , getSus :: Sus } deriving Show

chordFrom :: Root -> Quality -> HighestNatural -> [Extension] -> Sus -> Chord
chordFrom = Chord
