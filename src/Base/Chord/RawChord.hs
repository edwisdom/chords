module Base.Chord.RawChord
  ( Chord
  , getChordRoot
  , getMQuality
  , getHighestNatural
  , getExtensions
  , getSus
  , chordFrom
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Note
import Base.Chord.Sus

data Chord = Chord { getChordRoot :: Note
                   , getMQuality :: Maybe Quality
                   , getHighestNatural :: HighestNatural
                   , getExtensions :: [Extension]
                   , getSus :: Sus
                   } deriving Show

chordFrom :: Note -> Maybe Quality -> HighestNatural -> [Extension] -> Sus -> Chord
chordFrom = Chord
