module Base.Chord.RawChord
  ( Chord
  , getChordRoot
  , getMQuality
  , getHighestNatural
  , getExtensions
  , getSus
  , rawChordFrom
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Root
import Base.Chord.Sus

data Chord = Chord { getChordRoot :: Root
                   , getMQuality :: Maybe Quality
                   , getHighestNatural :: HighestNatural
                   , getExtensions :: [Extension]
                   , getSus :: Sus
                   } deriving Show

rawChordFrom :: Root -> Maybe Quality -> HighestNatural -> [Extension] -> Sus -> Chord
rawChordFrom = Chord
