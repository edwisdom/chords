module Base.Chord.Chord
  ( Chord
  , getChordRoot
  , getQuality
  , getHighestNatural
  , getExtensions
  , getSus
  , chordFrom
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Root
import Base.Chord.Sus



data Chord = Chord { getChordRoot :: Root
                   , getQuality :: Quality
                   , getHighestNatural :: HighestNatural
                   , getExtensions :: [Extension]
                   , getSus :: Sus
                   }

instance Show Chord where
  show chord = show     (getChordRoot      chord)
            ++ show     (getQuality     chord)
            ++ show     (getHighestNatural chord)
            ++ concat (map show (getExtensions  chord))
            ++ show     (getSus            chord)


chordFrom :: Root -> Quality -> HighestNatural -> [Extension] -> Sus -> Chord
chordFrom = Chord