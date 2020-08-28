module Base.Chord.Chord
  ( Chord
  , getChordRoot
  , getQuality
  , getHighestNatural
  , getExtensions
  , getSus
  , chordFrom
  , ExpChord
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Note
import Base.Chord.Sus



data Chord = Chord { getChordRoot :: Note
                   , getQuality :: Quality
                   , getHighestNatural :: HighestNatural
                   , getExtensions :: [Extension]
                   , getSus :: Sus
                   }
  deriving(Eq)

instance Show Chord where
  show chord = show     (getChordRoot      chord)
            ++ show     (getQuality     chord)
            ++ show     (getHighestNatural chord)
            ++ concatMap show (getExtensions chord)
            ++ show     (getSus            chord)


chordFrom :: Note -> Quality -> HighestNatural -> [Extension] -> Sus -> Chord
chordFrom = Chord

type ExpChord = (Chord, [Note])
