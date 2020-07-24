module Base.Chord.RawChord
  ( RawChord
  , getRawChordRoot
  , getMaybeQuality
  , getRawHighestNatural
  , getRawExtensions
  , getRawSus
  , rawChordFrom
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Root
import Base.Chord.Sus

data RawChord = RawChord { getRawChordRoot :: Root
                         , getMaybeQuality :: Maybe Quality
                         , getRawHighestNatural :: HighestNatural
                         , getRawExtensions :: [Extension]
                         , getRawSus :: Sus
                         } deriving Show

rawChordFrom :: Root -> Maybe Quality -> HighestNatural -> [Extension] -> Sus -> RawChord
rawChordFrom = RawChord
