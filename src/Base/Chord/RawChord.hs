{-|
Module      : Base.Chord.RawChord
Description : Implements the raw chord datatype (pre-canonicalization)
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the RawChord datatype and its smart constructor.
-}
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

-- The raw Chord datatype
data Chord = Chord { -- | Get the root of the chord
                     getChordRoot :: Note
                     -- | Get the quality of the chord if it's indicated.
                   , getMQuality :: Maybe Quality
                     -- | Get the highest natural degree
                   , getHighestNatural :: HighestNatural
                     -- | Get the list of chord extensions
                   , getExtensions :: [Extension]
                     -- | Get the sus
                   , getSus :: Sus
                   } deriving Show

-- Smart constructor for a raw chord
chordFrom :: Note -> Maybe Quality -> HighestNatural -> [Extension] -> Sus -> Chord
chordFrom = Chord
