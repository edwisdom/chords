{-|
Module      : Base.Chord.ChordShape
Description : Abstract representation of a chord's shape
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

@ChordShape@ captures all of the information in a 'Chord', other than the
chord's root. The only functions exported are a constructor and accessors.
-}

module Base.Chord.ChordShape
  ( ChordShape
  , getQuality
  , getHighestNatural
  , getExtensions
  , getSus
  , chordShapeFrom
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Sus

data ChordShape = ChordShape { getQuality :: Quality
                             , getHighestNatural :: HighestNatural
                             , getExtensions :: [Extension]
                             , getSus :: Sus
                             } deriving Show

chordShapeFrom :: Quality -> HighestNatural -> [Extension] -> Sus -> ChordShape
chordShapeFrom = ChordShape
