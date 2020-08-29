{-|
Module      : Base.Chord.Shape
Description : Abstract representation of a chord's shape
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

@ChordShape@ captures all of the information in a 'Chord', other than the
chord's root. The only functions exported are a constructor and accessors.
-}

module Base.Chord.Shape
  ( ChordShape
  , chordShapeFrom
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Sus

import Base.Class.Chordal

import Base.Heliotonic

import Data.Set hiding (foldr)

-- | A ChordShape, e.g. a M7 chord, is a rootless abstract shape.
data ChordShape = ChordShape { getQuality :: Quality
                             , getHighestNatural :: HighestNatural
                             , getExtensions :: [Extension]
                             , getSus :: Sus
                             } deriving (Eq, Show)

-- | ChordShape has all the properties of the Chordal typeclass
instance Chordal ChordShape where
  quality = getQuality
  highestNatural = getHighestNatural
  extensions = getExtensions
  suspension = getSus
  toIntervals shape =
    let
      qualInts  = qualityToIntervals $ getQuality shape
      baseScale = highestNaturalToIntervals qualInts $ getHighestNatural shape
      extendedScale = extendIntervals baseScale $ getExtensions shape
      intervals     = susIntervals extendedScale $ getSus shape
    in
      foldr insert empty intervals

-- | Smart constructor for ChordShape
chordShapeFrom :: Quality -> HighestNatural -> [Extension] -> Sus -> ChordShape
chordShapeFrom = ChordShape
