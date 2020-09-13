{-|
Module      : Base.Scale.Scale
Description : Representation of a scale
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the representation for a scale,
a function to convert scales to notes, and some utility
functions for brevity elsewhere (e.g. major and minor).
-}
module Base.Scale.Scale
  ( Scale(..)
  , scaleToNotes
  , major
  , minor
  , scaleLength
  ) where

import Base.Core.Note
import Base.Core.Interval
import Base.Scale.BaseMode
import Base.Scale.Mode
import Data.Set(Set(..), fromList, toAscList, elemAt, insert, delete, mapMonotonic, isSubsetOf, toList)
import qualified Data.Set as S(filter, map)
import Data.Maybe(fromJust)

-- | A scale is specified by a note and a mode.
data Scale = Scale Note Mode

instance Show Scale where
  show (Scale note mode) = show note ++ " " ++ show mode

-- | Given a note, this returns a major scale starting from that note.
major :: Note -> Scale
major key = Scale key $ fromJust $ modeFrom Ionian []

-- | Given a note, this returns a minor scale starting from that note.
minor :: Note -> Scale
minor key = Scale key $ fromJust $ modeFrom Aeolian []

-- | Converts a scale to a list of notes.
scaleToNotes :: Scale -> [Note]
scaleToNotes (Scale note mode) = toList $ mapMonotonic (`jumpIntervalFromNote` note) (modeToIntervals mode)

-- | Returns the number of notes in a scale.
scaleLength :: Scale -> Int
scaleLength s = length $ scaleToNotes s
