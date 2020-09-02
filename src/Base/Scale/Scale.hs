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

data Scale = Scale Note Mode

instance Show Scale where
  show (Scale note mode) = show note ++ " " ++ show mode

major :: Note -> Scale
major key = Scale key (modeFrom Ionian [])

minor :: Note -> Scale
minor key = Scale key (modeFrom Aeolian [])

scaleToNotes :: Scale -> [Note]
scaleToNotes (Scale note mode) = toList $ mapMonotonic (`jumpIntervalFromNote` note) (modeToIntervals mode)

scaleLength :: Scale -> Int
scaleLength s = length $ scaleToNotes s
