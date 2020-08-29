{-|
Module      : Base.Class.Chordal
Description : Type class for types that have chord-structure properties
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

The type class @Chordal@ stands for those types admitting a sensible notion of
chord structure, divorced from any notion of being rooted at a particular note.
-}

module Base.Class.Chordal
  ( Chordal (..)
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.HighestNatural
import Base.Chord.Extension
import Base.Chord.Sus

import Base.Interval

import Data.Set

class Chordal a where
  quality :: a -> Quality
  highestNatural :: a -> HighestNatural
  extensions :: a -> [Extension]
  suspension :: a -> Sus
  toIntervals :: a -> Set Interval
