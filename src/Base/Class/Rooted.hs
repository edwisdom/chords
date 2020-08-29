{-|
Module      : Base.Class.Rooted
Description : Type class for types that are alike to rooted chords
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

The type class @Rooted@ encompasses all of the 'Chordal' types that also admit
a sensible notion of being rooted, i.e. having a root note.
-}
module Base.Class.Rooted
  ( Rooted (..)
  ) where

import Base.Chord.Note

import Base.Class.Chordal

class Chordal a => Rooted a where
  root :: a -> Note
  toNotes :: a -> [Note]
