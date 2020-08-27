{-|
Module      : Base.Chord.ChordSymbol
Description : Abstract representation of a chord symbol
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

Abstractly represent a chord symbol, i.e. a chord shape with a specified root
note.
-}

module Base.Chord.ChordSymbol
  ( ChordSymbol
  , getChordRoot
  , getShape
  , chordSymbolFrom
  ) where

import Base.Chord.ChordShape
import Base.Chord.Root

data ChordSymbol = ChordSymbol { getChordRoot :: Root
                               , getShape :: ChordShape
                               } deriving Show

chordSymbolFrom :: Root -> ChordShape -> ChordSymbol
chordSymbolFrom = ChordSymbol
