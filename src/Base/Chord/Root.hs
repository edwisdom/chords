module Base.Chord.Root
  ( Root
  , getRoot
  , getAcc
  , rootFrom
  ) where

import Base.Core.Accidental
import Base.Core.Note

data Root = Root { getRoot :: Note
                 , getAcc :: Accidental
                 }

rootFrom :: Note -> Accidental -> Root
rootFrom note acc = Root { getRoot = note, getAcc = acc }

instance Show Root where
  show (Root note acc) = show note ++ show acc
