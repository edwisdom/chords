module Base.Chord where

import Base.Accidental
import Base.Extension
import Base.Note
import Base.Quality

data RawChord
  = RawChord Root (Maybe Quality) HighestNatural [Extension] Sus
 deriving Show

data HighestNatural
  = HighestNatural MajorOrNot Int
  deriving Show

data MajorOrNot
  = Major
  | NonMajor
  deriving Show

data Sus
  = Sus Int
  | NoSus
  deriving Show

data Root
  = Root Note Accidental

instance Show Root where
  show (Root note acc) = show note ++ show acc

data Chord = Chord Root Quality HighestNatural [Extension] Sus
 deriving Show

canonicalizeQuality :: Maybe Quality -> HighestNatural -> Quality
canonicalizeQuality Nothing  (HighestNatural _ i) = if i < 7 then QMajor else QDominant
canonicalizeQuality (Just q) _                    = q

canonicalizeChord :: RawChord -> Chord
canonicalizeChord (RawChord root mqual highNat exts sus) =
  Chord root (canonicalizeQuality mqual highNat) highNat exts sus
