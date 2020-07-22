module CanonicalChord
  ( canonicalizeChord
  , Chord(..)
  , Root(..)
  , Quality(..)
  , HighestNatural(..)
  , Extension(..)
  , Sus(..)
  , MajorOrNot(..)
  , Note(..)
  , Accidental(..)
  , nextNote
  , nextNthNote
  , prevNote
  , prevNthNote
  ) where

import Chord hiding (Chord)
import qualified Chord (Chord(..))

data Chord
  = Chord Root Quality HighestNatural [Extension] Sus
 deriving Show


canonicalizeChord :: Chord.Chord -> Chord
canonicalizeChord (Chord.Chord root mqual highNat ext sus) =
  let ccqual = canonicalizeQuality mqual highNat
  in
    Chord root ccqual highNat ext sus


canonicalizeQuality :: Maybe Quality -> HighestNatural -> Quality
canonicalizeQuality Nothing (HighestNatural _ i) = if i < 7 then QMajor else QDominant
canonicalizeQuality (Just q) _ = q
