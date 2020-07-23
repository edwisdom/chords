module Base.Accidental
  ( Accidental(..) ) where

data Accidental
 = AccSharp Int
 | AccFlat Int
 | AccNatural

instance Show Accidental where
  show (AccSharp i) = concat $ replicate i "#"
  show (AccFlat i) = concat $ replicate i "b"
  show AccNatural = ""