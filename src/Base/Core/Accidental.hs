module Base.Core.Accidental
  ( Accidental
  , nSharps
  , nFlats
  , natural
  , impliedShift
  ) where

data Accidental
 = AccSharp Int
 | AccFlat Int
 | AccNatural

instance Show Accidental where
  show (AccSharp i) = concat $ replicate i "#"
  show (AccFlat i)  = concat $ replicate i "b"
  show AccNatural   = ""

nSharps :: Int -> Accidental
nSharps = AccSharp

nFlats :: Int -> Accidental
nFlats = AccFlat

natural :: Accidental
natural = AccNatural

impliedShift :: Accidental -> Int
impliedShift (AccSharp i) = i
impliedShift (AccFlat i)  = -i
impliedShift AccNatural   = 0
