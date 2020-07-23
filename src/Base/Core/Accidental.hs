module Base.Core.Accidental
  ( Accidental
  , nSharp
  , nFlat
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

nSharp :: Int -> Accidental
nSharp = AccSharp

nFlat :: Int -> Accidental
nFlat = AccFlat

natural :: Accidental
natural = AccNatural

impliedShift :: Accidental -> Int
impliedShift (AccSharp i) = i
impliedShift (AccFlat i)  = -i
impliedShift AccNatural   = 0
