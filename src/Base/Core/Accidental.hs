module Base.Core.Accidental
  ( Accidental
  , nSharps
  , nFlats
  , natural
  , impliedShift
  , shiftToAcc
  ) where

data Accidental
 = AccSharp Int
 | AccFlat Int
 | AccNatural
 deriving Eq

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

shiftToAcc :: Int -> Accidental
shiftToAcc i = 
  case signum i of
        1  -> nSharps i
        -1 -> nFlats (-i)
        0  -> natural