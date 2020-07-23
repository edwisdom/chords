module Base.Accidental
  ( Accidental
  , nSharp
  , nFlat
  , natural
  , countAcc ) where

data Accidental
 = AccSharp Int
 | AccFlat Int
 | AccNatural

instance Show Accidental where
  show (AccSharp i) = concat $ replicate i "#"
  show (AccFlat i) = concat $ replicate i "b"
  show AccNatural = ""

nSharp :: Int -> Accidental
nSharp = AccSharp

nFlat :: Int -> Accidental
nFlat = AccFlat

natural :: Accidental
natural = AccNatural

countAcc :: Accidental -> Int
countAcc (AccSharp i) = i
countAcc (AccFlat i) = -i
countAcc AccNatural = 0
