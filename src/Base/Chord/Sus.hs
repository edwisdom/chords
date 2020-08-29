{-|
Module      : Base.Chord.Sus
Description : Implements Sus datatype (for chords)
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the Sus datatype and its smart constructors.
-}
module Base.Chord.Sus
  ( Sus
  , getMaybeDeg
  , isSus
  , sus
  , noSus
  , susNoNum
  ) where

-- | Either it can be a specific sus interval,
-- all the common sus intervals (2, 4, sometimes, 6),
-- or no sus intervals.
data Sus
  = Sus Int
  | SusNoNum
  | NoSus
  deriving(Eq)

-- | Get the degree to be sussed
getMaybeDeg :: Sus -> Maybe Int
getMaybeDeg (Sus i) = Just i
getMaybeDeg _ = Nothing

-- | Returns False if there's no sus and True otherwise.
isSus :: Sus -> Bool
isSus NoSus = False
isSus _ = True

-- Pretty-print for all sus options
instance Show Sus where
  show sus =
    case sus of
      (Sus x) -> "sus" ++ show x
      SusNoNum -> "sus"
      NoSus -> ""

-- | Smart constructor for sus without a specific interval
susNoNum :: Sus
susNoNum = SusNoNum

-- | Smart constructor for sus with a specific interval
sus :: Int -> Sus
sus = Sus

-- | Smart constructor for no sus
noSus :: Sus
noSus = NoSus
