module Base.Chord.Sus
  ( Sus
  , getMaybeDeg
  , isSus
  , sus
  , noSus
  , susNoNum
  ) where

data Sus
  = Sus Int
  | SusNoNum
  | NoSus
  deriving(Eq)

getMaybeDeg :: Sus -> Maybe Int
getMaybeDeg (Sus i) = Just i
getMaybeDeg _ = Nothing

isSus :: Sus -> Bool
isSus NoSus = False
isSus _ = True

instance Show Sus where
  show sus =
    case sus of
      (Sus x) -> "sus" ++ show x
      SusNoNum -> "sus"
      NoSus -> ""

susNoNum :: Sus
susNoNum = SusNoNum

sus :: Int -> Sus
sus deg = Sus deg

noSus :: Sus
noSus = NoSus
