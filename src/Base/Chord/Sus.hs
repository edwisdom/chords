module Base.Chord.Sus
  ( Sus
  , getMaybeDeg
  , sus
  , noSus
  ) where

newtype Sus = Sus { getMaybeDeg :: Maybe Int }

instance Show Sus where
  show sus = 
    case getMaybeDeg sus of 
      (Just x) -> "sus" ++ show x
      Nothing -> "sus"


sus :: Int -> Sus
sus deg = Sus $ Just deg

noSus :: Sus
noSus = Sus Nothing
