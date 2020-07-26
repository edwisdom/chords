module Base.Chord.Sus
  ( Sus
  , getMaybeDeg
  , sus
  , noSus
  ) where

newtype Sus = Sus { getMaybeDeg :: Maybe Int }
  deriving Show

sus :: Int -> Sus
sus deg = Sus $ Just deg

noSus :: Sus
noSus = Sus Nothing
