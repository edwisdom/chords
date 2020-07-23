module Base.Chord.HighestNatural
  ( HighestNatural
  , isMajor
  , getDegree
  , majorNatural
  , nonMajorNatural
  ) where

data HighestNatural =
  HighestNatural { isMajor :: Bool
                 , getDegree :: Int
                 } deriving Show

majorNatural :: Int -> HighestNatural
majorNatural deg = HighestNatural { isMajor = True, getDegree = deg }

nonMajorNatural :: Int -> HighestNatural
nonMajorNatural deg = HighestNatural { isMajor = False, getDegree = deg }
