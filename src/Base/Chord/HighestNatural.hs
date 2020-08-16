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
                 }

instance Show HighestNatural where
  show highNat = 
    case isMajor highNat of
      True -> "M" ++ show (getDegree highNat)
      False -> show (getDegree highNat)

majorNatural :: Int -> HighestNatural
majorNatural deg = HighestNatural { isMajor = True, getDegree = deg }

nonMajorNatural :: Int -> HighestNatural
nonMajorNatural deg = HighestNatural { isMajor = False, getDegree = deg }
