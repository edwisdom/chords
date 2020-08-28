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
  deriving(Eq)

instance Show HighestNatural where
  show highNat =
    case (isMajor highNat, getDegree highNat) of
      (True, _) -> "M" ++ show (getDegree highNat)
      (False, 5) -> ""
      (False, _) -> show (getDegree highNat)

majorNatural :: Int -> HighestNatural
majorNatural deg = HighestNatural { isMajor = True, getDegree = deg }

nonMajorNatural :: Int -> HighestNatural
nonMajorNatural deg = HighestNatural { isMajor = False, getDegree = deg }
