module Scale 
  ( Scale(..)
  , scaleToIntervals
  ) where


import Interval (Interval(..), IQuality(..))


data Scale
  = SLydian
  | SDorian
  | SMixolydian
  | SAugmentedQuality
  | SDiminishedQuality
  -- | SMajor
  -- | SMinor
  -- | SPhrygian
  -- | SLocrian    


scaleToIntervals :: Scale -> [Interval]
scaleToIntervals scale =  
  uncurry Interval <$> scaleToTuples scale   
  where
    scaleToTuples :: Scale -> [(IQuality, Int)]
    scaleToTuples SLydian = 
      [(IPerfect, 1), (IMajor, 2), (IMajor, 3), ((IAugmented 1), 4), 
       (IPerfect, 5), (IMajor, 6), (IMajor, 7)]
    scaleToTuples SDorian = 
      [(IPerfect, 1), (IMajor, 2), (IMinor, 3), (IPerfect, 4), 
       (IPerfect, 5), (IMajor, 6), (IMinor, 7)]
    scaleToTuples SMixolydian =
      [(IPerfect, 1), (IMajor, 2), (IMajor, 3), (IPerfect, 4), 
       (IPerfect, 5), (IMajor, 6), (IMinor, 7)]
    scaleToTuples SAugmentedQuality =
      [(IPerfect, 1), (IMajor, 2), (IMajor, 3), ((IAugmented 1), 4), 
       ((IAugmented 1), 5), (IMajor, 6), (IMinor, 7)]
    scaleToTuples SDiminishedQuality = 
      [(IPerfect, 1), (IMajor, 2), (IMinor, 3), (IPerfect, 4), 
       ((IDiminished 1), 5), (IMinor, 6), ((IDiminished 1), 7)]
    -- scaleToTuples _ = error "Not implemented yet"