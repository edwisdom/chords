module Interval 
  ( Interval(..)
  , (<+>)
  , (<->)
  , IQuality(..)
  , defaultIQuality
  , intervalToDistance
  , jumpIntervalFromNote
  ) where


import CanonicalChord (Root(..), Accidental(..), nextNthNote)
import PitchClass (rootToPitchClass, pitchClassToInt)


data Interval 
 = Interval IQuality Int


instance Eq Interval where
  int1 == int2 = intervalToDistance int1 == intervalToDistance int2

instance Ord Interval where
  int1 `compare` int2 = intervalToDistance int1 `compare` intervalToDistance int2

instance Show Interval where
  show (Interval iQual i) = 
    let 
      qualString =
        case iQual of
          IMajor -> "M"
          IMinor -> "m"
          IPerfect -> "P"
          (IDiminished x) -> (show x) ++ "dim"
          (IAugmented x) -> (show x) ++ "aug"
    in
      qualString ++ (show i)


data IQuality 
 = IMajor
 | IPerfect
 | IMinor
 | IDiminished Int
 | IAugmented Int
 deriving Show 


infixl 6 <+>
(<+>) (Interval iQual i) x = 
  Interval (iterate modFunc iQual !! x) i
  where 
    modFunc =
      case (defaultIQuality i, signum x) of
        (IPerfect, 1) -> raisePerfect
        (IMajor, 1) -> raiseMajor
        (IPerfect, -1) -> lowerPerfect
        (IMajor, -1) -> lowerMajor
        (_, 0) -> id


infixl 6 <->
interval <-> x =  interval <+> (-x)


raisePerfect :: IQuality -> IQuality
raisePerfect IPerfect = (IAugmented 1)
raisePerfect (IAugmented x) = (IAugmented (x+1))
raisePerfect (IDiminished 1) = IPerfect
raisePerfect (IDiminished x) = (IDiminished (x-1)) 

raiseMajor :: IQuality -> IQuality
raiseMajor IMajor = (IAugmented 1)
raiseMajor (IAugmented x) = (IAugmented (x+1))
raiseMajor IMinor = IMajor
raiseMajor (IDiminished 1) = IMinor
raiseMajor (IDiminished x) = (IDiminished (x-1))

lowerPerfect :: IQuality -> IQuality
lowerPerfect IPerfect = (IDiminished 1)
lowerPerfect (IDiminished x) = (IDiminished (x+1)) 
lowerPerfect (IAugmented 1) = IPerfect
lowerPerfect (IAugmented x) = (IAugmented (x-1))

lowerMajor :: IQuality -> IQuality
lowerMajor IMajor = IMinor
lowerMajor IMinor = (IDiminished 1)
lowerMajor (IDiminished x) = (IDiminished (x+1))
lowerMajor (IAugmented 1) = IMajor
lowerMajor (IAugmented x) = (IAugmented (x-1))


defaultIQuality :: Int -> IQuality
defaultIQuality i =
  case (i-1) `mod` 7 + 1 of
    intervalInt
      | intervalInt `elem` [1, 4, 5] -> IPerfect
      | intervalInt `elem` [2, 3, 6, 7] -> IMajor
      | otherwise -> error "Impl error, mod 7 issue"
 

intervalToDistance :: Interval -> Maybe Int
intervalToDistance (Interval q i) = 
  subIntervalToDistance (Interval q ((i-1) `mod` 7 + 1))
  where
    subIntervalToDistance (Interval IPerfect 1) = Just 0
    subIntervalToDistance (Interval IMajor 2) = Just 2
    subIntervalToDistance (Interval IMajor 3) = Just 4
    subIntervalToDistance (Interval IPerfect 4) = Just 5
    subIntervalToDistance (Interval IPerfect 5) = Just 7
    subIntervalToDistance (Interval IMajor 6) = Just 9
    subIntervalToDistance (Interval IMajor 7) = Just 11

    subIntervalToDistance (Interval IMinor i) = 
      let defQuality = defaultIQuality i 
      in 
        case defQuality of 
          IMajor -> (subtract 1) <$> subIntervalToDistance (Interval IMajor i)
          IPerfect -> Nothing
          _ -> error "Impl error, default quality must be M or P"


    subIntervalToDistance (Interval (IAugmented x) i) = 
      let defQuality = defaultIQuality i
      in 
        ((+) x) <$> subIntervalToDistance (Interval defQuality i)


    subIntervalToDistance (Interval (IDiminished x) i) = 
      let defQuality = defaultIQuality i
      in 
        case defQuality of 
          IMajor -> (subtract (x + 1)) <$> subIntervalToDistance (Interval IMajor i) 
          IPerfect -> (subtract x) <$> subIntervalToDistance (Interval IPerfect i)
          _ -> error "Impl error, default quality must be M or P"

    subIntervalToDistance _ = Nothing


jumpIntervalFromNote :: Interval -> Root -> Root
jumpIntervalFromNote (Interval iQual iNum) (Root note acc) = 
  let 
    newNote = nextNthNote note (iNum - 1)
    currDist = (pitchClassToInt (rootToPitchClass(Root newNote AccNatural))
              - pitchClassToInt (rootToPitchClass(Root note acc)))
    wantedDist = 
      case intervalToDistance (Interval iQual iNum) of
        Just (dist) -> dist
        Nothing -> error "Invalid interval in jumpIntervalFromNote"
    diff = (((wantedDist - currDist) + 6) `mod` 12) - 6
    newAcc = 
      case (signum diff) of
        1 -> AccSharp diff 
        -1 ->  AccFlat (-diff) 
        0 -> AccNatural 
  in Root newNote newAcc