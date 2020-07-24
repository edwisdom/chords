module Interval
  ( Interval(..)
  , (<+>)
  , (<->)
  , IQuality(..)
  , defaultIQuality
  , intervalToDistance
  , jumpIntervalFromNote
  , (|+|)
  , (|-|)
  , invert
  ) where

import Base.Core.Accidental
import Base.Core.Note (nextNthNote)
import Base.Chord.Root
import Base.PitchClass (rootToPitchClass, getPitchClass )
import Data.Maybe (fromJust)



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
          IMajor          -> "M"
          IMinor          -> "m"
          IPerfect        -> "P"
          (IDiminished x) -> show x ++ "dim"
          (IAugmented x)  -> show x ++ "aug"
    in
      qualString ++ show i


data IQuality
 = IMajor
 | IPerfect
 | IMinor
 | IDiminished Int
 | IAugmented Int
 deriving Show


infixl 6 <+>
(<+>) (Interval iQual i) x =
  Interval (iterate modFunc iQual !! abs x) i
  where
    modFunc =
      case (defaultIQuality i, signum x) of
        (IPerfect, 1)  -> raisePerfect
        (IMajor, 1)    -> raiseMajor
        (IPerfect, -1) -> lowerPerfect
        (IMajor, -1)   -> lowerMajor
        (_, 0)         -> id


infixl 6 <->
interval <-> x =  interval <+> (-x)


raisePerfect :: IQuality -> IQuality
raisePerfect IPerfect = IAugmented 1
raisePerfect (IAugmented x)  = IAugmented $ x + 1
raisePerfect (IDiminished 1) = IPerfect
raisePerfect (IDiminished x) = IDiminished $ x - 1

raiseMajor :: IQuality -> IQuality
raiseMajor IMajor = IAugmented 1
raiseMajor (IAugmented x)  = IAugmented $ x + 1
raiseMajor IMinor          = IMajor
raiseMajor (IDiminished 1) = IMinor
raiseMajor (IDiminished x) = IDiminished $ x - 1

lowerPerfect :: IQuality -> IQuality
lowerPerfect IPerfect        = IDiminished 1
lowerPerfect (IDiminished x) = IDiminished $ x + 1
lowerPerfect (IAugmented 1)  = IPerfect
lowerPerfect (IAugmented x)  = IAugmented $ x-1

lowerMajor :: IQuality -> IQuality
lowerMajor IMajor          = IMinor
lowerMajor IMinor          = IDiminished 1
lowerMajor (IDiminished x) = IDiminished $ x + 1
lowerMajor (IAugmented 1)  = IMajor
lowerMajor (IAugmented x)  = IAugmented $ x - 1


defaultIQuality :: Int -> IQuality
defaultIQuality i =
  case intMod i of
    intervalInt
      | intervalInt `elem` [1, 4, 5]    -> IPerfect
      | intervalInt `elem` [2, 3, 6, 7] -> IMajor
      | otherwise -> error "Impl error, mod 7 issue"


modShift :: Int -> Int -> (Int -> Int)
modShift newZero modulus i = ((i - newZero) `mod` modulus) + newZero

intMod :: Int -> Int
intMod = modShift 1 7

intervalMod :: Interval -> Interval
intervalMod (Interval iQual i) = Interval iQual $ intMod i

lowestAbsValue :: Int -> Int
lowestAbsValue = modShift (-6) 12


invert :: Interval -> Interval
invert (Interval iQual i) =
  let
    newI = intMod $ 9 - intMod i
    newQual =
      case iQual of
        IMajor          -> IMinor
        IMinor          -> IMajor
        IPerfect        -> IPerfect
        (IAugmented x)  -> IDiminished x
        (IDiminished x) -> IAugmented x
  in
    Interval newQual newI


infixl 6 |+|
(|+|) = intervalAdd

infixl 6 |-|
(|-|) = intervalSubtract

intervalAdd :: Interval -> Interval -> Interval
intervalAdd int1@(Interval q1 i1) int2@(Interval q2 i2) =
  let
    newI       = i1 + i2 - 1
    defQual    = defaultIQuality newI
    currDist   = fromJust $ intervalToDistance (Interval defQual newI)
    wantedDist = fromJust (intervalToDistance int1)
               + fromJust (intervalToDistance int2)
    diff       = lowestAbsValue $ wantedDist - currDist
  in
    Interval defQual newI <+> diff


intervalSubtract :: Interval -> Interval -> Interval
intervalSubtract int1 int2 = intervalMod $ intervalAdd int1 $ invert int2


intervalToDistance :: Interval -> Maybe Int
intervalToDistance int@(Interval q i) =
  subIntervalToDistance $ intervalMod int
  where
    subIntervalToDistance (Interval IPerfect 1) = Just 0
    subIntervalToDistance (Interval IMajor 2)   = Just 2
    subIntervalToDistance (Interval IMajor 3)   = Just 4
    subIntervalToDistance (Interval IPerfect 4) = Just 5
    subIntervalToDistance (Interval IPerfect 5) = Just 7
    subIntervalToDistance (Interval IMajor 6)   = Just 9
    subIntervalToDistance (Interval IMajor 7)   = Just 11

    subIntervalToDistance (Interval IMinor i) =
      let
        defQuality = defaultIQuality i
      in
        case defQuality of
          IMajor   -> subtract 1 <$> subIntervalToDistance (Interval IMajor i)
          IPerfect -> Nothing
          _        -> error "Impl error, default quality must be M or P"


    subIntervalToDistance (Interval (IAugmented x) i) =
      let
        defQuality = defaultIQuality i
      in
        (x +) <$> subIntervalToDistance (Interval defQuality i)


    subIntervalToDistance (Interval (IDiminished x) i) =
      let
        defQuality = defaultIQuality i
      in
        case defQuality of
          IMajor   -> subtract (x + 1) <$> subIntervalToDistance (Interval IMajor i)
          IPerfect -> subtract x <$> subIntervalToDistance (Interval IPerfect i)
          _        -> error "Impl error, default quality must be M or P"

    subIntervalToDistance _ = Nothing


jumpIntervalFromNote :: Interval -> Root -> Root
jumpIntervalFromNote (Interval iQual iNum) r =
  let
    newNote    = nextNthNote (getRoot r) $ iNum - 1
    currDist   = (getPitchClass $ rootToPitchClass $ rootFrom newNote natural)
               - (getPitchClass $ rootToPitchClass r)
    wantedDist =
      case intervalToDistance $ Interval iQual iNum of
        Just dist -> dist
        Nothing -> error "Invalid interval in jumpIntervalFromNote"
    diff   = lowestAbsValue $ wantedDist - currDist
    newAcc =
      case signum diff of
        1  -> nSharp diff
        -1 -> nFlat (-diff)
        0  -> natural
  in rootFrom newNote newAcc
