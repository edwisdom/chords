module Base.Interval.Interval
  ( Interval
  , getQuality
  , getSize
  , buildInterval
  , intervalToDistance
  , (<+>)
  , (<->)
  , jumpIntervalFromNote
  , (|+|)
  , (|-|)
  , modShift
  ) where

import Base.Core.Accidental
import Base.Core.Note
import Base.Core.Quality.IQuality

import Base.Chord.Root

import Base.IQuality

import Base.PitchClass

import Data.Maybe (fromJust)

data Interval = Interval { getQuality :: Quality
                         , getSize :: Int
                         }

instance Eq Interval where
  int1 == int2 = intervalToDistance int1 == intervalToDistance int2

instance Ord Interval where
  int1 `compare` int2 = intervalToDistance int1 `compare` intervalToDistance int2

instance Show Interval where
  show (Interval iQual i) =
    let
      qualString =
        case iQual of
          Major          -> "M"
          Minor          -> "m"
          Perfect        -> "P"
          (Diminished x) -> show x ++ "dim"
          (Augmented x)  -> show x ++ "aug"
    in
      qualString ++ show i

buildInterval :: Quality -> Int -> Interval
buildInterval q s = Interval { getQuality = q, getSize = ((s - 1) `mod` 7) + 1}

modShift :: Int -> Int -> (Int -> Int)
modShift newZero modulus i = ((i - newZero) `mod` modulus) + newZero

modShift' :: Int -> Int -> Int -> Int
modShift' y m x = (x `mod` m) + y + 1

intMod :: Int -> Int
intMod = modShift 1 7

intervalMod :: Interval -> Interval
intervalMod (Interval iQual i) = Interval iQual $ intMod i

intervalToDistance :: Interval -> Maybe Int
intervalToDistance int@(Interval q i) =
  subIntervalToDistance $ intervalMod int
  where
    subIntervalToDistance (Interval Perfect 1) = Just 0
    subIntervalToDistance (Interval Major 2)   = Just 2
    subIntervalToDistance (Interval Major 3)   = Just 4
    subIntervalToDistance (Interval Perfect 4) = Just 5
    subIntervalToDistance (Interval Perfect 5) = Just 7
    subIntervalToDistance (Interval Major 6)   = Just 9
    subIntervalToDistance (Interval Major 7)   = Just 11

    subIntervalToDistance (Interval Minor i) =
      let
        defQuality = baseQuality i
      in
        case defQuality of
          Major   -> subtract 1 <$> subIntervalToDistance (Interval Major i)
          Perfect -> Nothing
          _        -> error "Impl error, default quality must be M or P"


    subIntervalToDistance (Interval (Augmented x) i) =
      let
        defQuality = baseQuality i
      in
        (x +) <$> subIntervalToDistance (Interval defQuality i)


    subIntervalToDistance (Interval (Diminished x) i) =
      let
        defQuality = baseQuality i
      in
        case defQuality of
          Major   -> subtract (x + 1) <$> subIntervalToDistance (Interval Major i)
          Perfect -> subtract x <$> subIntervalToDistance (Interval Perfect i)
          _        -> error "Impl error, default quality must be M or P"

    subIntervalToDistance _ = Nothing

lowestAbsValue :: Int -> Int
lowestAbsValue = modShift (-6) 12


invert :: Interval -> Interval
invert (Interval iQual i) =
  let
    newI = intMod $ 9 - intMod i
    newQual =
      case iQual of
        Major          -> Minor
        Minor          -> Major
        Perfect        -> Perfect
        (Augmented x)  -> Diminished x
        (Diminished x) -> Augmented x
  in
    Interval newQual newI

infixl 6 <+>
(<+>) :: Interval -> Int -> Interval
(Interval iQual i) <+> x =
  Interval (iterate modFunc iQual !! abs x) i
  where
    modFunc =
      case (baseQuality i, signum x) of
        (Perfect, 1)  -> raisePerfect
        (Major, 1)    -> raiseMajor
        (Perfect, -1) -> lowerPerfect
        (Major, -1)   -> lowerMajor
        (_, 0)         -> id


infixl 6 <->
(<->) :: Interval -> Int -> Interval
interval <-> x =  interval <+> (-x)

infixl 6 |+|
(|+|) = intervalAdd

infixl 6 |-|
(|-|) = intervalSubtract

intervalAdd :: Interval -> Interval -> Interval
intervalAdd int1@(Interval q1 i1) int2@(Interval q2 i2) =
  let
    newI       = i1 + i2 - 1
    defQual    = baseQuality newI
    currDist   = fromJust $ intervalToDistance (Interval defQual newI)
    wantedDist = fromJust (intervalToDistance int1)
               + fromJust (intervalToDistance int2)
    diff       = lowestAbsValue $ wantedDist - currDist
  in
    Interval defQual newI <+> diff


intervalSubtract :: Interval -> Interval -> Interval
intervalSubtract int1 int2 = intervalMod $ intervalAdd int1 $ invert int2

jumpIntervalFromNote :: Interval -> Root -> Root
jumpIntervalFromNote (Interval iQual iNum) r =
  let
    newNote    = nextNthNote (getRoot r) $ iNum - 1
    currDist   = getPitchClass (rootToPitchClass $ rootFrom newNote natural)
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
