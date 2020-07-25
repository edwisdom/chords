module Base.Interval
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
  ) where

import Base.Core.Accidental
import Base.Core.Note
import Base.Core.Quality.IQuality

import Base.Chord.Root

import Base.IQuality

import Base.PitchClass

import Common.Utils (modByFrom)

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

normalizeIntervalSize :: Int -> Int
normalizeIntervalSize = modByFrom 7 1

-- This smart constructor normalizes the interval size to be between 1 and 7
buildInterval :: Quality -> Int -> Interval
buildInterval q s = Interval { getQuality = q, getSize = normalizeIntervalSize s }

normalizeInterval :: Interval -> Interval
normalizeInterval (Interval iQual i) = Interval iQual $ normalizeIntervalSize i

intervalToDistance :: Interval -> Maybe Int
intervalToDistance int@(Interval q i) =
  subIntervalToDistance $ normalizeInterval int
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

    subIntervalToDistance _ = Nothing

lowestAbsValue :: Int -> Int
lowestAbsValue = modByFrom 12 (-6)


invert :: Interval -> Interval
invert (Interval iQual i) =
  let
    newI = normalizeIntervalSize $ 9 - normalizeIntervalSize i
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
(|+|) :: Interval -> Interval -> Interval
int1@(Interval q1 i1) |+| int2@(Interval q2 i2) =
  let
    newI       = i1 + i2 - 1
    defQual    = baseQuality newI
    currDist   = fromJust $ intervalToDistance (Interval defQual newI)
    wantedDist = fromJust (intervalToDistance int1)
               + fromJust (intervalToDistance int2)
    diff       = lowestAbsValue $ wantedDist - currDist
  in
    Interval defQual newI <+> diff

infixl 6 |-|
(|-|) :: Interval -> Interval -> Interval
int1 |-| int2 = normalizeInterval $ int1 |+| invert int2

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