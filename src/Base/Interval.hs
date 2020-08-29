module Base.Interval
  ( Interval
  , getQuality
  , getSize
  , intervalFrom
  , intervalToDistance
  , (<+>)
  , (<->)
  , jumpIntervalFromNote
  , (|+|)
  , (|-|)
  , invert
  , intervalBetweenNotes
  , modByFrom
  , normalizeIntervalSize
  , getIntWithSize
  ) where

import Base.Class.Invertible

import Base.Core.Accidental
import Base.Core.Letter
import Base.Core.Quality.IQuality

import Base.Chord.Note

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
  show (Interval iQual i) = show iQual ++ show i

normalizeIntervalSize :: Int -> Int
normalizeIntervalSize = modByFrom 7 1

-- TODO: This should probably return an Either String Interval (or an error
-- type in place of String) instead of Maybe Interval, in order to facilitate
-- more sophisticated error handling / reporting.
intervalFrom :: Quality -> Int -> Maybe Interval
intervalFrom q s = if normalizedSize `elem` goodSizes then
                     Just Interval { getQuality = q, getSize = normalizedSize }
                   else
                     Nothing
  where
    normalizedSize :: Int
    normalizedSize = normalizeIntervalSize s

    goodSizes :: [Int]
    goodSizes = case q of
                  Perfect -> [1, 4, 5]
                  Major   -> [2, 3, 6, 7]
                  Minor   -> [2, 3, 6, 7]
                  _       -> [1 .. 7]

normalizeInterval :: Interval -> Interval
normalizeInterval (Interval iQual i) = Interval iQual $ normalizeIntervalSize i

intervalToDistance :: Interval -> Maybe Int
intervalToDistance interval =
  case normalizeInterval interval of
    -- Base interval distances
    Interval Perfect 1 -> Just 0
    Interval Major   2 -> Just 2
    Interval Major   3 -> Just 4
    Interval Perfect 4 -> Just 5
    Interval Perfect 5 -> Just 7
    Interval Major   6 -> Just 9
    Interval Major   7 -> Just 11

    -- Minor shifts
    Interval Minor   i ->
      case baseQuality i of
        Major   -> subtract 1 <$> intervalToDistance (Interval Major i)
        Perfect -> Nothing

    -- Augmented shifts
    Interval (Augmented x) i  ->
      (x +) <$> intervalToDistance (Interval (baseQuality i) i)

    -- Diminished shifts
    Interval (Diminished x) i ->
      subtract offset <$> intervalToDistance (Interval bq i)
      where
        bq = baseQuality i
        offset = case bq of
                   Major   -> x + 1
                   Perfect -> x

    -- Anything else must be an invalid interval
    _ -> Nothing

lowestAbsValue :: Int -> Int
lowestAbsValue = modByFrom 12 (-6)

instance Invertible Interval where
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
Interval iQual i <+> x =
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
interval <-> x = interval <+> (-x)

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

jumpIntervalFromNote :: Interval -> Note -> Note
jumpIntervalFromNote (Interval iQual iNum) r =
  let
    newNote    = nextNthLetter (getLetter r) $ iNum - 1
    currDist   = getPitchClass (noteToPitchClass (noteFrom newNote natural)) - getPitchClass (noteToPitchClass r)
    wantedDist =
      case intervalToDistance $ Interval iQual iNum of
        Just dist -> dist
        Nothing -> error "Invalid interval in jumpIntervalFromNote"
    newAcc     = shiftToAcc $ lowestAbsValue $ wantedDist - currDist
  in noteFrom newNote newAcc

intervalBetweenNotes :: Note -> Note -> Interval
intervalBetweenNotes start end =
  let
    letterDist = modByFrom 7 0 (fromEnum (getLetter end) - fromEnum (getLetter start)) + 1
    currInterval = fromJust $ intervalFrom (baseQuality letterDist) letterDist
    newNote = jumpIntervalFromNote currInterval start
    wantedDist = getPitchClass (noteToPitchClass end) - getPitchClass (noteToPitchClass newNote)
  in
    currInterval <+> modByFrom 12 (- 6) wantedDist

getIntWithSize :: Int -> [Interval] -> Interval
getIntWithSize i intList = head (filter (\ x -> getSize x == i) intList)
