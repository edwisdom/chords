module Lib
  ( parseChord
  , parserChord
  , parse
  , canonicalizeChord
  , nExtendIntervals
  , nHighestNaturalToIntervals
  , chordToIntervals
  , chordToNotes
  , nSusIntervals
  , qualityToIntervals
  , intervalToDistance
  , Interval(..)
  , IQuality(..)
  , defaultIQuality
  , jumpIntervalFromNote
  , Scale(..)
  , scaleToIntervals
  , (<+>)
  , (<->)
  ) where

import Parser
import Chord
import qualified CanonicalChord as CC
import PitchClass (rootToPitchClass, pitchClassToInt)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map, insert, fromList, toList, (!), delete, (!?))
import Data.Maybe (fromJust)
import Debug.Trace


chordToNotes :: CC.Chord -> [Root]
chordToNotes chord@(CC.Chord root _ _ _ _) = 
  (flip jumpIntervalFromNote root) <$> (S.toList $ chordToIntervals chord)


chordToIntervals :: CC.Chord -> Set Interval 
chordToIntervals (CC.Chord root quality highNat exts sus) =
  let
    baseScale = nHighestNaturalToIntervals highNat $ qualityToIntervals quality  
    intervals = nSusIntervals (nExtendIntervals baseScale exts) sus
  in 
    foldr (\int -> \intSet -> S.insert int intSet) S.empty intervals


type HeliotonicScale = Map Int Interval


data Scale
  = SLydian
  | SDorian
  | SMixolydian
  | SAugmentedQuality
  | SDiminishedQuality    


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


qualityToIntervals :: Quality -> HeliotonicScale
qualityToIntervals qual = fromList $ zip [1..7] (scaleToIntervals (qualityToScale qual))
  where
    qualityToScale :: Quality -> Scale
    qualityToScale QMajor = SLydian
    qualityToScale QMinor = SDorian
    qualityToScale QDominant = SMixolydian
    qualityToScale QAugmented = SAugmentedQuality
    qualityToScale QDiminished = SDiminishedQuality


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


nSusIntervals :: HeliotonicScale -> Sus -> HeliotonicScale
nSusIntervals scale NoSus = scale
nSusIntervals scale (Sus i) = insert i (Interval (defaultIQuality i) i) $ delete 3 scale

nExtendIntervals :: HeliotonicScale -> [Extension] -> HeliotonicScale
nExtendIntervals scale exts = foldr (\ext -> \scale -> extendInterval scale ext) scale exts 

extendInterval :: HeliotonicScale -> Extension -> HeliotonicScale
extendInterval scale (ExtSharp i) = insert i ((Interval (defaultIQuality i) i) <+> 1) scale
extendInterval scale (ExtFlat i) = insert i ((Interval (defaultIQuality i) i) <-> 1) scale
        

nHighestNaturalToIntervals :: HighestNatural -> HeliotonicScale -> HeliotonicScale
nHighestNaturalToIntervals (HighestNatural major i) scale = 
  insertMajor major $ getIntervals subset scale 
  where 
    subset = 
      if i `mod` 2 == 0 then
        [1, 3, 5, i]
      else 
        [1,3..i]
    getIntervals :: [Int] -> HeliotonicScale -> HeliotonicScale
    getIntervals ints hts = fromList $ map ($ hts) (getInterval <$> ints)
    getInterval :: Int -> HeliotonicScale -> (Int, Interval)
    getInterval int hts = 
      let interval = hts !? ((int-1) `mod` 7 + 1)
      in 
        (int, fromJust $ interval)
    insertMajor :: MajorOrNot -> HeliotonicScale -> HeliotonicScale
    insertMajor Major hts = insert 7 (Interval IMajor 7) hts
    insertMajor notMajor hts = hts


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
  

canonicalizeChord :: Chord -> CC.Chord
canonicalizeChord (Chord root mqual highNat ext sus) =
  let ccqual = canonicalizeQuality mqual highNat
  in
    CC.Chord root ccqual highNat ext sus

canonicalizeQuality :: (Maybe Quality) -> HighestNatural -> Quality
canonicalizeQuality Nothing (HighestNatural _ i) = if i < 7 then QMajor else QDominant
canonicalizeQuality (Just q) _ = q


