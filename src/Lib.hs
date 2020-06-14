module Lib
  ( parseChord
  , parserChord
  , parse
  , canonicalizeChord
  , extendIntervals
  , highestNaturalToIntervals
  , intervalsToPitchClasses
  , qualityToScaleMap
  , chordToPitchClasses
  , pitchClassesToString
  ) where

import Parser
import Chord
import qualified CanonicalChord as CC
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map, insert, fromList, toList, (!), delete)

pitchClassesToString :: Set PitchClass -> String
pitchClassesToString pitchClasses =
  show $ pitchClassToInt <$> S.toList pitchClasses

chordToPitchClasses :: CC.Chord -> Set PitchClass
chordToPitchClasses (CC.Chord root quality highNat exts sus) =
  let
    natInts = highestNaturalToIntervals highNat
    intervals = susIntervals (extendIntervals natInts exts) sus
    scale = qualityToScaleMap quality
  in
    intervalsToPitchClasses intervals scale

intervalsToPitchClasses :: Map Int CC.Accidental -> Map Int Int -> Set PitchClass
intervalsToPitchClasses intervals scale =
  S.fromList $ intervalToPitchClass <$> toList intervals
  where
    intervalToPitchClass :: (Int, CC.Accidental) -> PitchClass
    intervalToPitchClass (i, acc) =
      pitchClass $ scale ! ((i - 1) `mod` 7 + 1) + ccaccidentalToModifier acc

qualityToScaleMap :: Quality -> Map Int Int
qualityToScaleMap QMajor = fromList [(1,0), (2,2), (3,4), (4,6), (5,7), (6,9), (7,11)]
qualityToScaleMap QMinor = fromList [(1,0), (2,2), (3,3), (4,5), (5,7), (6,9), (7,10)]
qualityToScaleMap QDominant = fromList [(1,0), (2,2), (3,4), (4,5), (5,7), (6,9), (7,10)]
qualityToScaleMap QAugmented = fromList [(1,0), (2,2), (3,4), (4,6), (5,8), (6,9), (7,10)]
qualityToScaleMap QDiminished = fromList [(1,0), (2,2), (3,3), (4,5), (5,6), (6,8), (7,9)]

susIntervals :: Map Int CC.Accidental -> Sus -> Map Int CC.Accidental
susIntervals intervals NoSus = intervals
susIntervals intervals (Sus i) = insert i CC.AccNatural $ delete 3 intervals

extendIntervals :: Map Int CC.Accidental -> [Extension] -> Map Int CC.Accidental
extendIntervals =
  foldr (\ext -> \intervals -> insert (extToInt ext) (extToAcc ext) intervals) 
  where
    extToAcc :: Extension -> CC.Accidental
    extToAcc (ExtSharp i) = CC.AccSharp
    extToAcc (ExtFlat i) = CC.AccFlat
    extToInt :: Extension -> Int
    extToInt (ExtSharp i) = i
    extToInt (ExtFlat i) = i

highestNaturalToIntervals :: HighestNatural -> Map Int CC.Accidental
highestNaturalToIntervals (HighestNatural major i) =
  fromList $ (\k -> (k, computeAcc k)) <$> intervals
  where
    intervals =
      if i `mod` 2 == 0 then
        [1, 3, 5, i]
      else
        [1,3..i]
    computeAcc :: Int -> CC.Accidental
    computeAcc i =
      case major of
        Major ->
          if i == 7 then
            CC.AccSharp
          else
            CC.AccNatural
        NonMajor -> CC.AccNatural

data PitchClass = PitchClass Int
  deriving (Show, Eq, Ord)

pitchClass :: Int -> PitchClass
pitchClass i = PitchClass (i `mod` 12)

pitchClassToInt :: PitchClass -> Int
pitchClassToInt (PitchClass i) = i


shiftPitchClassBy :: Int -> PitchClass -> PitchClass
shiftPitchClassBy by (PitchClass i) = pitchClass (by + i)

pitchClassToRoot :: PitchClass -> CC.Root
pitchClassToRoot (PitchClass i) = pitch i
  where
    pitch 0 = CC.Root C CC.AccNatural
    pitch 1 = CC.Root D CC.AccFlat
    pitch 2 = CC.Root D CC.AccNatural
    pitch 3 = CC.Root E CC.AccFlat
    pitch 4 = CC.Root E CC.AccNatural
    pitch 5 = CC.Root F CC.AccNatural
    pitch 6 = CC.Root G CC.AccFlat
    pitch 7 = CC.Root G CC.AccNatural
    pitch 8 = CC.Root A CC.AccFlat
    pitch 9 = CC.Root A CC.AccNatural
    pitch 10 = CC.Root B CC.AccFlat
    pitch 11 = CC.Root B CC.AccNatural
    _ = error "Pitch class out of bounds"

canonicalizeChord :: Chord -> CC.Chord
canonicalizeChord (Chord root mqual highNat ext sus) =
  let ccroot = canonicalizeRoot root
      ccqual = canonicalizeQuality mqual highNat
  in
    CC.Chord ccroot ccqual highNat ext sus

canonicalizeQuality :: (Maybe Quality) -> HighestNatural -> Quality
canonicalizeQuality Nothing (HighestNatural _ i) = if i < 7 then QMajor else QDominant
canonicalizeQuality (Just q) _ = q

canonicalizeRoot :: Root -> CC.Root
canonicalizeRoot (Root note acc) =
  pitchClassToRoot $ shiftPitchClassBy (accidentalToModifier acc) (noteToPitchClass note)

accidentalToModifier :: Accidental -> Int
accidentalToModifier (AccSharp i) = i
accidentalToModifier (AccFlat i) = -i
accidentalToModifier AccNatural = 0

ccaccidentalToModifier :: CC.Accidental -> Int
ccaccidentalToModifier CC.AccSharp = 1
ccaccidentalToModifier CC.AccFlat = -1
ccaccidentalToModifier CC.AccNatural = 0


noteToPitchClass :: Note -> PitchClass
noteToPitchClass C = pitchClass 0
noteToPitchClass D = pitchClass 2
noteToPitchClass E = pitchClass 4
noteToPitchClass F = pitchClass 5
noteToPitchClass G = pitchClass 7
noteToPitchClass A = pitchClass 9
noteToPitchClass B = pitchClass 11

