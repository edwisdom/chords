module Lib
  ( parseChord
  , parserChord
  , parse
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
import CanonicalChord
import Interval (Interval(..), IQuality(..), (<+>), (<->))
import Interval (defaultIQuality, jumpIntervalFromNote, intervalToDistance)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map, insert, fromList, toList, (!), delete, (!?))
import Data.Maybe (fromJust)
import Debug.Trace


chordToNotes :: Chord -> [Root]
chordToNotes chord@(Chord root _ _ _ _) = 
  (flip jumpIntervalFromNote root) <$> (S.toList $ chordToIntervals chord)


chordToIntervals :: Chord -> Set Interval 
chordToIntervals (Chord root quality highNat exts sus) =
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



  

