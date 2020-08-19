module ChordSubs
  ( remove5
  , extend1
  , negativeNote
  ) where 



import Base.Chord
import Base.Chord.Chord as C
import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Root
import Base.Chord.Sus

import Base.Core.Quality.CQuality as CQ
import Base.Core.Quality.IQuality as IQ

import Base.Interval hiding (getQuality)
import qualified Base.Interval as I(getQuality)

import Lib(chordToIntervals, chordToNotes)
import Data.Set(Set(..), toList, delete)
import Data.Maybe(fromJust)
import Base.PitchClass(rootToPitchClass, pitchClass)



remove5 :: (Chord, [Root]) -> (Chord, [Root])
remove5 (chord, _) = (chord, (toNotes (getChordRoot chord) (delete (intervalFrom Perfect 5) (chordToIntervals chord))))
  where
    toNotes :: Root -> Set Interval -> [Root]
    toNotes root intSet = flip jumpIntervalFromNote root <$> toList intSet


extend1 :: (Chord, [Root]) -> (Chord, [Root])
extend1 (chord, _) =  (newChord, chordToNotes newChord)
  where
    newChord = chordFrom (getChordRoot chord) (C.getQuality chord)
               (extendHighestNat (getHighestNatural chord))
               (getExtensions chord) (getSus chord)
    extendHighestNat :: HighestNatural -> HighestNatural
    extendHighestNat highNat = 
      if isMajor highNat then
        majorNatural (getDegree highNat + 2)
      else 
        nonMajorNatural (getDegree highNat + 2)

negativeNote :: Root -> Root -> Root
negativeNote key note = 
  let 
    origInt = intervalBetweenNotes key note
    newNum = normalizeIntervalSize $ 6 - getSize origInt
    newDist = (7 - (fromJust $ intervalToDistance origInt)) `mod` 12
    baseInt = intervalFrom (baseQuality newNum) newNum
    newInt = baseInt <+> (newDist - (fromJust (intervalToDistance baseInt)))
  in 
    jumpIntervalFromNote newInt key

