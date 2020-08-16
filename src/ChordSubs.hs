module ChordSubs
  ( remove5
  , extend1
  ) where 



import Base.Chord
import Base.Chord.Chord as C
import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Root
import Base.Chord.Sus

import Base.Core.Quality.CQuality as CQ
import Base.Core.Quality.IQuality as IQ

import Base.Interval

import Lib(chordToIntervals, chordToNotes)
import Data.Set(Set(..), toList, delete)



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