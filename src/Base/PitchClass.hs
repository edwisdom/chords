module Base.PitchClass
  ( pitchClass
  , rootToPitchClass
  , getPitchClass
  ) where

import Base.Note
import Base.Accidental
import Base.Chord ( Root(..) )

newtype PitchClass = PitchClass { getPitchClass :: Int }
  deriving (Show, Eq, Ord)

pitchClass :: Int -> PitchClass
pitchClass i = PitchClass $ i `mod` 12

shiftPitchClassBy :: Int -> PitchClass -> PitchClass
shiftPitchClassBy by (PitchClass i) = pitchClass $ by + i

noteToPitchClass :: Note -> PitchClass
noteToPitchClass C = pitchClass 0
noteToPitchClass D = pitchClass 2
noteToPitchClass E = pitchClass 4
noteToPitchClass F = pitchClass 5
noteToPitchClass G = pitchClass 7
noteToPitchClass A = pitchClass 9
noteToPitchClass B = pitchClass 11

rootToPitchClass :: Root -> PitchClass
rootToPitchClass (Root note acc) =
  shiftPitchClassBy (countAcc acc) (noteToPitchClass note)
