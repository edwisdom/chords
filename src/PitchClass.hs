module PitchClass
  ( rootToPitchClass
  , pitchClassToInt
  ) where

import Chord



data PitchClass = PitchClass Int
  deriving (Show, Eq, Ord)


pitchClass :: Int -> PitchClass
pitchClass i = PitchClass (i `mod` 12)


pitchClassToInt :: PitchClass -> Int
pitchClassToInt (PitchClass i) = i


shiftPitchClassBy :: Int -> PitchClass -> PitchClass
shiftPitchClassBy by (PitchClass i) = pitchClass (by + i)


accidentalToModifier :: Accidental -> Int
accidentalToModifier (AccSharp i) = i
accidentalToModifier (AccFlat i) = -i
accidentalToModifier AccNatural = 0


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
  shiftPitchClassBy (accidentalToModifier acc) (noteToPitchClass note) 