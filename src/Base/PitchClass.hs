module Base.PitchClass
  ( pitchClass
  , PitchClass
  , letterToPitchClass
  , shiftPitchClassBy
  , getPitchClass
  , (@-@)
  , (@+@)
  ) where

import Base.Core.Letter
import Base.Core.Accidental


newtype PitchClass = PitchClass { getPitchClass :: Int }
  deriving (Show, Eq, Ord)

pitchClass :: Int -> PitchClass
pitchClass i = PitchClass $ i `mod` 12

shiftPitchClassBy :: Int -> PitchClass -> PitchClass
shiftPitchClassBy by (PitchClass i) = pitchClass $ by + i

infixl 6 @-@
(@-@) :: PitchClass -> PitchClass -> Int
(PitchClass p1) @-@ (PitchClass p2) = (p1 - p2) `mod` 12

infixl 6 @+@
(@+@) :: PitchClass -> Int -> PitchClass
(PitchClass p) @+@ i = pitchClass (p + i)


letterToPitchClass :: Letter -> PitchClass
letterToPitchClass C = pitchClass 0
letterToPitchClass D = pitchClass 2
letterToPitchClass E = pitchClass 4
letterToPitchClass F = pitchClass 5
letterToPitchClass G = pitchClass 7
letterToPitchClass A = pitchClass 9
letterToPitchClass B = pitchClass 11



