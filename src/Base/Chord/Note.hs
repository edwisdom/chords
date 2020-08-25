module Base.Chord.Note
  ( Note
  , getLetter
  , getAcc
  , noteFrom
  , noteToPitchClass
  , respell
  ) where

import Base.Core.Accidental
import Base.Core.Letter
import Base.PitchClass


data Note = Note { getLetter :: Letter
                 , getAcc :: Accidental
                 }
  deriving (Eq)

noteFrom :: Letter -> Accidental -> Note
noteFrom letter acc = Note { getLetter = letter, getAcc = acc }

instance Show Note where
  show (Note letter acc) = show letter ++ show acc

noteToPitchClass :: Note -> PitchClass
noteToPitchClass r =
  shiftPitchClassBy (impliedShift $ getAcc r) (letterToPitchClass $ getLetter r)

respell :: Note -> Note
respell (Note letter acc) =
  let
    accInt = impliedShift acc
    nextLetterDist = letterToPitchClass (nextNthLetter letter 1) @-@ letterToPitchClass letter
    prevLetterDist = letterToPitchClass letter @-@ letterToPitchClass (prevNthLetter letter 1)
  in
    if accInt > 0 && accInt >= nextLetterDist then
      respell $ Note (nextNthLetter letter 1) (shiftToAcc (accInt - nextLetterDist))
    else if accInt < 0 && -accInt >= prevLetterDist then
      respell $ Note (prevNthLetter letter 1) (shiftToAcc (accInt + prevLetterDist))
    else
      Note letter acc
