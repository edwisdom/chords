module Base.Chord.Chord
  ( Chord
  , chordFrom
  , chordFromSymbol
  , getSymbol
  , updateNotes
  , notesToChord
  ) where

import Base.Chord.ChordShape
import Base.Chord.ChordSymbol
import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.Note
import Base.Chord.Sus

import Base.Core.Quality.CQuality as CQ
import Base.Core.Quality.IQuality as IQ

import Base.Class.Chordal
import Base.Class.Rooted

import Base.Heliotonic

import Base.Interval

import Common.Utils (uncurry4, uncurry5)

import Data.Function (on)
import Data.List (sortBy, zip4, zip5)
import qualified Data.List as L
import Data.Map as M
import Data.Maybe (catMaybes, fromJust)

data Chord = Chord { getSymbol :: ChordSymbol
                   , getNotes :: [Note]
                   } deriving (Eq, Show)

instance Chordal Chord where
  quality = quality . getSymbol
  highestNatural = highestNatural . getSymbol
  extensions = extensions . getSymbol
  suspension = suspension . getSymbol
  toIntervals = toIntervals. getSymbol

instance Rooted Chord where
  root = root . getSymbol
  toNotes = getNotes

chordFrom :: Note -> CQ.Quality -> HighestNatural -> [Extension] -> Sus -> Chord
chordFrom r q hn exts s =
  Chord { getSymbol = chordSymbolFrom r $ chordShapeFrom q hn exts s
        , getNotes = toNotes sym
        }
  where
    sym = chordSymbolFrom r $ chordShapeFrom q hn exts s

chordFromSymbol :: ChordSymbol -> Chord
chordFromSymbol sym = Chord { getSymbol = sym, getNotes = toNotes sym }

updateNotes :: Chord -> [Note] -> Chord
updateNotes chord notes = chord { getNotes = notes }

notesToChord :: [Note] -> [Chord]
notesToChord notes =
  let
    roots :: [Note]
    roots = sortBy (compare `on` noteToPitchClass) notes

    findQuality :: Note -> CQ.Quality
    findQuality root
      | hasInterval IQ.Major 3 && hasInterval IQ.Minor 7 = CQ.Dominant
      | hasInterval IQ.Major 3 && hasInterval (IQ.Augmented 1) 5 = CQ.Augmented
      | hasInterval IQ.Major 3 = CQ.Major
      | hasInterval IQ.Minor 3 && hasInterval IQ.Minor 7 = CQ.Minor
      | hasInterval IQ.Minor 3 && hasInterval (IQ.Diminished 1) 5 = CQ.Diminished
      | hasInterval IQ.Minor 3 = CQ.Minor
      | hasInterval IQ.Minor 7 = CQ.Dominant
      | otherwise = CQ.Major
      where
        notesContainIntervalFromNote :: [Note] -> Note -> Interval -> Bool
        notesContainIntervalFromNote notes key interval =
          jumpIntervalFromNote interval key `elem` notes

        hasInterval :: IQ.Quality -> Int -> Bool
        hasInterval iQual iSize = notesContainIntervalFromNote notes root $ fromJust $ intervalFrom iQual iSize

    qualities :: [CQ.Quality]
    qualities = findQuality <$> roots

    findHighNat :: Note -> CQ.Quality -> HighestNatural
    findHighNat root quality
      |      has 7  &&     (has 9  ||      has 11) &&       has 13 = majorOrNot 13
      |      has 7  &&      has 9  &&      has 11  && not (has 13) = majorOrNot 11
      |      has 7  &&      has 9  && not (has 11) && not (has 13) = majorOrNot 9
      |      has 7                                                 = majorOrNot 7
      | not (has 7) && not (has 9) && not (has 11) &&       has 13 = majorOrNot 6
      |                                                  otherwise = majorOrNot 5
      where
        qInts :: [Interval]
        qInts = catMaybes $ ($ qualityToIntervals quality) <$> (M.lookup <$> [7, 2, 4, 6])

        cInts :: [Interval]
        cInts = intervalBetweenNotes root <$> roots

        majorOrNot :: Int -> HighestNatural
        majorOrNot = if (quality /= CQ.Major) && fromJust (intervalFrom IQ.Major 7) `elem` cInts
                     then majorNatural
                     else nonMajorNatural

        has :: Int -> Bool
        has int =
          let i = normalizeIntervalSize int
          in i `elem` (getSize <$> cInts) && (getIntWithSize i cInts == getIntWithSize i qInts)

    highNats :: [HighestNatural]
    highNats = uncurry findHighNat <$> zip roots qualities

    findSus :: Note -> HighestNatural -> Sus
    findSus root highNat
      | containsThird || getDegree highNat == 6                = noSus
      | getDegree highNat `elem` [9, 11, 13] || (has2 && has4) = susNoNum
      | has2                                                   = sus 2
      | has4                                                   = sus 4
      | otherwise                                              = noSus
      where
        cInts :: [Interval]
        cInts = intervalBetweenNotes root <$> roots

        containsThird :: Bool
        containsThird = 3 `elem` (getSize <$> cInts)

        has2 :: Bool
        has2 = fromJust (intervalFrom IQ.Major 2) `elem` cInts

        has4 :: Bool
        has4 = fromJust (intervalFrom IQ.Perfect 4) `elem` cInts

    chordSuses :: [Sus]
    chordSuses = uncurry findSus <$> zip roots highNats

    findExts :: Note -> CQ.Quality -> HighestNatural -> Sus -> [Extension]
    findExts root quality highNat chordSus =
      let
        qInts :: [Interval]
        qInts = elems $ qualityToIntervals quality

        cInts :: [Interval]
        cInts = intervalBetweenNotes root <$> roots

        numHighNat :: Int
        numHighNat = getDegree highNat

        removableDegs :: [Int]
        removableDegs = L.map normalizeIntervalSize [1, 3 .. numHighNat] ++ ([6 | numHighNat == 6])

        noNatInts :: [Interval]
        noNatInts = L.filter (\x -> not $ (x `elem` qInts) && getSize x `elem` removableDegs) cInts

        noSusInts :: [Interval]
        noSusInts
          | chordSus == sus 2 = L.delete (fromJust (intervalFrom IQ.Major 2)) noNatInts
          | chordSus == sus 4 = L.delete (fromJust (intervalFrom IQ.Perfect 4)) noNatInts
          | (chordSus == susNoNum && numHighNat < 9) = L.delete (fromJust (intervalFrom IQ.Major 2))
                                                     $ L.delete (fromJust (intervalFrom IQ.Perfect 4)) noNatInts
          | otherwise = noNatInts

        intToExt :: Interval -> Extension
        intToExt int
          | getSize int == 5 = distToExt (fromJust $ intervalToDistance $ int |-| getIntWithSize (getSize int) qInts) (getSize int)
          | otherwise = distToExt (fromJust $ intervalToDistance $ int |-| getIntWithSize (getSize int) qInts) (getSize int + 7)
      in
        intToExt <$> noSusInts

    extss :: [[Extension]]
    extss = uncurry4 findExts <$> zip4 roots qualities highNats chordSuses
    sortByExtLen :: [Chord] -> [Chord]
    sortByExtLen = sortBy (compare `on` length . extensions)
  in
    sortByExtLen $ uncurry5 chordFrom <$> zip5 roots qualities highNats extss chordSuses
