{-|
Module      : Language.Parser
Description : Parses chord symbols from raw strings
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions that parse user-entered
raw strings into chord symbols.
-}
module Language.Parser
  ( parseChord
  , parserChord
  , parse
  ) where

import Data.Maybe ( isJust, fromMaybe )

import Base.Core.Accidental
import Base.Core.Quality.CQuality
import Base.Chord

import Base.Chord.Extension
import Base.Chord.HighestNatural
import Base.Chord.RawChord
import Base.Chord.Note
import Base.Chord.Sus

import Common.Utils (rightToMaybe)

import Text.Parsec
    ( choice
    , option
    , optionMaybe
    , (<|>)
    , (<?>)
    , parse
    , many
    , many1
    , unexpected
    , eof
    )
import Text.Parsec.String (Parser) -- TODO: We're gonna want to do better than
                                   --       this... Fortunately that won't
                                   --       break anything, if we're careful.
import Text.Parsec.Char (string, char, oneOf, digit)
import Text.Parsec.Prim (try)

-- Returns a chord symbol from a string if possible. If not, returns Nothing.
parseChord :: String -> Maybe Chord
parseChord s = rightToMaybe $ parse parserChord "" s

-- Combine two parsers in a specific order so that if they
-- have shared prefixes, one of them is used first.
-- N.B. This should only be used when it's absolutely necessary
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

-- Returns a parser for accidentals. Note that
-- the natural symbol is not accounted for.
parserAccidental :: Parser Accidental
parserAccidental =
  option natural (parseAllOf '#' <|> parseAllOf 'b')
  where
    parseAllOf :: Char -> Parser Accidental
    parseAllOf acc = do let
                          constructor = case acc of
                                          '#' -> nSharps
                                          'b' -> nFlats
                        accs <- many1 $ char acc
                        return $ constructor $ length accs

-- Returns a parser for a note by first
-- parsing the letter and then the accidental.
parserNote :: Parser Note
parserNote = do letterChar <- oneOf "ABCDEFG"
                let
                  letter = read [letterChar]
                noteFrom letter <$> parserAccidental

-- Returns a parser for chord qualities.
-- TODO: Add Maj and min as options
parserQuality :: Parser Quality
parserQuality = choice $ parseQualfromString <$> qualStrings
  where
    qualStrings =
      [ ("^",   Major)
      , ("M",   Major)
      , ("-",   Minor)
      , ("m",   Minor)
      , ("dim", Diminished)
      , ("o",   Diminished)
      , ("aug", Augmented)
      , ("+",   Augmented)
      ]

    parseQualfromString :: (String, Quality) -> Parser Quality
    parseQualfromString (qualName, quality) = string qualName >> return quality

-- Returns a parser for the highest natural degree.
parserHighestNatural :: Parser HighestNatural
parserHighestNatural =
  do
    constructor <- parseMajToConstructor
    digits <- many1 digit
    return $ constructor $ read digits
  where
    parseMajToConstructor :: Parser (Int -> HighestNatural)
    parseMajToConstructor =
      do major <- optionMaybe $ string "Maj" <||> string "M" <|> string "^"
         return $ if isJust major then
                    majorNatural
                  else
                    nonMajorNatural

-- Returns a parser for the sus component.
parserSus :: Parser Sus
parserSus =
  do msus <- optionMaybe parserSusPresent
     return (maybe noSus (maybe susNoNum sus) msus)
  where
    parserSusPresent :: Parser (Maybe Int)
    parserSusPresent =
      do string "sus"
         number <- optionMaybe $ many1 digit
         return $ read <$> number

-- Returns a parser for chord extensions.
parserExtension :: Parser Extension
parserExtension =
  do sharpOrFlat <- parserSf
     number <- many1 digit
     return $ sharpOrFlat $ read number
  where
    parserSf :: Parser (Int -> Extension)
    parserSf =
      do sf <- string "b" <||> string "#" <|> string "add"
         return $ if sf == "b" then flat
                  else if sf == "#" then sharp
                  else add

-- Combines all the previous parsers to fully parse a ChordSymbol
parserChord :: Parser Chord
parserChord =
  do note <- parserNote
     mqual <- optionMaybe parserQuality
     highestQual <- option (nonMajorNatural 5) parserHighestNatural
     exts <- many parserExtension
     sus <- parserSus
     eof
     return $ chordFrom note mqual highestQual exts sus
