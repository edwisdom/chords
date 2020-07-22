module Parser
    ( parseChord
    , parserChord
    , parse
      ) where

import Chord
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
    , eof )
import Text.Parsec.String (Parser) -- TODO: We're gonna want to do better than
                                   --       this... Fortunately that won't
                                   --       break anything, if we're careful.
import Text.Parsec.Char (string, char, oneOf, digit)
import Text.Parsec.Prim (try)

import Data.Maybe (fromMaybe)

-- TODO: Let's be a little more robust here
parseChord :: String -> Maybe Chord
parseChord s = rightToMaybe $ parse parserChord "" s

-- N.B. This should only be used when it's absolutely necessary
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

parserAccidental :: Parser Accidental
parserAccidental =
  option AccNatural (parseAllOf '#' <|> parseAllOf 'b')
  where
    parseAllOf :: Char -> Parser Accidental
    parseAllOf acc
      | acc `elem` "#b" = let constructor = case acc of
                                              '#' -> AccSharp
                                              'b' -> AccFlat
                          in do accs <- many1 $ char acc
                                return $ constructor $ length accs
      | otherwise       = unexpected "character"

parserRoot :: Parser Root
parserRoot =
  do
    noteChar <- oneOf "ABCDEFG"
    let note = read [noteChar]
    Root note <$> parserAccidental

parserQuality :: Parser Quality
parserQuality =
       parser "^" QMajor
  <||> parser "M" QMajor
  <||> parser "-" QMinor
  <||> parser "m" QMinor
  <||> parser "dim" QDiminished
  <||> parser "o" QDiminished
  <||> parser "aug" QAugmented
  <||> parser "+" QAugmented
  where
    parser :: String -> Quality -> Parser Quality
    parser literal quality =
      do
        _ <- string literal
        return quality

parserHighestNatural :: Parser HighestNatural
parserHighestNatural =
  do
    constructor <- parserMajor
    digits <- many1 digit
    return $ constructor (read digits)
  where
    parserMajor :: Parser (Int -> HighestNatural)
    parserMajor =
      do
        major <- optionMaybe $ string "Maj" <||> string "M" <||> string "^"
        return $
          case major of
            Just _ -> HighestNatural Major
            Nothing -> HighestNatural NonMajor

parserSus :: Parser Sus
parserSus =
  do
    msus <- optionMaybe parserSusPresent
    let sus = maybe NoSus Sus msus
    return sus
  where
    parserSusPresent :: Parser Int
    parserSusPresent =
      do
        _ <- string "sus"
        number <- optionMaybe $ many1 digit
        return $ maybe 2 read number

parserExtension :: Parser Extension
parserExtension =
  do
    sharpOrFlat <- parserSf
    number <- many1 digit
    return $ sharpOrFlat $ read number
  where
    parserSf :: Parser (Int -> Extension)
    parserSf =
      do
        sf <- oneOf "b#"
        return $ case sf of
          'b' -> ExtFlat
          '#' -> ExtSharp
          _ -> error "Unhandled possiblity in parser"

parserChord :: Parser Chord
parserChord =
  do
    root <- parserRoot
    mqual <- optionMaybe parserQuality
    highestQual <- option (HighestNatural Major 5) parserHighestNatural
    exts <- many parserExtension
    sus <- parserSus
    _ <- eof
    return $ Chord root mqual highestQual exts sus
