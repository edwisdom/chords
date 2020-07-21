module Parser
    ( parseChord
    , parserChord
    , parse
      ) where

import Chord

import Text.Parsec (option, optionMaybe, (<|>), parse, many, eof)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Combinator (many1)
import Text.Parsec.Char (string, char, oneOf, digit)
import Text.Parsec.Prim (try)

import Data.Maybe (fromMaybe)

parseChord :: String -> Maybe Chord
parseChord s =
  case parse parserChord "" s of
    Left err  -> Nothing
    Right xs  -> Just xs


(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

parserAccidental :: Parser Accidental
parserAccidental =
  option AccNatural $ parser '#' AccSharp <||> parser 'b' AccFlat
  where
    parser :: Char -> (Int -> Accidental) -> Parser Accidental
    parser symbol constructor =
      do
        cs <- many1 $ char symbol
        return $ constructor (length cs)

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
        major <- optionMaybe (string "Maj" <||> string "M" <||> string "^")
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
        number <- optionMaybe (many1 digit)
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
