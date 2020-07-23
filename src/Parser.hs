module Parser
    ( parseChord
    , parserChord
    , parse
      ) where

import Base.Chord
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
parseChord :: String -> Maybe RawChord
parseChord s = rightToMaybe $ parse parserChord "" s

-- N.B. This should only be used when it's absolutely necessary
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

parserAccidental :: Parser Accidental
parserAccidental =
  option AccNatural (parseAllOf '#' <|> parseAllOf 'b')
  where
    parseAllOf :: Char -> Parser Accidental
    parseAllOf acc = do let constructor = case acc of
                                            '#' -> AccSharp
                                            'b' -> AccFlat
                        accs <- many1 $ char acc
                        return $ constructor $ length accs

parserRoot :: Parser Root
parserRoot = do noteChar <- oneOf "ABCDEFG"
                let note = read [noteChar]
                Root note <$> parserAccidental

parserQuality :: Parser Quality
parserQuality = choice $ parseQualfromString <$> qualStrings
  where
    qualStrings = [ ("^",   QMajor)
                  , ("M",   QMajor)
                  , ("-",   QMinor)
                  , ("m",   QMinor)
                  , ("dim", QDiminished)
                  , ("o",   QDiminished)
                  , ("aug", QAugmented)
                  , ("+",   QAugmented )
                  ]

    parseQualfromString :: (String, Quality) -> Parser Quality
    parseQualfromString (qualName, quality) = string qualName >> return quality

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
         return $ HighestNatural $
           case major of
             Just _  -> Major
             Nothing -> NonMajor

parserSus :: Parser Sus
parserSus =
  do msus <- optionMaybe parserSusPresent
     return $ maybe NoSus Sus msus
  where
    parserSusPresent :: Parser Int
    parserSusPresent =
      do string "sus"
         number <- optionMaybe $ many1 digit
         return $ maybe 2 read number

parserExtension :: Parser Extension
parserExtension =
  do sharpOrFlat <- parserSf
     number <- many1 digit
     return $ sharpOrFlat $ read number
  where
    parserSf :: Parser (Int -> Extension)
    parserSf =
      do sf <- oneOf "b#"
         return $ case sf of
                    'b' -> ExtFlat
                    '#' -> ExtSharp

parserChord :: Parser RawChord
parserChord =
  do root <- parserRoot
     mqual <- optionMaybe parserQuality
     highestQual <- option (HighestNatural Major 5) parserHighestNatural
     exts <- many parserExtension
     sus <- parserSus
     eof
     return $ RawChord root mqual highestQual exts sus
