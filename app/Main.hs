module Main where

import Language.Parser
import Base.Chord.Symbol
import Base.Class.Rooted
import Control.Monad (unless)

main :: IO ()
main =
  do chordStr <- getLine
     unless (chordStr == "q") $ readEvalPrint chordStr >> main

readEvalPrint :: String -> IO ()
readEvalPrint chordStr =
  do print chordStr
     let xs = parseChord chordStr
     print xs
     print $ canonicalizeChord <$> xs
     print $ show . toNotes . canonicalizeChord <$> xs
