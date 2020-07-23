module Main where

import Lib
import Parser
import CanonicalChord
import Control.Monad (unless)

main :: IO ()
main =
  do
    chordStr <- getLine
    unless (chordStr == "q") $ readEvalPrint chordStr >> main

readEvalPrint :: String -> IO ()
readEvalPrint chordStr =
  do
    _ <- print chordStr
    let xs = Parser.parseChord chordStr
    _ <- print xs
    _ <- print $ CanonicalChord.canonicalizeChord <$> xs
    print $ show . Lib.chordToNotes . CanonicalChord.canonicalizeChord <$> xs
