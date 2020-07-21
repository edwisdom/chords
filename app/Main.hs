module Main where

import Lib
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
    let xs = parseChord chordStr
    _ <- print xs
    _ <- print $ Lib.canonicalizeChord <$> xs
    print $ show <$> Lib.chordToNotes <$> Lib.canonicalizeChord <$> xs
