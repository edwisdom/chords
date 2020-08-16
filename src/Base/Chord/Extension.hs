module Base.Chord.Extension
  ( Extension
  , sharp
  , flat
  , degree
  , sign
  ) where

data Extension
  = ExtSharp Int
  | ExtFlat Int

instance Show Extension where
  show (ExtSharp x) = "#" ++ show x
  show (ExtFlat x) = "b" ++ show x

sharp :: Int -> Extension
sharp = ExtSharp

flat :: Int -> Extension
flat = ExtFlat

degree :: Extension -> Int
degree (ExtSharp deg) = deg
degree (ExtFlat deg)  = deg

sign :: Extension -> Int
sign (ExtSharp _) = 1
sign (ExtFlat _)  = -1
