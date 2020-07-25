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
  deriving Show

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
