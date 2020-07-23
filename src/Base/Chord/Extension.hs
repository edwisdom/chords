module Base.Chord.Extension
  ( Extension
  , sharp
  , flat
  , degree
  , extSign ) where

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

extSign :: Extension -> Int
extSign (ExtSharp _) = 1
extSign (ExtFlat _)  = -1
