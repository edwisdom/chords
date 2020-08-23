module Base.Chord.Extension
  ( Extension
  , sharp
  , flat
  , add
  , degree
  , sign
  , distToExt
  ) where

data Extension
  = ExtSharp Int
  | ExtFlat Int
  | ExtAdd Int

instance Show Extension where
  show (ExtSharp x) = "#" ++ show x
  show (ExtFlat x) = "b" ++ show x
  show (ExtAdd x) = "add" ++ show x

sharp :: Int -> Extension
sharp = ExtSharp

flat :: Int -> Extension
flat = ExtFlat

add :: Int -> Extension
add = ExtAdd

degree :: Extension -> Int
degree (ExtSharp deg) = deg
degree (ExtFlat deg)  = deg
degree (ExtAdd deg) = deg

sign :: Extension -> Int
sign (ExtSharp _) = 1
sign (ExtFlat _)  = -1
sign (ExtAdd _)  = 0

distToExt :: Int -> Int -> Extension
distToExt 1 = ExtSharp
distToExt 0 = ExtAdd
distToExt (-1) = ExtFlat