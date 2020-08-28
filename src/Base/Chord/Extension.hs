{-|
Module      : Base.Chord.Extension
Description : Implements Extension datatype (for chords)
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the Extension datatype, its smart constructors,
and functions to convert between Extensions and integers.
-}
module Base.Chord.Extension
  ( Extension
  , sharp
  , flat
  , add
  , degree
  , sign
  , distToExt
  ) where

-- Chord extensions can be sharp, flat, or adds, followed by the degree.
data Extension
  = ExtSharp Int
  | ExtFlat Int
  | ExtAdd Int
  deriving(Eq)

-- Show chord extensions as they are in lead sheets, with #, b, or add.
instance Show Extension where
  show (ExtSharp x) = "#" ++ show x
  show (ExtFlat x) = "b" ++ show x
  show (ExtAdd x) = "add" ++ show x

-- Smart constructor for a sharp extension
sharp :: Int -> Extension
sharp = ExtSharp

-- Smart constructor for a flat extension
flat :: Int -> Extension
flat = ExtFlat

-- Smart constructor for a natural/add extension
add :: Int -> Extension
add = ExtAdd

-- Get the degree from an extension
degree :: Extension -> Int
degree (ExtSharp deg) = deg
degree (ExtFlat deg)  = deg
degree (ExtAdd deg) = deg

-- Get the extension's implied shift in semitones.
sign :: Extension -> Int
sign (ExtSharp _) = 1
sign (ExtFlat _)  = -1
sign (ExtAdd _)  = 0

-- Get an extension from an implied semitone shift.
-- If it's not -1, 0, or 1, this function returns Nothing.
distToExt :: Int -> Maybe (Int -> Extension)
distToExt 1 = Just ExtSharp
distToExt 0 = Just ExtAdd
distToExt (-1) = Just ExtFlat
distToExt _ = Nothing