{-# LANGUAGE DeriveGeneric #-}
module Base.Core.Quality.IQuality
  ( Quality(..)
  , baseQuality
  ) where

import GHC.Generics
import Generic.Random
import Test.QuickCheck

import Common.Utils (modByFrom)

data Quality
 = Major
 | Perfect
 | Minor
 | Diminished Int
 | Augmented Int
 deriving (Show, Generic)

instance Arbitrary Quality where
  arbitrary = genericArbitrary uniform

baseQuality :: Int -> Quality
baseQuality n
  | canonicalized `elem` [1, 4, 5]    = Perfect
  | canonicalized `elem` [2, 3, 6, 7] = Major
  where
    canonicalized = modByFrom 7 1 n
