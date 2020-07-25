module Base.CQuality
  ( canonicalizeQuality
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.HighestNatural

canonicalizeQuality :: Maybe Quality -> HighestNatural -> Quality
canonicalizeQuality Nothing  hn = if getDegree hn < 7 then Major else Dominant
canonicalizeQuality (Just q) _  = q
