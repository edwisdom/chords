{-|
Module      : Base.CQuality
Description : Exports one canonicalization function on CQuality
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides a function on CQuality that canonicalizes it,
as part of the last step in parsing user-inputted chord symbol strings.
-}
module Base.CQuality
  ( canonicalizeQuality
  ) where

import Base.Core.Quality.CQuality

import Base.Chord.HighestNatural

-- | Given the user-provided quality (which may be Nothing) and the
-- HighestNatural, this infers the quality of the chord and returns it.
canonicalizeQuality :: Maybe Quality -> HighestNatural -> Quality
canonicalizeQuality Nothing  hn = if getDegree hn < 7 then Major else Dominant
canonicalizeQuality (Just q) _  = q
