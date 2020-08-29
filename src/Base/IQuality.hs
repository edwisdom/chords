{-|
Module      : Base.IQuality
Description : Functions to modify interval qualities
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions to modify interval qualities
of any base quality type and in any direction.
-}
module Base.IQuality
  ( raisePerfect
  , raiseMajor
  , lowerPerfect
  , lowerMajor
  ) where

import Base.Core.Quality.IQuality

-- Given an interval quality, this raises that quality by a semitone
-- assuming that its base quality is Perfect.
raisePerfect :: Quality -> Quality
raisePerfect Perfect = Augmented 1
raisePerfect (Augmented x)  = Augmented $ x + 1
raisePerfect (Diminished 1) = Perfect
raisePerfect (Diminished x) = Diminished $ x - 1

-- Given an interval quality, this raises that quality by a semitone
-- assuming that its base quality is Major.
raiseMajor :: Quality -> Quality
raiseMajor Major = Augmented 1
raiseMajor (Augmented x)  = Augmented $ x + 1
raiseMajor Minor          = Major
raiseMajor (Diminished 1) = Minor
raiseMajor (Diminished x) = Diminished $ x - 1

-- Given an interval quality, this lowers that quality by a semitone
-- assuming that its base quality is Perfect.
lowerPerfect :: Quality -> Quality
lowerPerfect Perfect        = Diminished 1
lowerPerfect (Diminished x) = Diminished $ x + 1
lowerPerfect (Augmented 1)  = Perfect
lowerPerfect (Augmented x)  = Augmented $ x-1

-- Given an interval quality, this lowers that quality by a semitone
-- assuming that its base quality is Major.
lowerMajor :: Quality -> Quality
lowerMajor Major          = Minor
lowerMajor Minor          = Diminished 1
lowerMajor (Diminished x) = Diminished $ x + 1
lowerMajor (Augmented 1)  = Major
lowerMajor (Augmented x)  = Augmented $ x - 1
