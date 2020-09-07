{-|
Module      : Base.Scale.Extension
Description : Representation of a scale extension
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the representation for a scale extension
along with its smart constructor.
-}
module Base.Scale.Extension
  ( Extension
  , scaleExtensionFrom
  , getAccidental
  , getDegree
  ) where

import Base.Core.Accidental

-- | A scale extension has an accidental and a degree associated with it.
data Extension = Extension { getAccidental :: Accidental
                           , getDegree :: Int
                           }

-- | Show a scale extension by showing the accidental and degree.
instance Show Extension where
  show ext = show (getAccidental ext) ++ show (getDegree ext)

-- | Smart constructor for a scale extension.
-- TODO: Figure out whether this should exclude the AccNatural possibility
scaleExtensionFrom :: Accidental -> Int -> Extension
scaleExtensionFrom = Extension
