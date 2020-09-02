module Base.Scale.Extension
  ( Extension
  , scaleExtensionFrom
  , getAccidental
  , getDegree
  ) where

import Base.Core.Accidental

data Extension = Extension { getAccidental :: Accidental
                           , getDegree :: Int
                           }

instance Show Extension where
  show ext = show (getAccidental ext) ++ show (getDegree ext)

scaleExtensionFrom :: Accidental -> Int -> Extension
scaleExtensionFrom = Extension
