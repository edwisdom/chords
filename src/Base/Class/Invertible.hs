{-|
Module      : Base.Class.Invertible
Description : Type class for types that are invertible
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

The type class @Invertible@ encompasses those types which can undergo a
sensible notion of musical inversion.
-}

module Base.Class.Invertible
  ( Invertible (..)
  ) where

import Data.Set as S

-- | The class of types admitting a sensible notion of inversion. There is only
-- one method on this typeclass, @invert@, which satisfies the law:
--
-- > invert . invert == id
class Invertible a where
  invert :: a -> a

instance (Ord a, Invertible a) => Invertible (Set a) where
  invert = S.map invert
