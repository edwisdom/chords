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

{-# LANGUAGE FlexibleInstances #-}

module Base.Class.Invertible
  ( Invertible (..)
  ) where

import Data.Set as S

-- | The class of types admitting a sensible notion of inversion. There is only
-- one method on this typeclass, @invert@, which satisfies the law:
--
-- There exists @n > 0@ such that @invert@ composed with itself @n@ times is
-- the identity function.
class Invertible a where
  invert :: a -> a

instance (Functor f, Invertible a) => Invertible (f a) where
  invert = fmap invert

instance (Ord a, Invertible a) => Invertible (Set a) where
  invert = S.map invert
