{-|
Module      : Common.Utils
Description : Provides non-project specific utility functions
Copyright   : (c) Uhhhh
License     : GPL-3
Maintainers : cphifer@galois.com, ejain49@gmail.com
Stability   : experimental
Portability : POSIX

This module exports basic utility functions that don't depend on
with the project's custom types and domain.
-}
module Common.Utils
  ( modByFrom
  , rightToMaybe
  , uncurry3
  , uncurry4
  , uncurry5
  , getIndices
  ) where

-- | Given a modulus and the new lower bound, returns a function
-- to mod a number accordingly.
modByFrom :: Int -- ^ The modulus
          -> Int -- ^ The new ring's lower bound
          -> Int -- ^ The number to mod
          -> Int -- ^ The result
modByFrom m y x = ((x - y) `mod` m) + y

-- | Given an Either type, converts Left to Nothing and Right to a Just value.
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right b) = Just b

-- | Uncurries a function of 3 arguments
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | Uncurries a function of 4 arguments
uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- | Uncurries a function of 5 arguments
uncurry5 :: (a -> b -> c -> d -> e -> g) -> (a, b, c, d, e) -> g
uncurry5 f (a, b, c, d, e) = f a b c d e

-- | Given a set of indices and a list, extracts the elements at those indices.
-- Note that if the indices are out of bounds, this function will panic.
getIndices :: [Int] -> [a] -> [a]
getIndices indices xs = map (xs !!) indices