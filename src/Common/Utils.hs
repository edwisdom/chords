module Common.Utils
  ( rightToMaybe
  ) where

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right b) = Just b