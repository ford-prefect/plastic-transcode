module Utils where

import Data.Char (isLower, toLower)

-- Convert InputUri to input-uri
camelToLower :: String -> String
camelToLower = concatMap upperHyphen . lowerFirst
  where
    upperHyphen c       = if isLower c
                          then [c]
                          else ['-', toLower c]
    lowerFirst (x : xs) = toLower x : xs
    lowerFirst c        = map toLower c

