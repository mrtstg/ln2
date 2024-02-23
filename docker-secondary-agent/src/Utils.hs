module Utils (formatString') where

import           Data.String.Utils (strip)

formatSymbols :: [Char]
formatSymbols = "\n\t "

formatString' :: String -> String
formatString' = foldr f "" . strip where
  f x acc = (if x /= '\t' then x else ' '):if x `elem` formatSymbols then dropWhile (`elem` formatSymbols) acc else acc
