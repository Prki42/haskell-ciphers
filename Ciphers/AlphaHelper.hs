module Ciphers.AlphaHelper where

import           Data.Char  (chr, isAlpha, isAscii, ord, toLower, toUpper)
import           Data.Maybe (mapMaybe)

isAsciiAlpha :: Char -> Bool
isAsciiAlpha x = isAlpha x && isAscii x

alphaToInt :: Char -> Maybe Int
alphaToInt x
    | isAsciiAlpha x = Just $ ord (toUpper x) - ord 'A'
    | otherwise = Nothing

intToAlpha :: Int -> Maybe Char
intToAlpha x
    | x >= 0 && x < 26 = Just $ chr $ ord 'A' + x
    | otherwise = Nothing

filterString :: String -> [[Int]]
filterString = map (mapMaybe alphaToInt) . words

matchDim :: Foldable t => [t a1] -> [a2] -> [[a2]]
matchDim (x:xs) y = y1 : matchDim xs y2
    where (y1,y2) = splitAt (length x) y
matchDim [] _ = []
