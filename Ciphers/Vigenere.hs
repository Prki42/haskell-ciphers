module Ciphers.Vigenere (
    vigenereEncrypt, vigenereDecrypt, vigenereOpEnc, vigenereOpDec
) where

import           Ciphers.AlphaHelper (alphaToInt, filterString, intToAlpha,
                                      matchDim)
import           Data.Maybe          (mapMaybe)

vigenereOpEnc :: Integral b => [[b]] -> [[b]] -> [[b]]
vigenereOpEnc psw msg =
    map (map $ flip mod 26)
    $ zipWith (zipWith (+)) psw msg

vigenereOpDec :: Integral b => [[b]] -> [[b]] -> [[b]]
vigenereOpDec psw msg =
    map (map $ flip mod 26 . (+26))
    $ zipWith (zipWith (-)) msg psw

vigenereWrap :: ([[Int]] -> [[Int]] -> [[Int]]) -> String -> String -> Maybe String
vigenereWrap _ "" _ = Nothing
vigenereWrap op psw msg =
    Just $ unwords
    $ map (mapMaybe intToAlpha)
    $ op filteredPsw filteredMsg
  where
    filteredMsg = filterString msg
    filteredPsw =
        matchDim filteredMsg
        $ cycle
        $ concat
        $ filterString psw

vigenereEncrypt :: String -> String -> Maybe String
vigenereEncrypt = vigenereWrap vigenereOpEnc

vigenereDecrypt :: String -> String -> Maybe String
vigenereDecrypt = vigenereWrap vigenereOpDec
