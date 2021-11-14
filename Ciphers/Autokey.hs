module Ciphers.Autokey (autokeyEncrypt, autokeyDecrypt) where

import           Ciphers.AlphaHelper (filterString, intToAlpha, matchDim)
import           Ciphers.Vigenere    (vigenereEncrypt, vigenereOpDec)
import           Data.Maybe          (mapMaybe)

genPsw :: String -> String -> String
genPsw psw msg =
    mapMaybe intToAlpha
    $ concat
    $ filterString psw ++ filterString msg

autokeyEncrypt :: String -> String -> Maybe String
autokeyEncrypt "" _    = Nothing
autokeyEncrypt psw msg = vigenereEncrypt (genPsw psw msg) msg

autokeyDecryptRec :: Integral a => Int -> [a] -> [[a]] -> [[a]]
autokeyDecryptRec baseLen psw c
    | pswLen >= cLen = state
    | otherwise = autokeyDecryptRec baseLen (basePsw ++ take pswLen (concat state)) c
  where
    (basePsw,_) = splitAt baseLen psw
    pswLen = length psw
    cLen = sum $ map length c
    state = vigenereOpDec (matchDim c psw) c

autokeyDecrypt :: String -> String -> Maybe String
autokeyDecrypt "" _ = Nothing
autokeyDecrypt psw c =
    Just $ unwords
    $ map (mapMaybe intToAlpha)
    $ autokeyDecryptRec (length filteredPsw) filteredPsw filteredC
  where
    filteredC = filterString c
    filteredPsw =
        concat
        $ filterString psw
