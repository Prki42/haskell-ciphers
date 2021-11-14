module Ciphers.Atbash where

import           Ciphers.AlphaHelper (alphaToInt, intToAlpha, isAsciiAlpha)
import           Data.Maybe          (mapMaybe)

atbashTransform :: String -> String
atbashTransform =
    unwords
    . map (
        mapMaybe(intToAlpha . abs . (+ negate 25))
        . mapMaybe alphaToInt
    )
    . words
