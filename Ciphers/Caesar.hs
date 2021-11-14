module Ciphers.Caesar where

import Ciphers.AlphaHelper (alphaToInt, intToAlpha, isAsciiAlpha)
import Data.Maybe (catMaybes, mapMaybe)

caesarEncrypt :: Int -> String -> String
caesarEncrypt shift msg =
  unwords $
    map
      ( mapMaybe (intToAlpha . (flip mod 26 . (+ shift)))
          . mapMaybe alphaToInt
      )
      $ words msg

caesarDecrypt :: Int -> String -> String
caesarDecrypt shift = caesarEncrypt (negate shift)
