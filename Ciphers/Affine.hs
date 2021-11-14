module Ciphers.Affine (affineEncrypt, affineDecrypt) where

import           Ciphers.AlphaHelper (alphaToInt, intToAlpha, isAsciiAlpha)
import           Data.Maybe          (fromJust, isNothing, mapMaybe)

import           Data.Map            (Map)
import qualified Data.Map            as Map

mod26Inverses :: Map Int Int
mod26Inverses = Map.fromList [(1,1),(3,9),(5,21),(7,15),(9,3),(11,19),(15,7),(17,23),(19,11),(21,5),(23,17),(25,25)]

affineEncrypt :: Int -> Int -> String -> Maybe String
affineEncrypt a b msg
    | Map.notMember a mod26Inverses = Nothing
    | otherwise = Just (
        unwords
        $ map(
            mapMaybe (intToAlpha . (flip mod 26 . (+b) . (*a)))
            . mapMaybe alphaToInt
        )
        $ words msg
    )

affineDecrypt :: Int -> Int -> String -> Maybe String
affineDecrypt a b msg
    | isNothing aInvLookup = Nothing
    | otherwise = affineEncrypt aInv (negate aInv*b) msg
    where
        aInvLookup = Map.lookup a mod26Inverses
        aInv = fromJust aInvLookup
