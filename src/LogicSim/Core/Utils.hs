module LogicSim.Core.Utils
    ( bit2intSigned
    , bit2intUnsigned
    , bit2string
    , int2bit
    , isNegative
    , parseBinaryString
    , unfold
    ) where

import LogicSim.Core.Bit (Bit(..))

bit2intSigned :: [Bit] -> Int
bit2intSigned [] = 0
bit2intSigned bits =
    let unsigned = bit2intUnsigned (init bits)
        msb = last bits
    in if msb == One then unsigned - (2 ^ (length bits - 1)) else unsigned

bit2intUnsigned :: [Bit] -> Int
bit2intUnsigned = foldr (\b acc -> acc * 2 + case b of Zero -> 0; One -> 1) 0 . reverse

bit2string :: Bit -> String
bit2string Zero = "0"
bit2string One  = "1"

int2bit :: Int -> Bit
int2bit 0 = Zero
int2bit _ = One

isNegative :: [Bit] -> Bool
isNegative [] = False
isNegative bits = last bits == One

parseBinaryString :: String -> [Bit]
parseBinaryString = map (\c -> if c == '1' then One else Zero)

unfold :: Int -> Int -> [Bit]
unfold 0 _ = []
unfold n val = int2bit (val `mod` 2) : unfold (n - 1) (val `div` 2)
