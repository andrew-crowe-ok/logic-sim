{-# LANGUAGE OverloadedStrings #-}
module TUI.State
    ( AppMode(..)
    , DisplayBase(..)
    , Focus(..)
    , AppModel(..)
    , checkOverflow
    , getNumericValue
    , parseNumber
    , inputInRange
    , syncFromStr
    , syncFromByte
    , setBitAt
    , formatValue
    , largeDigit
    ) where

import LogicSim.Core.Bit (Bit(..))
import LogicSim.Core.Word8
    ( Word8
    , toList
    , fromListPadded
    , word8ToIntUnsigned
    , word8ToIntSigned
    , intToWord8
    )
import Numeric (showHex, showOct, readHex, readOct)
import Data.Char (toLower)
import Data.List (isPrefixOf)

data AppMode = UnsignedAdd | SignedAdd deriving (Eq, Show, Enum, Bounded)

data DisplayBase = Dec | Hex | Oct deriving (Eq, Show, Enum, Bounded)

data Focus = NumA | SwA Int | NumB | SwB Int deriving (Eq, Show)

data AppModel = AppModel
    { byteA          :: Word8
    , byteB          :: Word8
    , strA           :: String
    , strB           :: String
    , mode           :: AppMode
    , focus          :: Focus
    , displayBase    :: DisplayBase
    , showHelp       :: Bool
    , frame          :: Int
    , lastInputFrame :: Int
    , termW          :: Int
    , termH          :: Int
    , logLines       :: [(String, Int)]  -- ^ newest first, with repeat count
    , decayA         :: [Int]
    , decayB         :: [Int]
    , decayALU       :: [Int]
    , aluDisplayBits :: [Bit]
    }

checkOverflow :: AppMode -> Word8 -> Word8 -> Bool
checkOverflow UnsignedAdd a b =
    (word8ToIntUnsigned a + word8ToIntUnsigned b) > 255
checkOverflow SignedAdd a b =
    let valA = word8ToIntSigned a
        valB = word8ToIntSigned b
        sumVal = valA + valB
    in sumVal < -128 || sumVal > 127

getNumericValue :: AppMode -> Word8 -> Int
getNumericValue UnsignedAdd b = word8ToIntUnsigned b
getNumericValue SignedAdd b = word8ToIntSigned b

parseNumber :: String -> Maybe Int
parseNumber raw =
    let s = dropWhile (== ' ') raw
        isNeg = "-" `isPrefixOf` s
        body = if isNeg then drop 1 s else s
        low = map toLower body
    in case () of
        _ | null body -> Just 0
          | low == "0x" || low == "0o" || low == "x" || low == "o" -> Just 0
          | "0x" `isPrefixOf` low -> case readHex (drop 2 low) of
              [(n, "")] -> Just (if isNeg then -n else n)
              _         -> Nothing
          | "0o" `isPrefixOf` low -> case readOct (drop 2 low) of
              [(n, "")] -> Just (if isNeg then -n else n)
              _         -> Nothing
          | otherwise -> case reads s :: [(Int, String)] of
              [(n, "")] -> Just n
              _         -> case readHex low of
                  [(n, "")] -> Just (if isNeg then -n else n)
                  _         -> Nothing

-- | Whether a typed value fits the register. Decimal entry follows the
-- arithmetic mode; hex/octal accept any 8-bit pattern.
inputInRange :: AppMode -> DisplayBase -> Int -> Bool
inputInRange UnsignedAdd Dec n = n >= 0 && n <= 255
inputInRange SignedAdd   Dec n = n >= -128 && n <= 127
inputInRange _           _   n = n >= -128 && n <= 255

syncFromStr :: String -> Word8
syncFromStr str = case parseNumber str of
    Just n  -> intToWord8 n
    Nothing -> intToWord8 0

syncFromByte :: AppMode -> DisplayBase -> Word8 -> String
syncFromByte m b base = formatValue b (getNumericValue m base)

setBitAt :: Int -> Bit -> Word8 -> Word8
setBitAt idx newBit w =
    let bits = toList w
    in case splitAt idx bits of
        (pre, _:post) -> fromListPadded (pre ++ newBit : post)
        _             -> w

formatValue :: DisplayBase -> Int -> String
formatValue Dec n = show n
formatValue Hex n = "0x" ++ showHex (if n < 0 then n + 256 else n) ""
formatValue Oct n = "0o" ++ showOct (if n < 0 then n + 256 else n) ""

largeDigit :: Char -> [String]
largeDigit '0' = [" _ ", "| |", "|_|"]
largeDigit '1' = ["   ", "  |", "  |"]
largeDigit '2' = [" _ ", " _|", "|_ "]
largeDigit '3' = [" _ ", " _|", " _|"]
largeDigit '4' = ["   ", "|_|", "  |"]
largeDigit '5' = [" _ ", "|_ ", " _|"]
largeDigit '6' = [" _ ", "|_ ", "|_|"]
largeDigit '7' = [" _ ", "  |", "  |"]
largeDigit '8' = [" _ ", "|_|", "|_|"]
largeDigit '9' = [" _ ", "|_|", " _|"]
largeDigit '-' = ["   ", " _ ", "   "]
largeDigit '_' = ["   ", "   ", " _ "]
largeDigit 'x' = ["   ", "\\ /", "/ \\"]
largeDigit 'o' = ["   ", " _ ", "|_|"]
largeDigit 'a' = [" _ ", "|_|", "| |"]
largeDigit 'b' = ["   ", "|_ ", "|_|"]
largeDigit 'c' = [" _ ", "|  ", "|_ "]
largeDigit 'd' = ["   ", " _|", "|_|"]
largeDigit 'e' = [" _ ", "|_ ", "|_ "]
largeDigit 'f' = [" _ ", "|_ ", "|  "]
largeDigit ' ' = ["   ", "   ", "   "]
largeDigit _   = ["???", "???", "???"]
