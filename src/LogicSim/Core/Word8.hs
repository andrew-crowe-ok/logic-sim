module LogicSim.Core.Word8
    ( Word8(..)
    , toList
    , fromListPadded
    , bitwiseNot
    , negateWord8
    , word8ToIntUnsigned
    , word8ToIntSigned
    , intToWord8
    ) where

import LogicSim.Core.Bit (Bit(..))
import LogicSim.Core.Classes (Arithmetic(..))
import qualified LogicSim.Combinational.Gates as G
import LogicSim.Combinational.Circuits (fullAdder)

data Word8 = Word8 !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit
    deriving (Eq)

instance Show Word8 where
    show (Word8 b0 b1 b2 b3 b4 b5 b6 b7) =
        concatMap show [b7, b6, b5, b4, b3, b2, b1, b0]

toList :: Word8 -> [Bit]
toList (Word8 b0 b1 b2 b3 b4 b5 b6 b7) = [b0, b1, b2, b3, b4, b5, b6, b7]

fromListPadded :: [Bit] -> Word8
fromListPadded xs =
    case xs ++ repeat Zero of
        (b0:b1:b2:b3:b4:b5:b6:b7:_) -> Word8 b0 b1 b2 b3 b4 b5 b6 b7
        _                           -> Word8 Zero Zero Zero Zero Zero Zero Zero Zero

bitwiseNot :: Word8 -> Word8
bitwiseNot (Word8 b0 b1 b2 b3 b4 b5 b6 b7) =
    Word8 (G.not b0) (G.not b1) (G.not b2) (G.not b3)
          (G.not b4) (G.not b5) (G.not b6) (G.not b7)

instance Arithmetic Word8 where
    addWithCarry (Word8 a0 a1 a2 a3 a4 a5 a6 a7)
                 (Word8 b0 b1 b2 b3 b4 b5 b6 b7) c0 =
        let (s0, c1) = fullAdder a0 b0 c0
            (s1, c2) = fullAdder a1 b1 c1
            (s2, c3) = fullAdder a2 b2 c2
            (s3, c4) = fullAdder a3 b3 c3
            (s4, c5) = fullAdder a4 b4 c4
            (s5, c6) = fullAdder a5 b5 c5
            (s6, c7) = fullAdder a6 b6 c6
            (s7, c8) = fullAdder a7 b7 c7
        in (Word8 s0 s1 s2 s3 s4 s5 s6 s7, c8)

    add a b = fst (addWithCarry a b Zero)
    sub a b = fst (addWithCarry a (bitwiseNot b) One)

negateWord8 :: Word8 -> Word8
negateWord8 w = fst (addWithCarry (bitwiseNot w) (intToWord8 0) One)

bitVal :: Bit -> Int
bitVal Zero = 0
bitVal One  = 1

word8ToIntUnsigned :: Word8 -> Int
word8ToIntUnsigned w =
    sum $ zipWith (\b p -> bitVal b * (2 ^ p)) (toList w) [0 :: Int .. 7]

word8ToIntSigned :: Word8 -> Int
word8ToIntSigned (Word8 b0 b1 b2 b3 b4 b5 b6 b7) =
    let lower = sum $ zipWith (\b p -> bitVal b * (2 ^ p)) [b0, b1, b2, b3, b4, b5, b6] [0 :: Int .. 6]
        msb   = -(bitVal b7 * 128)
    in msb + lower

intToWord8 :: Int -> Word8
intToWord8 n =
    let normalized = n `mod` 256
        extract p  = if (normalized `div` (2 ^ p)) `mod` 2 == 1 then One else Zero
    in Word8 (extract (0 :: Int)) (extract (1 :: Int)) (extract (2 :: Int)) (extract (3 :: Int))
             (extract (4 :: Int)) (extract (5 :: Int)) (extract (6 :: Int)) (extract (7 :: Int))
