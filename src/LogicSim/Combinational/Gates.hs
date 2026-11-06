module LogicSim.Combinational.Gates
    ( nand
    , not
    , and
    , or
    , xor
    , nandN
    , andN
    , orN
    , xorN
    ) where

import Prelude hiding (not, and, or)
import LogicSim.Core.Bit (Bit(..))

nand :: Bit -> Bit -> Bit
nand One One = Zero
nand _   _   = One

not :: Bit -> Bit
not a = nand a a

and :: Bit -> Bit -> Bit
and a b = not (nand a b)

or :: Bit -> Bit -> Bit
or a b = nand (not a) (not b)

xor :: Bit -> Bit -> Bit
xor a b = and (or a b) (nand a b)

nandN :: [Bit] -> Bit
nandN [] = One
nandN [x] = not x
nandN (x:xs) = nand x (nandN xs)

andN :: [Bit] -> Bit
andN = not . nandN

orN :: [Bit] -> Bit
orN [] = Zero
orN (x:xs) = or x (orN xs)

xorN :: [Bit] -> Bit
xorN = foldl xor Zero
