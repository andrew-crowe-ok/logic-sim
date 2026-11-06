{-# LANGUAGE TypeFamilies #-}

module LogicSim.Sequential.Memory
    ( SrLatch(..)
    , DFlipFlop(..)
    , Register8(..)
    ) where

import LogicSim.Core.Bit (Bit(..))
import LogicSim.Core.Word8 (Word8)
import LogicSim.Core.Classes (Sequential(..))
import qualified LogicSim.Combinational.Gates as G

newtype SrLatch = SrLatch Bit
    deriving (Eq, Show)

instance Sequential SrLatch where
    type Input SrLatch = (Bit, Bit)
    type Output SrLatch = Bit
    step (s, r) (SrLatch qPrev) =
        let qNot  = G.not (G.or s qPrev)
            qNext = G.not (G.or r qNot)
        in SrLatch qNext
    sample (SrLatch q) = q

newtype DFlipFlop = DFlipFlop Bit
    deriving (Eq, Show)

instance Sequential DFlipFlop where
    type Input DFlipFlop = (Bit, Bit)
    type Output DFlipFlop = Bit
    step (d, clk) (DFlipFlop q) =
        let qNew = G.or (G.and (G.not clk) q) (G.and clk d)
        in DFlipFlop qNew
    sample (DFlipFlop q) = q

newtype Register8 = Register8 Word8
    deriving (Eq, Show)

instance Sequential Register8 where
    type Input Register8 = (Word8, Bit)
    type Output Register8 = Word8
    step (newVal, clk) (Register8 currentVal) =
        if clk == One then Register8 newVal else Register8 currentVal
    sample (Register8 val) = val
