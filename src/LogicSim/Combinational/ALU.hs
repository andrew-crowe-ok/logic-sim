module LogicSim.Combinational.ALU
    ( ALUOp(..)
    , ALUFlags(..)
    , ALUOutput(..)
    , computeALU
    ) where

import LogicSim.Core.Bit (Bit(..))
import LogicSim.Core.Word8 (Word8(..), bitwiseNot, toList)
import LogicSim.Core.Classes (Arithmetic(..))
import qualified LogicSim.Combinational.Gates as G

data ALUOp = OpAdd | OpSub | OpAnd | OpOr | OpXor | OpNotA
    deriving (Eq, Show)

data ALUFlags = ALUFlags
    { flagCarry    :: !Bit
    , flagZero     :: !Bit
    , flagNegative :: !Bit
    , flagOverflow :: !Bit
    } deriving (Eq, Show)

data ALUOutput = ALUOutput
    { aluResult :: !Word8
    , aluFlags  :: !ALUFlags
    } deriving (Eq, Show)

computeALU :: ALUOp -> Word8 -> Word8 -> ALUOutput
computeALU op a@(Word8 _ _ _ _ _ _ _ a7) b@(Word8 _ _ _ _ _ _ _ b7) =
    let (res@(Word8 _ _ _ _ _ _ _ r7), cOut) = case op of
            OpAdd  -> addWithCarry a b Zero
            OpSub  -> addWithCarry a (bitwiseNot b) One
            OpAnd  -> (zipWord8 G.and a b, Zero)
            OpOr   -> (zipWord8 G.or  a b, Zero)
            OpXor  -> (zipWord8 G.xor a b, Zero)
            OpNotA -> (bitwiseNot a, Zero)
        zFlag = G.not (G.orN (toList res))
        nFlag = r7
        effB7 = case op of
            OpSub -> G.not b7
            _     -> b7
        vFlag = case op of
            OpAdd -> G.and (G.not (G.xor a7 effB7)) (G.xor a7 r7)
            OpSub -> G.and (G.not (G.xor a7 effB7)) (G.xor a7 r7)
            _     -> Zero
    in ALUOutput res (ALUFlags cOut zFlag nFlag vFlag)

zipWord8 :: (Bit -> Bit -> Bit) -> Word8 -> Word8 -> Word8
zipWord8 f (Word8 a0 a1 a2 a3 a4 a5 a6 a7) (Word8 b0 b1 b2 b3 b4 b5 b6 b7) =
    Word8 (f a0 b0) (f a1 b1) (f a2 b2) (f a3 b3)
          (f a4 b4) (f a5 b5) (f a6 b6) (f a7 b7)
