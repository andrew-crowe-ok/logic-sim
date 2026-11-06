module LogicSim.Sequential.Machine
    ( MachineState(..)
    , initMachine
    , tickClock
    ) where

import Control.Monad.State.Strict (State, modify)
import LogicSim.Core.Bit (Bit(..))
import LogicSim.Core.Word8 (Word8, intToWord8)
import LogicSim.Core.Classes (Sequential(..))
import LogicSim.Sequential.Memory (Register8(..))
import LogicSim.Combinational.ALU (ALUOp(..), ALUOutput, computeALU)

data MachineState = MachineState
    { regA      :: !Register8
    , regB      :: !Register8
    , aluOp     :: !ALUOp
    , lastALU   :: !ALUOutput
    , clockTick :: !Int
    } deriving (Eq, Show)

initMachine :: MachineState
initMachine =
    let zeroW = intToWord8 0
    in MachineState
        { regA      = Register8 zeroW
        , regB      = Register8 zeroW
        , aluOp     = OpAdd
        , lastALU   = computeALU OpAdd zeroW zeroW
        , clockTick = 0
        }

tickClock :: Word8 -> Word8 -> Bit -> State MachineState ()
tickClock busA busB clk = modify $ \s ->
    let regA'@(Register8 valA) = step (busA, clk) (regA s)
        regB'@(Register8 valB) = step (busB, clk) (regB s)
        out                    = computeALU (aluOp s) valA valB
    in s { regA      = regA'
         , regB      = regB'
         , lastALU   = out
         , clockTick = clockTick s + 1
         }
