module Main where

import System.Exit (exitFailure, exitSuccess)
import LogicSim.Core.Bit (Bit(..))
import LogicSim.Core.Word8
    ( Word8(..)
    , word8ToIntUnsigned
    , word8ToIntSigned
    , intToWord8
    , negateWord8
    )
import LogicSim.Combinational.Circuits (halfAdder, fullAdder)
import qualified LogicSim.Combinational.Gates as G
import LogicSim.Core.Classes (Arithmetic(..), Sequential(..))
import LogicSim.Sequential.Memory (SrLatch(..), DFlipFlop(..), Register8(..))

main :: IO ()
main = do
    putStrLn "================================================="
    putStrLn "   LOGIC CIRCUIT SIMULATOR : TEST SUITE"
    putStrLn "================================================="
    
    putStrLn "\n[1/5] Verifying Logic Gates..."
    verifyGates
    
    putStrLn "[2/5] Verifying Arithmetic Circuits (Half/Full Adders)..."
    verifyCircuits
    
    putStrLn "[3/5] Verifying Byte & Two's Complement Operations..."
    verifyBytes
    
    putStrLn "[4/5] Verifying Sequential Circuits (Latches & Registers)..."
    verifyMemory
    
    putStrLn "[5/5] Exhaustively Verifying 8-bit Ripple-Carry Addition (65,536 cases)..."
    verifyRippleCarry
    
    putStrLn "\n================================================="
    putStrLn "   ALL VERIFICATION CHECKS PASSED (100% OK)"
    putStrLn "================================================="
    exitSuccess

assert :: String -> Bool -> IO ()
assert name True  = putStrLn $ "  [PASS] " ++ name
assert name False = do
    putStrLn $ "  [FAIL] " ++ name
    exitFailure

verifyGates :: IO ()
verifyGates = do
    -- NAND Truth Table
    assert "NAND 0 0 == 1" (G.nand Zero Zero == One)
    assert "NAND 0 1 == 1" (G.nand Zero One  == One)
    assert "NAND 1 0 == 1" (G.nand One  Zero == One)
    assert "NAND 1 1 == 0" (G.nand One  One  == Zero)

    -- NOT
    assert "NOT 0 == 1" (G.not Zero == One)
    assert "NOT 1 == 0" (G.not One  == Zero)

    -- AND
    assert "AND 0 0 == 0" (G.and Zero Zero == Zero)
    assert "AND 0 1 == 0" (G.and Zero One  == Zero)
    assert "AND 1 0 == 0" (G.and One  Zero == Zero)
    assert "AND 1 1 == 1" (G.and One  One  == One)

    -- OR
    assert "OR 0 0 == 0" (G.or Zero Zero == Zero)
    assert "OR 0 1 == 1" (G.or Zero One  == One)
    assert "OR 1 0 == 1" (G.or One  Zero == One)
    assert "OR 1 1 == 1" (G.or One  One  == One)

    -- XOR
    assert "XOR 0 0 == 0" (G.xor Zero Zero == Zero)
    assert "XOR 0 1 == 1" (G.xor Zero One  == One)
    assert "XOR 1 0 == 1" (G.xor One  Zero == One)
    assert "XOR 1 1 == 0" (G.xor One  One  == Zero)

    -- De Morgan's Laws: NOT (A OR B) == (NOT A) AND (NOT B)
    let allBits = [Zero, One]
        deMorgan1 = and [ G.not (G.or a b) == G.and (G.not a) (G.not b) | a <- allBits, b <- allBits ]
        deMorgan2 = and [ G.not (G.and a b) == G.or (G.not a) (G.not b) | a <- allBits, b <- allBits ]
    assert "De Morgan Law: NOT (A OR B) == (NOT A) AND (NOT B)" deMorgan1
    assert "De Morgan Law: NOT (A AND B) == (NOT A) OR (NOT B)" deMorgan2

verifyCircuits :: IO ()
verifyCircuits = do
    -- Half Adder
    assert "HalfAdder (0, 0) -> (0, 0)" (halfAdder Zero Zero == (Zero, Zero))
    assert "HalfAdder (0, 1) -> (1, 0)" (halfAdder Zero One  == (One,  Zero))
    assert "HalfAdder (1, 0) -> (1, 0)" (halfAdder One  Zero == (One,  Zero))
    assert "HalfAdder (1, 1) -> (0, 1)" (halfAdder One  One  == (Zero, One))

    -- Full Adder: (sOut, cOut) = fullAdder x y cIn
    assert "FullAdder (0, 0, 0) -> (0, 0)" (fullAdder Zero Zero Zero == (Zero, Zero))
    assert "FullAdder (1, 0, 0) -> (1, 0)" (fullAdder One  Zero Zero == (One,  Zero))
    assert "FullAdder (0, 1, 0) -> (1, 0)" (fullAdder Zero One  Zero == (One,  Zero))
    assert "FullAdder (1, 1, 0) -> (0, 1)" (fullAdder One  One  Zero == (Zero, One))
    assert "FullAdder (0, 0, 1) -> (1, 0)" (fullAdder Zero Zero One  == (One,  Zero))
    assert "FullAdder (1, 0, 1) -> (0, 1)" (fullAdder One  Zero One  == (Zero, One))
    assert "FullAdder (0, 1, 1) -> (0, 1)" (fullAdder Zero One  One  == (Zero, One))
    assert "FullAdder (1, 1, 1) -> (1, 1)" (fullAdder One  One  One  == (One,  One))

verifyBytes :: IO ()
verifyBytes = do
    -- Roundtrip signed integers [-128..127]
    let signedOk = and [ word8ToIntSigned (intToWord8 n) == n | n <- [-128..127] ]
    assert "Roundtrip intToWord8 <-> word8ToIntSigned for all [-128..127]" signedOk

    -- Negation property: -(-x) == x for x in [-127..127] (standard two's complement)
    let negOk = and [ word8ToIntSigned (negateWord8 (negateWord8 (intToWord8 n))) == n | n <- [-127..127] ]
    assert "Double negation: negateWord8 (negateWord8 x) == x for all [-127..127]" negOk

    -- Unsigned interpretation: zero and positive ranges
    assert "word8ToIntUnsigned (intToWord8 0) == 0" (word8ToIntUnsigned (intToWord8 0) == 0)
    assert "word8ToIntUnsigned (intToWord8 127) == 127" (word8ToIntUnsigned (intToWord8 127) == 127)
    assert "word8ToIntUnsigned (intToWord8 (-1)) == 255" (word8ToIntUnsigned (intToWord8 (-1)) == 255)

verifyMemory :: IO ()
verifyMemory = do
    -- SR Latch: Set=(1,0)->Q=1, Reset=(0,1)->Q=0, Hold=(0,0)
    let q0 = SrLatch Zero
        qSet = step (One, Zero) q0
        qHold = step (Zero, Zero) qSet
        qReset = step (Zero, One) qHold
    assert "SR Latch Set: S=1, R=0 -> Q=1" (qSet == SrLatch One)
    assert "SR Latch Hold: S=0, R=0 retains Q=1" (qHold == SrLatch One)
    assert "SR Latch Reset: S=0, R=1 -> Q=0" (qReset == SrLatch Zero)

    -- D Flip-Flop: Clock=0 holds state, Clock=1 samples D
    let dff0 = DFlipFlop Zero
        dffIgnored = step (One, Zero) dff0
        dffSampled = step (One, One) dff0
        dffHeld    = step (Zero, Zero) dffSampled
    assert "D Flip-Flop: Clock=0 ignores Data" (dffIgnored == DFlipFlop Zero)
    assert "D Flip-Flop: Clock=1 samples Data" (dffSampled == DFlipFlop One)
    assert "D Flip-Flop: Clock=0 retains sampled Data" (dffHeld == DFlipFlop One)

    -- Register8
    let reg0 = Register8 (intToWord8 0)
        targetByte = intToWord8 42
        regLatched = step (targetByte, One) reg0
        regHeld    = step (intToWord8 99, Zero) regLatched
    assert "Register8: Latches 8-bit byte on Clock=1" (regLatched == Register8 targetByte)
    assert "Register8: Retains 8-bit byte on Clock=0" (regHeld == Register8 targetByte)

verifyRippleCarry :: IO ()
verifyRippleCarry = do
    -- Test all 256 * 256 = 65,536 combinations of unsigned addition
    let allBytes = [ Word8 b0 b1 b2 b3 b4 b5 b6 b7
                   | b0 <- [Zero, One], b1 <- [Zero, One], b2 <- [Zero, One], b3 <- [Zero, One]
                   , b4 <- [Zero, One], b5 <- [Zero, One], b6 <- [Zero, One], b7 <- [Zero, One]
                   ]
        testPair a b =
            let valA = word8ToIntUnsigned a
                valB = word8ToIntUnsigned b
                expected = (valA + valB) `mod` 256
                actual = word8ToIntUnsigned (add a b)
            in actual == expected
        allOk = and [ testPair a b | a <- allBytes, b <- allBytes ]
    assert "All 65,536 unsigned 8-bit addition combinations match (a + b) mod 256" allOk

    -- Subtraction: A - B == A + (-B) for sample range
    let subOk = and [ word8ToIntSigned (sub (intToWord8 a) (intToWord8 b)) ==
                      word8ToIntSigned (add (intToWord8 a) (negateWord8 (intToWord8 b)))
                    | a <- [-50..50], b <- [-50..50]
                    ]
    assert "Arithmetic: sub a b == add a (negateWord8 b) across 10,201 cases" subOk
