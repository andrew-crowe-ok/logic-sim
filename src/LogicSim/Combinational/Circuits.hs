module LogicSim.Combinational.Circuits
    ( halfAdder
    , fullAdder
    , rippleAddN
    , mux
    , crossbar
    ) where

import LogicSim.Core.Bit (Bit(..))
import qualified LogicSim.Combinational.Gates as G

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b = (G.xor a b, G.and a b)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b cIn =
    let (s1, c1) = halfAdder a b
        (s2, c2) = halfAdder s1 cIn
    in (s2, G.or c1 c2)

rippleAddN :: [Bit] -> [Bit] -> Bit -> ([Bit], Bit)
rippleAddN [] [] cIn = ([], cIn)
rippleAddN (a:as) (b:bs) cIn =
    let (s, cOut) = fullAdder a b cIn
        (rest, finalCarry) = rippleAddN as bs cOut
    in (s : rest, finalCarry)
rippleAddN _ _ cIn = ([], cIn)

mux :: Bit -> Bit -> Bit -> Bit
mux sel a b = G.or (G.and (G.not sel) a) (G.and sel b)

crossbar :: Bit -> (Bit, Bit) -> (Bit, Bit)
crossbar sel (a, b) =
    if sel == Zero
        then (a, b)
        else (b, a)
