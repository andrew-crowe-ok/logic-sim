{-# LANGUAGE TypeFamilies #-}

module LogicSim.Core.Classes
    ( Arithmetic(..)
    , Sequential(..)
    ) where

import LogicSim.Core.Bit (Bit)

class Arithmetic a where
    addWithCarry :: a -> a -> Bit -> (a, Bit)
    add          :: a -> a -> a
    sub          :: a -> a -> a

class Sequential s where
    type Input s
    type Output s
    step   :: Input s -> s -> s
    sample :: s -> Output s
