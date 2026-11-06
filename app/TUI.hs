{-# LANGUAGE OverloadedStrings #-}
module TUI 
    ( logicSimApp
    , runPostSequence
    , runShutdownSequence
    , getInitialSize
    , module TUI.State
    , module TUI.Update
    , module TUI.View
    ) where

import Layoutz (LayoutzApp(..), Cmd(..))
import Control.Concurrent (threadDelay)
import qualified System.Console.Terminal.Size as Term (size, Window(width, height))

import LogicSim.Core.Bit (Bit(..))
import LogicSim.Core.Word8 (intToWord8)
import TUI.State
import TUI.Update
import TUI.View

getInitialSize :: IO (Int, Int)
getInitialSize = do
    sz <- Term.size
    case sz of
        Just w  -> return (Term.width w, Term.height w)
        Nothing -> return (80, 24)

logicSimApp :: Int -> Int -> LayoutzApp AppModel AppAction
logicSimApp w h = LayoutzApp
    { appInit = (AppModel
        { byteA          = intToWord8 0
        , byteB          = intToWord8 0
        , strA           = "0"
        , strB           = "0"
        , mode           = UnsignedAdd
        , focus          = NumA
        , displayBase    = Dec
        , showHelp       = False
        , frame          = 0
        , lastInputFrame = 0
        , logLines       = []
        , decayA         = replicate 8 0
        , decayB         = replicate 8 0
        , decayALU       = replicate 8 0
        , aluDisplayBits = replicate 8 Zero
        , termW          = w
        , termH          = h
        }, CmdNone)
    , appUpdate = handleAction
    , appSubscriptions = handleSubs
    , appView = renderView
    }

runPostSequence :: IO ()
runPostSequence = do
    let delayMs :: Int -> IO ()
        delayMs ms = threadDelay (ms * 1000)
        
        typeStr :: String -> IO ()
        typeStr s = mapM_ (\c -> putStr [c] >> delayMs 15) s
        
        dotWait :: Int -> IO ()
        dotWait n = mapM_ (\_ -> putStr "." >> delayMs 200) [1..n]
    
    putStr "\ESC[32m" -- ANSI Green
    typeStr "VECTRONIX SYSTEMS BIOS v1.04\n"
    delayMs 500
    
    typeStr "0x0000 MEMORY CHECK "
    dotWait 8
    putStrLn " OK [640K]"
    delayMs 300
    
    typeStr "0x001A INITIALIZING I/O BUS "
    dotWait 5
    putStrLn " OK"
    delayMs 300
    
    typeStr "0x002F MOUNTING LOGIC PROCESSOR "
    dotWait 3
    putStrLn " OK"
    delayMs 500
    
    typeStr "SYSTEM READY. TRANSFERRING CONTROL TO FRONT PANEL"
    dotWait 5
    putStrLn "\n"
    putStr "\ESC[0m" -- ANSI Reset

runShutdownSequence :: IO ()
runShutdownSequence = do
    let delayMs :: Int -> IO ()
        delayMs ms = threadDelay (ms * 1000)
        
        typeStr :: String -> IO ()
        typeStr s = mapM_ (\c -> putStr [c] >> delayMs 15) s

    putStr "\ESC[2J\ESC[H\ESC[32m" -- Clear screen, cursor home, ANSI Green
    typeStr "INITIATING SYSTEM HALT...\n"
    delayMs 300
    typeStr "0x00F0 FLUSHING REGISTERS.......... OK\n"
    delayMs 300
    typeStr "0x00FA DISCONNECTING I/O BUS....... OK\n"
    delayMs 300
    typeStr "0x00FF POWER DOWN COMPLETE.\n"
    delayMs 800
    putStr "\ESC[0m" -- ANSI Reset
