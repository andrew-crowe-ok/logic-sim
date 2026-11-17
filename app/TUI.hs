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
import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (BufferMode(..), hReady, hSetBuffering, hWaitForInput, stdin, stdout)
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
        }, CmdRun (Nothing <$ hSetBuffering stdout (BlockBuffering Nothing)))
        -- layoutz forces stdout to NoBuffering, which makes every frame go out
        -- one byte per write(2). Block buffering + its per-frame hFlush sends
        -- each frame in a handful of writes instead.
    , appUpdate = handleAction
    , appSubscriptions = handleSubs
    , appView = renderView
    }

-- | Sleep up to @ms@, returning early on a keypress. Once a key has been
-- pressed the flag stays set and every later pause is a no-op, so the rest of
-- the sequence prints instantly.
pause :: IORef Bool -> Int -> IO ()
pause skipRef ms = do
    skipped <- readIORef skipRef
    unless skipped $ do
        hit <- hWaitForInput stdin ms `catch` \e -> const (pure False) (e :: IOException)
        when hit $ drainInput >> writeIORef skipRef True
  where
    drainInput = do
        ready <- hReady stdin `catch` \e -> const (pure False) (e :: IOException)
        when ready (getChar >> drainInput)

typeStr :: IORef Bool -> String -> IO ()
typeStr ref = mapM_ (\c -> putStr [c] >> pause ref 15)

-- | Boot animation. Any key skips to the end.
runPostSequence :: IO ()
runPostSequence = do
    ref <- newIORef False
    let dotWait n = mapM_ (\_ -> putStr "." >> pause ref 200) [1 .. n :: Int]

    putStr "\ESC[32m" -- ANSI Green
    typeStr ref "VECTRONIX SYSTEMS BIOS v1.04\n"
    pause ref 500

    typeStr ref "0x0000 MEMORY CHECK "
    dotWait 8
    putStrLn " OK [640K]"
    pause ref 300

    typeStr ref "0x001A INITIALIZING I/O BUS "
    dotWait 5
    putStrLn " OK"
    pause ref 300

    typeStr ref "0x002F MOUNTING LOGIC PROCESSOR "
    dotWait 3
    putStrLn " OK"
    pause ref 500

    typeStr ref "SYSTEM READY. TRANSFERRING CONTROL TO FRONT PANEL"
    dotWait 5
    putStrLn "\n"
    putStr "\ESC[0m" -- ANSI Reset

-- | Power-down animation. Any key skips to the end.
runShutdownSequence :: IO ()
runShutdownSequence = do
    ref <- newIORef False
    putStr "\ESC[2J\ESC[H\ESC[32m" -- Clear screen, cursor home, ANSI Green
    typeStr ref "INITIATING SYSTEM HALT...\n"
    pause ref 300
    typeStr ref "0x00F0 FLUSHING REGISTERS.......... OK\n"
    pause ref 300
    typeStr ref "0x00FA DISCONNECTING I/O BUS....... OK\n"
    pause ref 300
    typeStr ref "0x00FF POWER DOWN COMPLETE.\n"
    pause ref 500
    putStr "\ESC[0m" -- ANSI Reset
