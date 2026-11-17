{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hSetEncoding, hSetBuffering, hSetEcho, stdout, stdin, utf8, BufferMode(..))
import System.Exit (exitFailure)
import Layoutz (runAppWith, defaultAppOptions, AppOptions(..), AppAlignment(..))

import TUI (logicSimApp, runPostSequence, runShutdownSequence, getInitialSize)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    (w, h) <- getInitialSize

    let minW = 76
        minH = 24

    if w < minW || h < minH
        then do
            putStrLn "\ESC[31mSYSTEM HALT: TERMINAL GEOMETRY EXCEPTION\ESC[0m"
            putStrLn $ "REQUIRED BOUNDING BOX : " ++ show minW ++ "x" ++ show minH
            putStrLn $ "CURRENT GEOMETRY      : " ++ show w ++ "x" ++ show h
            putStrLn "\nPlease resize your terminal window and restart the system."
            exitFailure
        else do
            putStr "\ESC[?25l"
            putStr "\ESC[?1049h"
            putStr "\ESC[?7l"
            putStr "\ESC[2J\ESC[H"

            hSetBuffering stdout NoBuffering
            runPostSequence

            -- Stay on the alt screen after the app exits so the shutdown
            -- sequence doesn't clear the user's primary screen.
            runAppWith defaultAppOptions
                { optAlignment   = AppAlignCenter
                , optClearOnExit = False
                } (logicSimApp w h)

            hSetBuffering stdout NoBuffering
            putStr "\ESC[?25l"
            runShutdownSequence

            putStr "\ESC[?7h"
            putStr "\ESC[?1049l"
            putStr "\ESC[?25h"
