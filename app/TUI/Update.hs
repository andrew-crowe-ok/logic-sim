{-# LANGUAGE OverloadedStrings #-}
module TUI.Update
    ( AppAction(..)
    , handleAction
    , recordActivity
    , handleSubs
    ) where

import Layoutz
    ( Cmd(..)
    , Sub
    , subBatch
    , subKeyPress
    , subEveryMs
    , Key(..)
    )
import LogicSim.Core.Bit (Bit(..))
import LogicSim.Core.Word8
    ( toList
    , intToWord8
    )
import LogicSim.Core.Classes (Arithmetic(add))
import Data.Char (toLower, isDigit, isHexDigit, isOctDigit)

import TUI.State

data AppAction
    = TypeChar Char
    | Backspace
    | CycleFocusForward
    | CycleFocusBackward
    | MoveCursor Int
    | AdjustValue Int
    | ToggleSwitch
    | ToggleMode
    | ToggleBase
    | ToggleHelp
    | AnimTick
    | Quit

handleAction :: AppAction -> AppModel -> (AppModel, Cmd AppAction)
handleAction action model = case action of

    Quit -> (model, CmdExit)

    ToggleHelp ->
        (model { showHelp = not (showHelp model) }, CmdNone)

    AnimTick ->
        let newF = frame model + 1
            actualResBits = toList (add (byteA model) (byteB model))

            rippleRtoL target current = reverse $ stepBit (reverse target) (reverse current)
              where
                stepBit [] [] = []
                stepBit (t:ts) (c:cs)
                  | t == c    = t : stepBit ts cs
                  | otherwise = t : cs
                stepBit _ _ = []

            newAluDisplay = rippleRtoL actualResBits (aluDisplayBits model)

            updateDecay w decays = zipWith (\b d -> if b == One then newF else d) (toList w) decays
            newDecayA = updateDecay (byteA model) (decayA model)
            newDecayB = updateDecay (byteB model) (decayB model)
            newDecayALU = zipWith (\b d -> if b == One then newF else d) newAluDisplay (decayALU model)

        in (model { frame = newF, decayA = newDecayA, decayB = newDecayB, decayALU = newDecayALU, aluDisplayBits = newAluDisplay }, CmdNone)

    CycleFocusForward ->
        let nextFocus = case focus model of
                SwA 0 -> NumA
                SwA i -> SwA (i - 1)
                NumA  -> SwB 7
                SwB 0 -> NumB
                SwB i -> SwB (i - 1)
                NumB  -> SwA 7
        in (model { focus = nextFocus }, CmdNone)

    CycleFocusBackward ->
        let prevFocus = case focus model of
                NumA  -> SwA 0
                SwA 7 -> NumB
                SwA i -> SwA (i + 1)
                NumB  -> SwB 0
                SwB 7 -> NumA
                SwB i -> SwB (i + 1)
        in (model { focus = prevFocus }, CmdNone)

    MoveCursor delta ->
        let m' = case focus model of
                SwA i -> model { focus = SwA (max 0 (min 7 (i + delta))) }
                SwB i -> model { focus = SwB (max 0 (min 7 (i + delta))) }
                _     -> model
        in (m', CmdNone)

    ToggleMode ->
        let newMode = if mode model == UnsignedAdd then SignedAdd else UnsignedAdd
            m' = model { mode = newMode
                       , strA = syncFromByte newMode (displayBase model) (byteA model)
                       , strB = syncFromByte newMode (displayBase model) (byteB model) 
                       }
            logMsg = "> SYS CTRL: ARITHMETIC MODE SET TO " ++ show newMode
        in (recordActivity m' logMsg, CmdNone)

    ToggleBase ->
        let nextBase = if displayBase model == Oct then Dec else succ (displayBase model)
            m' = model { displayBase = nextBase
                       , strA = syncFromByte (mode model) nextBase (byteA model)
                       , strB = syncFromByte (mode model) nextBase (byteB model)
                       }
        in (m', CmdNone)

    ToggleSwitch ->
        let toggle b = if b == One then Zero else One
            getBit idx w = toList w !! idx
        in case focus model of
            SwA i -> let newByte = setBitAt i (toggle (getBit i (byteA model))) (byteA model)
                         m' = model { byteA = newByte, strA = syncFromByte (mode model) (displayBase model) newByte }
                     in (recordActivity m' ("> BUS INTERRUPT: REG A BIT " ++ show i ++ " TOGGLED"), CmdNone)
            SwB i -> let newByte = setBitAt i (toggle (getBit i (byteB model))) (byteB model)
                         m' = model { byteB = newByte, strB = syncFromByte (mode model) (displayBase model) newByte }
                     in (recordActivity m' ("> BUS INTERRUPT: REG B BIT " ++ show i ++ " TOGGLED"), CmdNone)
            _     -> (model, CmdNone)

    AdjustValue delta ->
        case focus model of
            NumA -> let current = getNumericValue (mode model) (byteA model)
                        newVal = current + delta
                        clamped = case mode model of
                            UnsignedAdd -> max 0 (min 255 newVal)
                            SignedAdd   -> max (-128) (min 127 newVal)
                        newByte = intToWord8 clamped
                        m' = model { byteA = newByte, strA = syncFromByte (mode model) (displayBase model) newByte }
                    in (recordActivity m' "> DATA ENTRY: REG A VALUE SHIFTED", CmdNone)
            NumB -> let current = getNumericValue (mode model) (byteB model)
                        newVal = current + delta
                        clamped = case mode model of
                            UnsignedAdd -> max 0 (min 255 newVal)
                            SignedAdd   -> max (-128) (min 127 newVal)
                        newByte = intToWord8 clamped
                        m' = model { byteB = newByte, strB = syncFromByte (mode model) (displayBase model) newByte }
                    in (recordActivity m' "> DATA ENTRY: REG B VALUE SHIFTED", CmdNone)
            SwA i | delta < 0 -> (model { focus = SwB i }, CmdNone)
            SwB i | delta > 0 -> (model { focus = SwA i }, CmdNone)
            _    -> (model, CmdNone)

    TypeChar c ->
        case focus model of
            NumA -> let ns = strA model ++ [c] 
                        m' = model { strA = ns, byteA = syncFromStr ns }
                    in (recordActivity m' "> BUS INTERRUPT: REGISTER STATE ALTERED", CmdNone)
            NumB -> let ns = strB model ++ [c] 
                        m' = model { strB = ns, byteB = syncFromStr ns }
                    in (recordActivity m' "> BUS INTERRUPT: REGISTER STATE ALTERED", CmdNone)
            _    -> (model, CmdNone)

    Backspace ->
        let m' = case focus model of
                NumA -> let ns = if null (strA model) then "" else init (strA model)
                        in model { strA = ns, byteA = syncFromStr ns }
                NumB -> let ns = if null (strB model) then "" else init (strB model)
                        in model { strB = ns, byteB = syncFromStr ns }
                _ -> model
        in (m', CmdNone)

recordActivity :: AppModel -> String -> AppModel
recordActivity m msg = 
    let newLog = take 3 (msg : logLines m)
    in m { lastInputFrame = frame m, logLines = newLog }

handleSubs :: AppModel -> Sub AppAction
handleSubs model = subBatch
    [ subKeyPress $ \key -> case key of
        KeyChar 'q'  -> Just Quit
        KeyEscape    -> if showHelp model then Just ToggleHelp else Just Quit
        KeyChar '?'  -> Just ToggleHelp
        KeyChar '/'  | not isNumActive -> Just ToggleHelp
        KeyChar 'm'  | not isNumActive -> Just ToggleMode
        KeyChar 'h'  | not isNumActive -> Just ToggleBase
        KeyChar ' '  | not isNumActive -> Just ToggleSwitch
        KeyTab       -> Just CycleFocusForward
        KeyChar 'b'  | not isHexActive -> Just CycleFocusBackward
        KeyBackspace -> Just Backspace
        KeyUp        -> Just (AdjustValue 1)
        KeyDown      -> Just (AdjustValue (-1))
        KeyLeft      -> Just (MoveCursor 1)
        KeyRight     -> Just (MoveCursor (-1))
        KeyChar c    | isValidInputChar c -> Just (TypeChar c)
        _            -> Nothing
    , subEveryMs 100 AnimTick
    ]
  where
    isNumActive = focus model `elem` [NumA, NumB]
    isHexActive = isNumActive && displayBase model == Hex
    isValidInputChar c = isNumActive && case displayBase model of
        Hex -> isHexDigit c || toLower c `elem` ("x-" :: String)
        Oct -> isOctDigit c || toLower c `elem` ("o-" :: String)
        Dec -> isDigit c || c == '-'
