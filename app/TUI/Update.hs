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

    -- Tab stops: A value -> A switches -> B value -> B switches.
    -- Arrow keys move between individual switches within a bank.
    CycleFocusForward ->
        let nextFocus = case focus model of
                NumA  -> SwA 7
                SwA _ -> NumB
                NumB  -> SwB 7
                SwB _ -> NumA
        in (model { focus = nextFocus }, CmdNone)

    CycleFocusBackward ->
        let prevFocus = case focus model of
                NumA  -> SwB 7
                SwA _ -> NumA
                NumB  -> SwA 7
                SwB _ -> NumB
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
            modeName = if newMode == SignedAdd then "SIGNED" else "UNSIGNED"
            logMsg = "> SYS CTRL: ARITHMETIC MODE SET TO " ++ modeName
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
        let -- A lone "0" is a placeholder: the first digit replaces it.
            append str = if str == "0" && c /= 'x' && c /= 'o' then [c] else str ++ [c]
            -- Reject keystrokes that don't parse or would leave the register's range,
            -- so the display never disagrees with the stored byte.
            accept ns = length ns <= 5 && maybe False (inputInRange (mode model) (displayBase model)) (parseNumber ns)
        in case focus model of
            NumA | accept (append (strA model)) ->
                     let ns = append (strA model)
                         m' = model { strA = ns, byteA = syncFromStr ns }
                     in (recordActivity m' "> BUS INTERRUPT: REG A DATA ENTRY", CmdNone)
            NumB | accept (append (strB model)) ->
                     let ns = append (strB model)
                         m' = model { strB = ns, byteB = syncFromStr ns }
                     in (recordActivity m' "> BUS INTERRUPT: REG B DATA ENTRY", CmdNone)
            _    -> (model, CmdNone)

    Backspace ->
        let m' = case focus model of
                NumA -> let ns = if null (strA model) then "" else init (strA model)
                        in model { strA = ns, byteA = syncFromStr ns }
                NumB -> let ns = if null (strB model) then "" else init (strB model)
                        in model { strB = ns, byteB = syncFromStr ns }
                _ -> model
        in (m', CmdNone)

-- | Log an event (newest first, max 3), collapsing repeats into a count.
recordActivity :: AppModel -> String -> AppModel
recordActivity m msg =
    let newLog = case logLines m of
            ((prev, n) : rest) | prev == msg -> (prev, n + 1) : rest
            entries                          -> take 3 ((msg, 1) : entries)
    in m { lastInputFrame = frame m, logLines = newLog }

handleSubs :: AppModel -> Sub AppAction
handleSubs model = subBatch
    [ subKeyPress $ \key -> case key of
        KeyChar 'q'  -> Just Quit
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
