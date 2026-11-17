{-# LANGUAGE OverloadedStrings #-}
module TUI.View
    ( renderBits
    , renderAluBits
    , renderSwitches
    , renderDigitalDisplay
    , renderModeSwitch
    , renderStatusBar
    , renderSystemLights
    , renderHelpView
    , renderView
    ) where

import Layoutz
import LogicSim.Core.Bit (Bit(..))
import LogicSim.Core.Word8 (toList, fromListPadded)
import LogicSim.Combinational.ALU (ALUOp(..), ALUFlags(..), ALUOutput(..), computeALU)

import TUI.State

-- | Which colour channel an LED bank glows in.
data LedTint = TintRed | TintGreen

tint :: LedTint -> Int -> Color
tint TintRed   v = ColorTrue v 0 0
tint TintGreen v = ColorTrue 0 v 0

-- | Pulsing LEDs; recently-lit ones fade out over a few frames.
renderLeds :: LedTint -> Int -> [Bit] -> [Int] -> L
renderLeds t currentF bits decays = tightRow $ map draw (reverse (zip bits decays))
  where
    pulse :: Double
    pulse = sin (fromIntegral currentF * 0.3)
    lit   :: Int
    lit   = round $ 177 + (78 * pulse)
    frameCol = withColor (ColorTrue 60 60 60)

    draw (bit, lastOnF) =
        let led = case bit of
                One  -> withStyle StyleBold $ withColor (tint t lit) (text "O")
                Zero -> let diff = currentF - lastOnF
                            v | diff <= 1 = 140
                              | diff == 2 = 90
                              | otherwise = 40
                        in withColor (tint t v) (text "O")
        in tightRow [ frameCol (text " ["), led, frameCol (text "] ") ]

renderBits :: Int -> [Bit] -> [Int] -> L
renderBits = renderLeds TintRed

renderAluBits :: Int -> [Bit] -> [Int] -> L
renderAluBits = renderLeds TintGreen

renderSwitches :: Focus -> String -> [Bit] -> L
renderSwitches foc target bits =
    let activeIdx = case foc of { SwA i | target == "A" -> i; SwB i | target == "B" -> i; _ -> -1 }
    in tightRow [ draw activeIdx i b | (i, b) <- zip [7,6..0] (reverse bits) ]
  where
    draw activeIdx i b =
        let isON = b == One
            char = if isON then "█" else "▄"
            col  = if isON then ColorTrue 180 180 180 else ColorTrue 60 60 60
            lBracket = if activeIdx == i then withStyle StyleBold $ withColor ColorBrightWhite (text " [") else withColor (ColorTrue 60 60 60) (text "  ")
            rBracket = if activeIdx == i then withStyle StyleBold $ withColor ColorBrightWhite (text "] ") else withColor (ColorTrue 60 60 60) (text "  ")
            swChar   = if activeIdx == i then withStyle StyleBold $ withColor ColorBrightWhite (text char) else withColor col (text char)
        in tightRow [lBracket, swChar, rBracket]

-- | Three-row seven-segment readout. @side@ supplies up to three widgets
-- drawn to the right of the digits, one per row.
renderDigitalDisplay :: Int -> Bool -> String -> [L] -> [L]
renderDigitalDisplay currentF isFocused str side =
    let safeStr = if null str then " " else str
        cursorChar = if isFocused && (currentF `mod` 10 < 5) then "_" else " "
        displayStr = safeStr ++ cursorChar

        padded = replicate (max 0 (6 - length displayStr)) ' ' ++ displayStr
        matrices = map largeDigit padded

        getRow i m = if length m > i then m !! i else "   "
        rowStr i = unwords (map (getRow i) matrices)

        col = if isFocused then ColorTrue 255 176 0 else ColorTrue 150 100 0
        sides = side ++ repeat (text " ")

        formatRow i ext = tightRow
            [ text (replicate 21 ' ')
            , withStyle StyleBold $ withColor col $ text (rowStr i)
            , text "   "
            , ext
            ]
    in zipWith formatRow [0, 1, 2] sides

renderModeSwitch :: AppMode -> L
renderModeSwitch m = 
    let labelU = if m == UnsignedAdd then "[ UNSIGNED ]" else "  UNSIGNED  "
        labelS = if m == SignedAdd   then "[  SIGNED  ]" else "   SIGNED   "
        lever  = if m == UnsignedAdd then "  (O)──┤  " else "  ├──(O)  "

        fullStrLen = length labelU + length lever + length labelS
        padLen = max 0 (62 - fullStrLen) `div` 2
        padding = text (replicate padLen ' ')

        lU = if m == UnsignedAdd then withStyle StyleBold (withColor ColorBrightWhite (text labelU)) else withColor (ColorTrue 60 60 60) (text labelU)
        lS = if m == SignedAdd   then withStyle StyleBold (withColor ColorBrightWhite (text labelS)) else withColor (ColorTrue 60 60 60) (text labelS)
        lLev = withColor ColorBrightWhite (text lever)
    in tightRow [ padding, lU, lLev, lS ]

renderStatusBar :: Focus -> AppMode -> DisplayBase -> L
renderStatusBar foc m base =
    let targetInfo = case foc of
            NumA   -> "REG A [ NUMERIC ENTRY ]"
            SwA i  -> "REG A [ SWITCH BIT " ++ show i ++ " ]"
            NumB   -> "REG B [ NUMERIC ENTRY ]"
            SwB i  -> "REG B [ SWITCH BIT " ++ show i ++ " ]"
        modeInfo = case m of
            UnsignedAdd -> "UNSIGNED"
            SignedAdd   -> "SIGNED"
        baseInfo = case base of
            Dec -> "DEC"
            Hex -> "HEX"
            Oct -> "OCT"
    in tightRow [ text "    ", withColor (ColorTrue 0 150 150) $ text $
       "STATUS: " ++ targetInfo ++ " | " ++ modeInfo ++ " | BASE [ " ++ baseInfo ++ " ]" ]

renderSystemLights :: Int -> Int -> L
renderSystemLights currentF lastF =
    let isActive = currentF - lastF < 4 
        pulse :: Double
        pulse = sin (fromIntegral currentF * 0.3)
        gVal  = round $ 177 + (78 * pulse)
        
        pwrCol = withStyle StyleBold $ withColor (ColorTrue 0 gVal 0) (text "● PWR")
        actCol = if isActive
                 then withStyle StyleBold $ withColor (ColorTrue 255 255 0) (text "● BUS")
                 else withColor (ColorTrue 60 60 0) (text "● BUS")
                 
        clockStates = ["-", "\\", "|", "/"]
        clockChar = clockStates !! ((currentF `div` 2) `mod` 4)
        clkCol = withStyle StyleBold $ withColor (ColorTrue 0 150 150) (text $ "  CLK [" ++ clockChar ++ "]")
        
    in tightRow [ pwrCol, text "  ", actCol, clkCol ]

-- | Status lamp: bold and bright when lit, a dark ember of the same hue when not.
lamp :: (Int, Int, Int) -> Bool -> String -> L
lamp (r, g, b) on label
    | on        = withStyle StyleBold $ withColor (ColorTrue r g b) (text ("● " ++ label))
    | otherwise = withColor (ColorTrue (r `div` 5) (g `div` 5) (b `div` 5)) (text ("● " ++ label))

padTo :: Int -> L -> L
padTo n l = tightRow [ l, text (replicate (max 0 (n - width l)) ' ') ]

-- | ALU status flags from the gate-level ALU. They only light once the ripple
-- animation has settled on the final sum, like a latched status register.
data FlagLamps = FlagLamps { lampC, lampZ, lampN, lampV :: L }

aluFlagLamps :: AppModel -> FlagLamps
aluFlagLamps model =
    let ALUOutput res fl = computeALU OpAdd (byteA model) (byteB model)
        settled = toList res == aluDisplayBits model
        on f    = settled && f == One
        ovf     = case mode model of
            UnsignedAdd -> flagCarry fl
            SignedAdd   -> flagOverflow fl
        amber   = (255, 176, 0)
    in FlagLamps
        { lampC = lamp amber        (on (flagCarry fl))    "CARRY"
        , lampZ = lamp amber        (on (flagZero fl))     "ZERO"
        , lampN = lamp amber        (on (flagNegative fl)) "NEG"
        , lampV = lamp (255, 40, 40) (on ovf)              "OVF"
        }

-- | One-cell frameMargin around an element. Replaces layoutz's 'pad', which
-- measures lines with raw 'length' (ANSI codes included) and so right-pads
-- every coloured line with hundreds of spaces, bloating each frame ~5x.
frameMargin :: L -> L
frameMargin x = layout [ text " ", tightRow [ text " ", x ], text " " ]

-- | Footer key legend: bright keys, dim descriptions.
renderKeyHints :: [(String, String)] -> L
renderKeyHints hints =
    tightRow $ text " " : concatMap hint hints
  where
    -- note: layoutz crashes on `text ""`, so every piece here is non-empty
    hint (k, d) =
        [ text " "
        , withStyle StyleBold $ withColor (ColorTrue 255 176 0) (text k)
        , withColor (ColorTrue 130 130 130) (text (' ' : d ++ " "))
        ]

renderHelpView :: AppModel -> L
renderHelpView _ =
    let w = 72
        thinDivider = withColor (ColorTrue 70 70 70) $ text (replicate w '─')
        header s = withStyle StyleBold $ withColor ColorBrightWhite $ text s
        sec s = withStyle StyleBold $ withColor (ColorTrue 0 200 200) $ text s
        dim s = withColor (ColorTrue 160 160 160) $ text s
        val s = withColor (ColorTrue 255 176 0) $ text s
        item k d = tightRow [ text "   ", val k, text (replicate (max 1 (22 - length k)) ' '), dim d ]
    in layout
    [ frameMargin $ withBorder BorderDouble $ box " (+) ≡≡ VECTRONIX SYSTEMS : MAINFRAME MANUAL [?] ≡≡ (+) "
        [ thinDivider
        , sec " [ KEYBOARD CONTROLS ]"
        , item "[Tab] / [b]" "Next / previous field (value, switches)"
        , item "[Left] / [Right]" "Move switch cursor within a bank"
        , item "[Up] / [Down]" "Adjust value / jump between Reg A <-> B"
        , item "[Space]" "Toggle the selected hardware switch"
        , item "[m]" "Mode: unsigned 0..255 <-> signed -128..127"
        , item "[h]" "Cycle display base: Dec -> Hex -> Oct"
        , item "[?] or [/]" "Toggle this manual"
        , item "[q] or [ESC]" "Power down and quit"
        , text " "
        , thinDivider
        , sec " [ HARDWARE LOGIC GATES (NAND-Derived First Principles) ]"
        , tightRow [ text "   ", dim "NAND(A,B) = NOT(A AND B)   | NOT(A)    = NAND(A,A)" ]
        , tightRow [ text "   ", dim "AND(A,B)  = NOT(NAND(A,B)) | OR(A,B)   = NAND(NOT A, NOT B)" ]
        , tightRow [ text "   ", dim "XOR(A,B)  = AND(OR(A,B), NAND(A,B))" ]
        , text " "
        , thinDivider
        , sec " [ ARITHMETIC HARDWARE ARCHITECTURE ]"
        , tightRow [ text "   ", dim "* Half Adder : Sum = A XOR B,  Carry = A AND B" ]
        , tightRow [ text "   ", dim "* Full Adder : Sum = A XOR B XOR Cin" ]
        , tightRow [ text "   ", dim "               Cout = (A AND B) OR (Cin AND (A XOR B))" ]
        , tightRow [ text "   ", dim "* Ripple-Carry: 8-bit chained adder propagating C0..C7" ]
        , tightRow [ text "   ", dim "* Accumulator : Dynamic LED array with simulated RC bus decay" ]
        , tightRow [ text "   ", dim "* Flags       : CARRY out, ZERO, NEG (bit 7), OVF (per mode)" ]
        , thinDivider
        , tightRow [ text "   ", header "Press [?] to return to the Front Panel" ]
        ]
    , renderKeyHints [("?", "close manual"), ("q", "quit")]
    ]

renderView :: AppModel -> L
renderView model
    | showHelp model = renderHelpView model
    | otherwise =
        let bitsA = toList (byteA model)
            bitsB = toList (byteB model)
            isCompact = termH model < 38
            flags = aluFlagLamps model

            w = 72
            centerTxt s = let p = max 0 (w - length s) `div` 2 in replicate p ' ' ++ s ++ replicate (w - length s - p) ' '

            thickDivider = withColor (ColorTrue 50 50 50) $ text (replicate w '▓')

            busDivider label =
                let str = "[ " ++ label ++ " ]"
                    remLen = max 0 (w - length str)
                    leftChars = replicate (remLen `div` 2) '═'
                    rightChars = replicate (remLen - (remLen `div` 2)) '═'
                in withColor (ColorTrue 80 80 80) $ text (leftChars ++ str ++ rightChars)

            -- The focused register's header lights up amber.
            renderHeader' active title =
                let titleSpacing = (if active then " ▶ " else "  ") ++ title ++ "  "
                    leftBlocks = "███"
                    rightLen = max 0 (w - length titleSpacing - length leftBlocks)
                    rightBlocks = replicate rightLen '█'
                    titleCol = if active then ColorTrue 255 176 0 else ColorBrightWhite
                in tightRow
                   [ withColor (ColorTrue 50 50 50) $ text leftBlocks
                   , withStyle StyleBold $ withColor titleCol $ text titleSpacing
                   , withColor (ColorTrue 50 50 50) $ text rightBlocks
                   ]
            renderHeader = renderHeader' False
            focusOnA = case focus model of { NumA -> True; SwA _ -> True; _ -> False }
            focusOnB = case focus model of { NumB -> True; SwB _ -> True; _ -> False }

            renderCompactVal isFocused valStr ext =
                let col = if isFocused then ColorTrue 255 176 0 else ColorTrue 180 140 0
                    cursorChar = if isFocused && (frame model `mod` 10 < 5) then "_" else " "
                in [ tightRow
                       [ text "                "
                       , withColor (ColorTrue 130 130 130) (text "VALUE: [ ")
                       , withStyle StyleBold $ withColor col (text (valStr ++ cursorChar))
                       , withColor (ColorTrue 130 130 130) (text " ]")
                       , text (replicate (max 1 (10 - length valStr)) ' ')
                       , ext
                       ]
                   ]

            displayA = if isCompact
                       then renderCompactVal (focus model == NumA) (strA model) (text " ")
                       else renderDigitalDisplay (frame model) (focus model == NumA) (strA model) []
            displayB = if isCompact
                       then renderCompactVal (focus model == NumB) (strB model) (text " ")
                       else renderDigitalDisplay (frame model) (focus model == NumB) (strB model) []
            aluWord = fromListPadded (aluDisplayBits model)
            aluValStr = formatValue (displayBase model) (getNumericValue (mode model) aluWord)
            displayALU = if isCompact
                         then renderCompactVal False aluValStr $ tightRow
                                [ lampC flags, text "  ", lampZ flags, text "  ", lampN flags, text "  ", lampV flags ]
                         else renderDigitalDisplay (frame model) False aluValStr
                                [ tightRow [ padTo 10 (lampC flags), lampZ flags ]
                                , tightRow [ padTo 10 (lampN flags), lampV flags ]
                                ]

            spacingLines = if isCompact then [] else [text " "]
            -- Compact mode must fit 24 rows (the runtime ends every line,
            -- including the last, with a newline), so drop the dividers there.
            sectionBreak = if isCompact then [] else [thickDivider]
            logLinesCount = if isCompact then 1 else 3
            -- logLines is newest-first: keep the newest N, print oldest at top.
            shownLog = reverse (take logLinesCount (logLines model))
            logShade age = case age of
                0 -> ColorTrue 0 200 0
                1 -> ColorTrue 0 130 0
                _ -> ColorTrue 0 80 0
            renderLogLine (age, (msg, n)) = tightRow $
                [ text "    "
                , withColor (logShade age) (text msg)
                ] ++ [ withColor (ColorTrue 0 90 0) (text ("  (x" ++ show n ++ ")")) | n > 1 ]
            logView = if null shownLog
                      then [ tightRow [ text "    ", withColor (ColorTrue 0 100 0) (text "> SYSTEM AWAITING INPUT...") ] ]
                      else map renderLogLine (zip [length shownLog - 1, length shownLog - 2 .. 0] shownLog)

            boxContent =
                [ tightRow [ text "   ", renderSystemLights (frame model) (lastInputFrame model) ]
                , withColor (ColorTrue 130 130 130) $ text $ centerTxt "MODE SELECTOR"
                , renderModeSwitch (mode model)
                ] ++ spacingLines ++
                [ busDivider "I/O DATA BUS"
                , renderHeader' focusOnA "REGISTER A"
                , tightRow [ text "                ", renderBits (frame model) bitsA (decayA model) ]
                , tightRow [ text "                ", renderSwitches (focus model) "A" bitsA ]
                ] ++ displayA ++
                sectionBreak ++
                [ renderHeader' focusOnB "REGISTER B"
                , tightRow [ text "                ", renderBits (frame model) bitsB (decayB model) ]
                , tightRow [ text "                ", renderSwitches (focus model) "B" bitsB ]
                ] ++ displayB ++
                sectionBreak ++
                [ renderHeader "ALU ACCUMULATOR"
                , tightRow [ text "                ", renderAluBits (frame model) (aluDisplayBits model) (decayALU model) ]
                ] ++ displayALU ++
                spacingLines ++
                [ busDivider "DIAGNOSTIC LOGIC BUS"
                , renderHeader "SYSTEM LOG"
                ] ++ logView ++
                spacingLines ++
                [ renderHeader "STATE MONITOR"
                , renderStatusBar (focus model) (mode model) (displayBase model)
                ]

        in layout
        [ (if isCompact then id else frameMargin) $ withBorder BorderDouble $ box " (+) ≡≡ VECTRONIX SYSTEMS : MAINFRAME 2000 ≡≡ (+) " boxContent
        , renderKeyHints
            [ ("Tab", "field"), ("←→", "bit"), ("Spc", "flip"), ("↑↓", "adjust")
            , ("m", "mode"), ("h", "base"), ("?", "help"), ("q", "quit") ]
        ]
