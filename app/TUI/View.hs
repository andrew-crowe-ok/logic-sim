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

import TUI.State

renderBits :: Int -> [Bit] -> [Int] -> L
renderBits currentF bits decays = tightRow $ map draw (reverse (zip bits decays))
  where
    pulse :: Double
    pulse = sin (fromIntegral currentF * 0.3)
    rVal  :: Int
    rVal  = round $ 177 + (78 * pulse)

    draw (One, _) = tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                             , withStyle StyleBold $ withColor (ColorTrue rVal 0 0) (text "O")
                             , withColor (ColorTrue 60 60 60) (text "] ")
                             ]
    draw (Zero, lastOnF) =
        let diff = currentF - lastOnF
            ghostCol = if diff <= 1 then ColorTrue 140 0 0
                       else if diff == 2 then ColorTrue 90 0 0
                       else ColorTrue 40 0 0
        in tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                    , withColor ghostCol (text "O")
                    , withColor (ColorTrue 60 60 60) (text "] ")
                    ]

renderAluBits :: Int -> [Bit] -> [Int] -> L
renderAluBits currentF bits decays = tightRow $ map draw (reverse (zip bits decays))
  where
    pulse :: Double
    pulse = sin (fromIntegral currentF * 0.3)
    gVal  :: Int
    gVal  = round $ 177 + (78 * pulse)

    draw (One, _) = tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                             , withStyle StyleBold $ withColor (ColorTrue 0 gVal 0) (text "O")
                             , withColor (ColorTrue 60 60 60) (text "] ")
                             ]
    draw (Zero, lastOnF) =
        let diff = currentF - lastOnF
            ghostCol = if diff <= 1 then ColorTrue 0 140 0
                       else if diff == 2 then ColorTrue 0 90 0
                       else ColorTrue 0 40 0
        in tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                    , withColor ghostCol (text "O")
                    , withColor (ColorTrue 60 60 60) (text "] ")
                    ]

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

renderDigitalDisplay :: Int -> Bool -> String -> L -> [L]
renderDigitalDisplay currentF isFocused str appendRight =
    let safeStr = if null str then " " else str
        cursorChar = if isFocused && (currentF `mod` 10 < 5) then "_" else " "
        displayStr = safeStr ++ cursorChar
        
        padded = replicate (max 0 (5 - length displayStr)) ' ' ++ displayStr
        matrices = map largeDigit padded

        getRow i m = if length m > i then m !! i else "   "

        row1 = unwords (map (getRow 0) matrices)
        row2 = unwords (map (getRow 1) matrices)
        row3 = unwords (map (getRow 2) matrices)

        col = if isFocused then ColorTrue 255 176 0 else ColorTrue 150 100 0
        
        formatRow r ext = tightRow [ text "                         ", withStyle StyleBold $ withColor col $ text r, text "   ", ext ]
    in [ formatRow row1 (text " ")
       , formatRow row2 appendRight
       , formatRow row3 (text " ")
       ]

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

renderStatusBar :: Focus -> DisplayBase -> L
renderStatusBar foc base =
    let targetInfo = case foc of
            NumA   -> "REG A [ NUMERIC ENTRY ]"
            SwA i  -> "REG A [ SWITCH BIT " ++ show i ++ " ]"
            NumB   -> "REG B [ NUMERIC ENTRY ]"
            SwB i  -> "REG B [ SWITCH BIT " ++ show i ++ " ]"
        baseInfo = "BASE [ " ++ show base ++ " ]"
    in tightRow [ text "    ", withColor (ColorTrue 0 150 150) $ text $
       "STATUS: " ++ targetInfo ++ " | " ++ baseInfo ]

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
    [ pad 1 $ withBorder BorderDouble $ box " (+) ≡≡ VECTRONIX SYSTEMS : MAINFRAME MANUAL [?] ≡≡ (+) "
        [ thinDivider
        , sec " [ KEYBOARD CONTROLS ]"
        , item "[Tab] / [b]" "Cycle focus forward / backward"
        , item "[Left] / [Right]" "Move switch cursor (Bit 7 <-> Bit 0)"
        , item "[Up] / [Down]" "Adjust numeric value / Switch Reg A <-> B"
        , item "[Space]" "Toggle currently selected hardware switch"
        , item "[m]" "Toggle mode: Unsigned (0..255) <-> Signed (-128..127)"
        , item "[h]" "Cycle display base: Decimal -> Hexadecimal -> Octal"
        , item "[?] or [/]" "Toggle this Help & Circuit Manual overlay"
        , item "[q] or [ESC]" "Safely power down system and quit"
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
        , thinDivider
        , tightRow [ text "   ", header "Press [?] or [ESC] to return to Front Panel" ]
        ]
    , withStyle StyleBold $ withColor ColorBrightWhite $ text "  [?] Close Manual | [q] Quit"
    ]

renderView :: AppModel -> L
renderView model
    | showHelp model = renderHelpView model
    | otherwise =
        let bitsA = toList (byteA model)
            bitsB = toList (byteB model)
            isOV = checkOverflow (mode model) (byteA model) (byteB model)
            isCompact = termH model < 38

            w = 72
            centerTxt s = let p = max 0 (w - length s) `div` 2 in replicate p ' ' ++ s ++ replicate (w - length s - p) ' '

            thickDivider = withColor (ColorTrue 50 50 50) $ text (replicate w '▓')
            
            busDivider label =
                let str = "[ " ++ label ++ " ]"
                    remLen = max 0 (w - length str)
                    leftChars = replicate (remLen `div` 2) '═'
                    rightChars = replicate (remLen - (remLen `div` 2)) '═'
                in withColor (ColorTrue 80 80 80) $ text (leftChars ++ str ++ rightChars)

            renderHeader title =
                let titleSpacing = "  " ++ title ++ "  "
                    leftBlocks = "███"
                    rightLen = max 0 (w - length titleSpacing - length leftBlocks)
                    rightBlocks = replicate rightLen '█'
                in tightRow
                   [ withColor (ColorTrue 50 50 50) $ text leftBlocks
                   , withStyle StyleBold $ withColor ColorBrightWhite $ text titleSpacing
                   , withColor (ColorTrue 50 50 50) $ text rightBlocks
                   ]

            ovfLight = if isOV then withStyle StyleBold $ withColor (ColorTrue 255 0 0) (text "● OVF") else withColor (ColorTrue 40 0 0) (text "● OVF")

            renderCompactVal isFocused valStr ext =
                let col = if isFocused then ColorTrue 255 176 0 else ColorTrue 180 140 0
                    cursorChar = if isFocused && (frame model `mod` 10 < 5) then "_" else " "
                in [ tightRow 
                       [ text "                "
                       , withColor (ColorTrue 130 130 130) (text "VALUE: [ ")
                       , withStyle StyleBold $ withColor col (text (valStr ++ cursorChar))
                       , withColor (ColorTrue 130 130 130) (text " ]")
                       , text "    "
                       , ext
                       ]
                   ]

            displayA = if isCompact
                       then renderCompactVal (focus model == NumA) (strA model) (text " ")
                       else renderDigitalDisplay (frame model) (focus model == NumA) (strA model) (text " ")
            displayB = if isCompact
                       then renderCompactVal (focus model == NumB) (strB model) (text " ")
                       else renderDigitalDisplay (frame model) (focus model == NumB) (strB model) (text " ")
            aluWord = fromListPadded (aluDisplayBits model)
            aluValStr = formatValue (displayBase model) (getNumericValue (mode model) aluWord)
            displayALU = if isCompact
                         then renderCompactVal False aluValStr ovfLight
                         else renderDigitalDisplay (frame model) False aluValStr ovfLight

            spacingLines = if isCompact then [] else [text " "]
            logLinesCount = if isCompact then 1 else 3
            linesToRender = if null (logLines model)
                            then ["> SYSTEM AWAITING INPUT..."]
                            else take logLinesCount (reverse (logLines model))

            boxContent =
                [ tightRow [ text "   ", renderSystemLights (frame model) (lastInputFrame model) ]
                , text $ centerTxt "MODE SELECTOR"
                , renderModeSwitch (mode model)
                ] ++ spacingLines ++
                [ busDivider "I/O DATA BUS"
                , renderHeader "REGISTER A"
                , tightRow [ text "                ", renderBits (frame model) bitsA (decayA model) ]
                , tightRow [ text "                ", renderSwitches (focus model) "A" bitsA ]
                ] ++ displayA ++
                [ thickDivider
                , renderHeader "REGISTER B"
                , tightRow [ text "                ", renderBits (frame model) bitsB (decayB model) ]
                , tightRow [ text "                ", renderSwitches (focus model) "B" bitsB ]
                ] ++ displayB ++
                [ thickDivider
                , renderHeader "ALU ACCUMULATOR"
                , tightRow [ text "                ", renderAluBits (frame model) (aluDisplayBits model) (decayALU model) ]
                ] ++ displayALU ++
                spacingLines ++
                [ busDivider "DIAGNOSTIC LOGIC BUS"
                , renderHeader "SYSTEM LOG"
                ] ++ map (\s -> tightRow [ text "    ", withColor (ColorTrue 0 100 0) (text s) ]) linesToRender ++
                spacingLines ++
                [ renderHeader "STATE MONITOR"
                , renderStatusBar (focus model) (displayBase model)
                ]

        in layout
        [ (if isCompact then id else pad 1) $ withBorder BorderDouble $ box " (+) ≡≡ VECTRONIX SYSTEMS : MAINFRAME 2000 ≡≡ (+) " boxContent
        , withStyle StyleBold $ withColor ColorBrightWhite $ text "  [Tab] Next | [Arrows] Navigate | [Space] Toggle | [m] Mode | [h] Base | [?] Help | [q] Quit"
        ]
