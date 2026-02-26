{-# LANGUAGE OverloadedStrings #-}

-- | Playwright - TTY-free test harness for HyperConsole
--
-- "He'd operated on an almost permanent adrenaline high, a byproduct
--  of youth and proficiency, jacked into a custom cyberspace deck..."
--
--                                                    — Neuromancer
--
-- Renders widgets to text for debugging without requiring a terminal.
-- Dumps canvas structure, line counts, and rendered output.
module Main where

import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import HyperConsole
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printHelp
    ["-h"] -> printHelp
    ["progress"] -> runProgressPlaywright
    ["progress-bar"] -> runProgressBarPlaywright
    ["hbox-mixed"] -> runHboxMixedPlaywright
    ["all"] -> runAllPlaywright
    [] -> runAllPlaywright
    _ -> printHelp

printHelp :: IO ()
printHelp = do
  putStrLn "HyperConsole Playwright - TTY-free test harness"
  putStrLn ""
  putStrLn "Usage: hyperconsole-playwright [TEST]"
  putStrLn ""
  putStrLn "Available tests:"
  putStrLn "  progress      Full progress showcase widget"
  putStrLn "  progress-bar  Just the progressBar widget"
  putStrLn "  hbox-mixed    hbox with mixed-height children"
  putStrLn "  all           Run all tests (default)"

runAllPlaywright :: IO ()
runAllPlaywright = do
  runProgressBarPlaywright
  putStrLn ""
  runHboxMixedPlaywright
  putStrLn ""
  runProgressPlaywright

-- ════════════════════════════════════════════════════════════════════════════
-- Canvas Inspection
-- ════════════════════════════════════════════════════════════════════════════

-- | Render a canvas to plain text (no ANSI codes)
renderCanvasPlain :: Canvas -> Text
renderCanvasPlain canvas =
  T.unlines [renderLine line | line <- V.toList (canvasLines canvas)]
  where
    renderLine line = T.concat [spanText s | s <- toList line]

-- | Dump canvas structure for debugging
dumpCanvas :: String -> Canvas -> IO ()
dumpCanvas name canvas = do
  putStrLn $ "═══ " <> name <> " ═══"
  putStrLn $ "Dimensions: " <> show (canvasDims canvas)
  putStrLn $ "Line count: " <> show (V.length (canvasLines canvas))
  putStrLn "Lines:"
  V.iforM_ (canvasLines canvas) $ \i line -> do
    putStrLn $ "  [" <> show i <> "] spans=" <> show (Seq.length line) <> ": " <> show (take 3 $ toList line) <> "..."
  putStrLn "Rendered:"
  TIO.putStrLn $ renderCanvasPlain canvas

-- | Show actual character widths of each line
dumpLineWidths :: Canvas -> IO ()
dumpLineWidths canvas = do
  putStrLn "Line widths:"
  V.iforM_ (canvasLines canvas) $ \i line -> do
    let totalChars = sum [T.length (spanText s) | s <- toList line]
    putStrLn $ "  [" <> show i <> "] chars=" <> show totalChars

-- ════════════════════════════════════════════════════════════════════════════
-- Test Cases
-- ════════════════════════════════════════════════════════════════════════════

runProgressBarPlaywright :: IO ()
runProgressBarPlaywright = do
  putStrLn "Testing progressBar widget..."
  let dims = Dimensions 80 10

  -- Test at 0%
  putStrLn "\n--- progressBar at 0% ---"
  let w0 = progressBar (fg Cyan) (fg BrightBlack) (fg White) 0.0
      c0 = runWidget w0 dims
  dumpCanvas "progressBar 0%" c0

  -- Test at 50%
  putStrLn "\n--- progressBar at 50% ---"
  let w50 = progressBar (fg Cyan) (fg BrightBlack) (fg White) 0.5
      c50 = runWidget w50 dims
  dumpCanvas "progressBar 50%" c50

  -- Test at 100%
  putStrLn "\n--- progressBar at 100% ---"
  let w100 = progressBar (fg Cyan) (fg BrightBlack) (fg White) 1.0
      c100 = runWidget w100 dims
  dumpCanvas "progressBar 100%" c100

  -- Test the ACTUAL pattern from the demo: hbox [text, progress]
  putStrLn "\n--- hbox [label, progress] - THE BUG ---"
  let wBug =
        hbox
          [ textStyled (fg BrightBlack) "Standard:    ",
            progress (fg Green) (fg BrightBlack) 0.5
          ]
      cBug = runWidget wBug dims
  dumpCanvas "hbox label+progress" cBug
  dumpLineWidths cBug

  -- What SHOULD happen with proper constraints
  putStrLn "\n--- hboxWith [Exact 13, Fill 1] [label, progress] ---"
  let wFixed =
        hboxWith
          [Exact 13, Fill 1]
          [ textStyled (fg BrightBlack) "Standard:    ",
            progress (fg Green) (fg BrightBlack) 0.5
          ]
      cFixed = runWidget wFixed dims
  dumpCanvas "hboxWith fixed label" cFixed
  dumpLineWidths cFixed

  -- NEW: Using labeled combinator
  putStrLn "\n--- labeled (THE FIX) ---"
  let wLabeled =
        labeled
          (textStyled (fg BrightBlack) "Standard:    ")
          (progress (fg Green) (fg BrightBlack) 0.5)
      cLabeled = runWidget wLabeled dims
  dumpCanvas "labeled combinator" cLabeled
  dumpLineWidths cLabeled

  -- NEW: Using <+> operator
  putStrLn "\n--- Using <+> operator ---"
  let wOp = textStyled (fg BrightBlack) "Standard:    " <+> progress (fg Green) (fg BrightBlack) 0.5
      cOp = runWidget wOp dims
  dumpCanvas "<+> operator" cOp
  dumpLineWidths cOp

runHboxMixedPlaywright :: IO ()
runHboxMixedPlaywright = do
  putStrLn "Testing hbox with mixed-height children..."
  let dims = Dimensions 80 10

  -- Mixed heights
  putStrLn "\n--- hbox [vbox 3 lines, text 1 line, vbox 2 lines] ---"
  let w =
        hbox
          [ vbox [text "a", text "b", text "c"],
            text "x",
            vbox [text "1", text "2"]
          ]
      c = runWidget w dims
  dumpCanvas "hbox mixed" c

  -- With empty
  putStrLn "\n--- hbox [empty, text, empty] ---"
  let w2 = hbox [empty, text "middle", empty]
      c2 = runWidget w2 dims
  dumpCanvas "hbox with empty" c2

runProgressPlaywright :: IO ()
runProgressPlaywright = do
  putStrLn "Testing full progressShowcase..."
  let dims = Dimensions 120 20
      pct = 0.0
      tick = 0

  let w = progressShowcase pct tick
      c = runWidget w dims
  dumpCanvas "progressShowcase" c
  dumpLineWidths c

-- ════════════════════════════════════════════════════════════════════════════
-- Widgets (copied from Demo.hs for isolation)
-- ════════════════════════════════════════════════════════════════════════════

progressShowcase :: Double -> Int -> Widget
progressShowcase pct tick =
  bordered $
    padded 1 2 1 2 $
      vbox
        [ textStyled (bold (fg BrightWhite)) "Progress Bar Showcase",
          rule (fg BrightBlack) '─',
          space 0 1,
          -- Standard (FIXED: using <+>)
          textStyled (fg BrightBlack) "Standard:    " <+> progress (fg Green) (fg BrightBlack) pct,
          -- Gradient
          textStyled (fg BrightBlack) "Gradient:    " <+> progress (gradientColor pct) (fg BrightBlack) pct,
          -- With label
          textStyled (fg BrightBlack) "With Label:  " <+> progressBar (fg Cyan) (fg BrightBlack) (fg White) pct,
          space 0 1,
          rule (fg BrightBlack) '─',
          -- Gauges
          textStyled (fg BrightBlack) "Gauges:      "
            <+> hbox
              [ gauge (fg Green) (fg Yellow) (fg Red) pct,
                textStyled (fg BrightBlack) "  ",
                gauge (fg Green) (fg Yellow) (fg Red) (min 1.0 (pct * 1.2)),
                textStyled (fg BrightBlack) "  ",
                gauge (fg Green) (fg Yellow) (fg Red) (min 1.0 (pct * 1.5)),
                textStyled (fg BrightBlack) "  ",
                gauge (fg Green) (fg Yellow) (fg Red) (min 1.0 (pct * 2.0))
              ],
          -- Spinners
          space 0 1,
          textStyled (fg BrightBlack) "Spinners:    "
            <+> hbox
              [ spinner (fg Cyan) tick,
                textStyled (fg BrightBlack) "  ",
                spinnerStyle (fg Magenta) "◐◓◑◒" tick,
                textStyled (fg BrightBlack) "  ",
                spinnerStyle (fg Yellow) "⣾⣽⣻⢿⡿⣟⣯⣷" tick,
                textStyled (fg BrightBlack) "  ",
                spinnerStyle (fg Green) "←↖↑↗→↘↓↙" tick,
                textStyled (fg BrightBlack) "  ",
                spinnerStyle (fg Red) "▁▂▃▄▅▆▇█▇▆▅▄▃▂" tick
              ]
        ]
  where
    gradientColor p
      | p < 0.33 = fg Red
      | p < 0.66 = fg Yellow
      | otherwise = fg Green
