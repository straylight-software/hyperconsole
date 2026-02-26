{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | Sensenet Build Dashboard
--
-- "He'd operated on an almost permanent adrenaline high, a byproduct
--  of youth and proficiency, jacked into a custom cyberspace deck
--  that projected his disembodied consciousness into the consensual
--  hallucination that was the matrix."
--
--                                                    — Neuromancer
--
-- Full ono-sendai razorgirl aesthetic build monitor.
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.IORef
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import HyperConsole
import HyperConsole.Theme


-- ════════════════════════════════════════════════════════════════════════════
-- Types
-- ════════════════════════════════════════════════════════════════════════════

data Package = Package
  { pkgName  :: !Text
  , pkgPath  :: !Text
  , pkgLang  :: !Text
  , pkgTime  :: !Int
  , pkgStart :: !Int
  }
  deriving stock (Eq, Show)

data PackageGroup = PackageGroup
  { groupLabel  :: !Text
  , groupFilter :: Package -> Bool
  }

data LogLine = LogLine
  { logText  :: !Text
  , logStyle :: !Style
  }

-- ════════════════════════════════════════════════════════════════════════════
-- Package Data
-- ════════════════════════════════════════════════════════════════════════════

packages :: [Package]
packages = computeStartTimes
  [ Package "nv:hello" "examples/nv" "cuda" 2100 0
  , Package "nv:mdspan_device_test" "examples/nv" "cuda" 3200 0
  , Package "nv:tensor_core" "examples/nv" "cuda" 4100 0
  , Package "sigil-trtllm:test-rope" "sigil-trtllm" "cuda" 6200 0
  , Package "sigil-trtllm:test-fp4-quant" "sigil-trtllm" "cuda" 3400 0
  , Package "sigil-trtllm:sigil-eval-perplexity" "sigil-trtllm" "cuda" 4800 0
  , Package "sigil-trtllm:sigil-inspect-checkpoint" "sigil-trtllm" "cuda" 7200 0
  , Package "sigil-trtllm:sigil-build-engine" "sigil-trtllm" "cuda" 8800 0
  , Package "sigil-trtllm:sigil-run-inference" "sigil-trtllm" "cuda" 5100 0
  , Package "cxx:hello-cxx" "examples/cxx" "cxx" 900 0
  , Package "cxx:foo" "examples/cxx" "cxx" 1100 0
  , Package "cxx:bar" "examples/cxx" "cxx" 1300 0
  , Package "cxx:baz" "examples/cxx" "cxx" 800 0
  , Package "simdjson:twitter" "examples/simdjson" "cxx" 3600 0
  , Package "haskell:hello-hs" "examples/haskell" "haskell" 2200 0
  , Package "haskell:json_demo" "examples/haskell" "haskell" 2800 0
  , Package "haskell:greetlib" "examples/haskell" "haskell" 1600 0
  , Package "haskell-cxx:test_ffi" "examples/haskell-cxx" "haskell" 3100 0
  , Package "hasktorch:hasktorch_demo" "examples/hasktorch" "haskell" 3500 0
  , Package "hasktorch:linear_regression" "examples/hasktorch" "haskell" 3800 0
  , Package "rust:hello-rs" "examples/rust" "rust" 1800 0
  , Package "rust:mathlib" "examples/rust" "rust" 1200 0
  , Package "rust-crate-test:cfg-if" "examples/rust-crate-test" "rust" 1400 0
  , Package "rust-crate-test:once_cell" "examples/rust-crate-test" "rust" 1500 0
  , Package "lean:hello-lean" "examples/lean" "lean" 2400 0
  , Package "lean:hashmap" "examples/lean" "lean" 2900 0
  , Package "lean-multifile:straylight" "examples/lean-multifile" "lean" 3900 0
  , Package "lean-continuity:continuity" "examples/lean-continuity" "lean" 700 0
  , Package "blake:blake" "examples/blake" "rust" 2600 0
  , Package "sqlite-test:sqlite-test" "examples/sqlite-test" "cxx" 1100 0
  , Package "zlib-test:zlib-test" "examples/zlib-test" "cxx" 1700 0
  , Package "multi-dep:multi-dep" "examples/multi-dep" "cxx" 1900 0
  , Package "cross-pkg-test/lib:greeter" "examples/cross-pkg-test" "cxx" 1000 0
  , Package "purescript:halogen-todo" "examples/purescript" "purescript" 2700 0
  , Package "straylight-web:straylight-web" "examples/straylight-web" "purescript" 3000 0
  , Package "sensenet:sensenet-core" "sensenet" "haskell" 5600 0
  , Package "sensenet:sensenet-dice" "sensenet" "haskell" 4200 0
  , Package "sensenet:dhallfast" "sensenet" "haskell" 4500 0
  , Package "nix-analyze:nix-analyze" "nix-analyze" "haskell" 4800 0
  ]

buildFiles :: [Text]
buildFiles =
  [ "examples/blake", "examples/cxx", "examples/haskell-cxx", "examples/haskell"
  , "examples/hasktorch", "examples/lean-continuity", "examples/lean-multifile"
  , "examples/lean", "examples/multi-dep", "examples/nv", "examples/rust-crate-test"
  , "examples/rust", "examples/simdjson", "examples/sqlite-test", "examples/zlib-test"
  , "examples/purescript", "examples/straylight-web", "examples/cross-pkg-test/app"
  , "examples/cross-pkg-test/lib", "nix-analyze", "sensenet", "sigil-trtllm"
  ]

ruleCount :: Text -> Int
ruleCount p = case p of
  "examples/blake" -> 1; "examples/cxx" -> 4; "examples/haskell-cxx" -> 1
  "examples/haskell" -> 4; "examples/hasktorch" -> 2; "examples/lean-continuity" -> 1
  "examples/lean-multifile" -> 1; "examples/lean" -> 2; "examples/multi-dep" -> 1
  "examples/nv" -> 3; "examples/rust-crate-test" -> 5; "examples/rust" -> 3
  "examples/simdjson" -> 1; "examples/sqlite-test" -> 1; "examples/zlib-test" -> 1
  "examples/purescript" -> 1; "examples/straylight-web" -> 1
  "examples/cross-pkg-test/app" -> 1; "examples/cross-pkg-test/lib" -> 1
  "nix-analyze" -> 1; "sensenet" -> 5; "sigil-trtllm" -> 6
  _ -> 1

-- All groups in display order (single column layout)
allGroups :: [PackageGroup]
allGroups =
  [ PackageGroup "SIGIL · TENSORRT-LLM" (\p -> pkgPath p == "sigil-trtllm")
  , PackageGroup "NVIDIA · CUDA" (\p -> pkgPath p == "examples/nv")
  , PackageGroup "SENSENET · CORE" (\p -> pkgPath p `elem` ["sensenet", "nix-analyze"])
  , PackageGroup "HASKELL" (\p -> pkgPath p `elem` ["examples/haskell", "examples/haskell-cxx", "examples/hasktorch"])
  , PackageGroup "LEAN4 · FORMAL VERIFICATION" (\p -> pkgLang p == "lean")
  , PackageGroup "RUST" (\p -> pkgLang p == "rust")
  , PackageGroup "C++ · NATIVE" (\p -> pkgLang p == "cxx" && not ("haskell" `T.isInfixOf` pkgPath p) && pkgPath p /= "examples/nv")
  , PackageGroup "FRONTEND" (\p -> pkgLang p == "purescript")
  ]

-- ════════════════════════════════════════════════════════════════════════════
-- Scheduling
-- ════════════════════════════════════════════════════════════════════════════

computeStartTimes :: [Package] -> [Package]
computeStartTimes pkgs =
  let maxPar = 12
      indexed = zip [0..] pkgs
      sorted = sortBy (comparing (Down . pkgTime . snd)) indexed
      initialLanes = replicate maxPar 0 :: [Int]
      initialStarts = replicate (length pkgs) 0 :: [Int]
      (finalStarts, _) = foldl assignPkg (initialStarts, initialLanes) sorted
  in zipWith (\p s -> p { pkgStart = s }) pkgs finalStarts
  where
    assignPkg (starts, lanes) (origIdx, pkg) =
      let (bestLane, bestTime) = minimumByComparing snd (zip [0..] lanes)
          newLaneEnd = bestTime + pkgTime pkg
          newLanes = updateAt bestLane newLaneEnd lanes
          newStarts = updateAt origIdx bestTime starts
      in (newStarts, newLanes)
    minimumByComparing f = foldr1 (\a b -> if f a <= f b then a else b)
    updateAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

totalTime :: [Package] -> Int
totalTime pkgs = maximum (map (\p -> pkgStart p + pkgTime p) pkgs)

-- ════════════════════════════════════════════════════════════════════════════
-- Progress Curves
-- ════════════════════════════════════════════════════════════════════════════

-- | Perceptual progress: fast start, slow finish
perceptual :: Double -> Double
perceptual x
  | x <= 0    = 0
  | x >= 1    = 1
  | x < 0.4   = (x / 0.4) * 0.75
  | otherwise = 0.75 + ((x - 0.4) / 0.6) * 0.25

-- ════════════════════════════════════════════════════════════════════════════
-- Main
-- ════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = withConsole $ \console -> do
  dims <- getTerminalSize

  -- Phase 1: Streaming log preamble
  runPreamble console dims

  -- Phase 2: Building
  runBuilding console dims

  -- Hold final state
  threadDelay 3000000
  clear console

-- ════════════════════════════════════════════════════════════════════════════
-- Preamble Phase
-- ════════════════════════════════════════════════════════════════════════════

runPreamble :: Console -> Dimensions -> IO ()
runPreamble console dims = do
  logRef <- newIORef ([] :: [LogLine])
  let addLog style txt = modifyIORef' logRef (++ [LogLine txt style])

  -- Scan phase
  addLog razorAccent "◌ scanning //..."
  forM_ buildFiles $ \path -> do
    addLog razorMuted ("◉ found //src/" <> path <> "/BUILD.dhall")
    logs <- readIORef logRef
    render console (preambleWidget dims logs "scan" (length logs) (length buildFiles))
    threadDelay 28000

  -- Parse phase
  addLog razorAccent "◌ parsing //..."
  forM_ buildFiles $ \path -> do
    addLog razorInfo ("◉ eval //src/" <> path <> "/BUILD.dhall (" <> T.pack (show (ruleCount path)) <> " rules)")
    logs <- readIORef logRef
    render console (preambleWidget dims logs "parse" (length logs) (length buildFiles))
    threadDelay 22000

  -- Graph
  addLog razorAccent "◌ graph //..."
  logs <- readIORef logRef
  render console (preambleWidget dims logs "graph" 0 0)
  threadDelay 100000

  -- Cache check phase
  forM_ packages $ \pkg -> do
    addLog razorMiss ("✗ miss //" <> pkgName pkg)
    logs <- readIORef logRef
    render console (preambleWidget dims logs "cache" (length (filter (\l -> "miss" `T.isInfixOf` logText l) logs)) (length packages))
    threadDelay 10000

preambleWidget :: Dimensions -> [LogLine] -> Text -> Int -> Int -> Widget
preambleWidget dims logs phase current total =
  vbox
    [ headerWidget
    , space 0 1
    , logStreamWidget dims logs
    , space 0 1
    , preambleStatus phase current total
    , footerWidget
    ]

logStreamWidget :: Dimensions -> [LogLine] -> Widget
logStreamWidget dims logs =
  let visibleCount = max 12 (height dims - 12)
      visible = take visibleCount (reverse logs)
      -- Fade effect: older lines are dimmer
      renderLine :: Int -> LogLine -> Widget
      renderLine i (LogLine txt style) =
        let opacity = if i < 3 then dim style else style
        in textStyled opacity txt
  in vbox (zipWith renderLine [0..] (reverse visible))

preambleStatus :: Text -> Int -> Int -> Widget
preambleStatus phase current total =
  let statusText = case phase of
        "scan"  -> T.pack (show current) <> "/" <> T.pack (show total) <> " packages"
        "parse" -> T.pack (show current) <> "/" <> T.pack (show total) <> " parsed"
        "graph" -> "computing dependency graph..."
        "cache" -> T.pack (show current) <> "/" <> T.pack (show total) <> " cache checks"
        _       -> ""
  in hbox
       [ fill razorRule '─'
       , textStyled razorDim (" " <> statusText <> " ")
       ]

-- ════════════════════════════════════════════════════════════════════════════
-- Building Phase
-- ════════════════════════════════════════════════════════════════════════════

runBuilding :: Console -> Dimensions -> IO ()
runBuilding console dims = do
  let makespan = totalTime packages

  let go elapsed
        | elapsed >= makespan + 300 = do
            -- Final state
            render console (completeWidget dims packages makespan)
        | otherwise = do
            render console (buildingWidget dims packages elapsed)
            threadDelay 25000  -- ~40fps
            go (elapsed + 75)  -- ~3x speed

  go 0

buildingWidget :: Dimensions -> [Package] -> Int -> Widget
buildingWidget _dims pkgs elapsed =
  vboxWith [Exact 3, Exact 1, Exact 4, Exact 1, Fill 1, Exact 1]
    [ headerWidget
    , space 0 1
    , statsRowWidget pkgs elapsed
    , space 0 1
    , groupsWidget pkgs elapsed
    , footerWidget
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- Header
-- ════════════════════════════════════════════════════════════════════════════

headerWidget :: Widget
headerWidget =
  vbox
    [ textStyled razorMuted "STRAYLIGHT SOFTWARE · BUILD MONITOR"
    , hbox
        [ textStyled razorMuted "> "
        , textStyled (bold razorBright) "sensenet build "
        , textStyled razorBright "// ..."
        , textStyled razorAccent " █"
        ]
    , textStyled razorDim "railgun · shell-autocomplete · 39 targets · dhall → nix → exec"
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- Stats Row
-- ════════════════════════════════════════════════════════════════════════════

statsRowWidget :: [Package] -> Int -> Widget
statsRowWidget pkgs elapsed =
  hboxWith [Fill 1, Fill 1, Fill 1]
    [ statCard "TARGETS" (T.pack (show done)) (Just ("/ " <> T.pack (show total))) razorBright
    , statCard "ELAPSED" (formatElapsed elapsed) (Just "s") razorBright
    , statCard "CACHE HITS" "0" (Just "%") razorMiss
    ]
  where
    total = length pkgs
    done = length [p | p <- pkgs, elapsed >= pkgStart p + pkgTime p]

statCard :: Text -> Text -> Maybe Text -> Style -> Widget
statCard label value mUnit valueStyle =
  borderedStyled razorRule $
    padded 0 1 0 1 $
      vbox
        [ textStyled razorMuted label
        , hbox $
            [ textStyled (bold valueStyle) value ] ++
            maybe [] (\u -> [textStyled razorMuted u]) mUnit
        ]

-- | Format elapsed time as "X.Y" (without the 's')
formatElapsed :: Int -> Text
formatElapsed ms =
  let s = fromIntegral ms / 1000.0 :: Double
      whole = floor s :: Int
      frac = round ((s - fromIntegral whole) * 10) :: Int
  in T.pack (show whole) <> "." <> T.pack (show (frac `mod` 10))

-- ════════════════════════════════════════════════════════════════════════════
-- Groups (single column)
-- ════════════════════════════════════════════════════════════════════════════

groupsWidget :: [Package] -> Int -> Widget
groupsWidget pkgs elapsed =
  let groupWidgets = map (groupWidget pkgs elapsed) allGroups
      -- Use Exact constraints for each group based on line count
      groupSizes = map (groupLineCount pkgs) allGroups
      constraints = map Exact groupSizes
  in vboxWith constraints groupWidgets
  where
    groupLineCount ps g = 
      let items = filter (groupFilter g) ps
      in if null items then 0 else 1 + length items

groupWidget :: [Package] -> Int -> PackageGroup -> Widget
groupWidget pkgs elapsed PackageGroup{..} =
  let items = filter groupFilter pkgs
      done = length [p | p <- items, elapsed >= pkgStart p + pkgTime p]
      total = length items
      countStyle = if done == total then razorAccent else razorMuted
      -- Use Exact 1 for each line
      constraints = replicate (1 + length items) (Exact 1)
  in if null items then empty else
     vboxWith constraints $
       [ groupHeaderWidget groupLabel done total countStyle
       ] ++ map (pkgRow elapsed) items

groupHeaderWidget :: Text -> Int -> Int -> Style -> Widget
groupHeaderWidget label done total countStyle =
  -- Count column: " X/YY" - max width is " 39/39" = 6 chars, pad to 7 for spacing
  let countText = T.pack (show done) <> "/" <> T.pack (show total)
      paddedCount = padTextLeft 6 countText  -- right-align the count
      labelText = " " <> label <> " "
      labelLen = T.length labelText
  in hboxWith [Exact 2, Exact labelLen, Fill 1, Exact 7]
       [ textStyled razorAccent "//"
       , textStyled razorMuted labelText
       , fill razorRule '─'
       , textStyled countStyle (" " <> paddedCount)
       ]

pkgRow :: Int -> Package -> Widget
pkgRow elapsed pkg =
  let rawPct = clamp01 (fromIntegral (elapsed - pkgStart pkg) / fromIntegral (pkgTime pkg))
      pct = perceptual rawPct
      done = rawPct >= 1
      active = elapsed >= pkgStart pkg && not done
      waiting = elapsed < pkgStart pkg

      glyph | done      = "✓"
            | active    = "→"
            | otherwise = "○"

      glyphStyle | done      = razorAccent
                 | active    = razorAccent
                 | otherwise = razorDim

      -- Completed packages are slightly dimmed, waiting are very dim
      nameStyle | done      = razorMuted
                | active    = razorBright
                | otherwise = razorDim

      timeStr | done      = formatSecs (pkgTime pkg)
              | active    = formatSecs (elapsed - pkgStart pkg)
              | otherwise = ""

      timeStyle = razorMuted

      barStyle = razorAccent
      barEmptyStyle = razorDim
      
      -- Pad name to fixed width so progress bars align
      nameText = "//" <> pkgName pkg
      paddedName = padTextRight 40 nameText  -- longest name is 38 chars + //
  in if waiting
       -- Waiting rows: extend the fill to cover time column (no trailing spaces)
       then hboxWith [Exact 2, Exact 40, Exact 1, Fill 1]
              [ textStyled glyphStyle (glyph <> " ")
              , textStyled nameStyle paddedName
              , textStyled razorDim " "
              , fill razorDim '─'
              ]
       -- Active/completed rows: show progress bar and time
       else hboxWith [Exact 2, Exact 40, Exact 1, Fill 1, Exact 1, Exact 5]
              [ textStyled glyphStyle (glyph <> " ")
              , textStyled nameStyle paddedName
              , textStyled razorDim " "
              , progressThin barStyle barEmptyStyle pct
              , textStyled razorDim " "
              , textStyled timeStyle (padTextLeft 5 timeStr)
              ]

-- | Pad text to width on the right (left-align)
padTextRight :: Int -> Text -> Text
padTextRight w t = 
  let len = T.length t
  in if len >= w then T.take w t else t <> T.replicate (w - len) " "

-- | Pad text to width on the left (right-align)
padTextLeft :: Int -> Text -> Text
padTextLeft w t =
  let len = T.length t
  in if len >= w then t else T.replicate (w - len) " " <> t

-- | Thin progress bar for rows
progressThin :: Style -> Style -> Double -> Widget
progressThin filledStyle emptyStyle pct = Widget $ \dims ->
  let w = width dims
      filled = max 0 (min w (round (pct * fromIntegral w)))
      unfilled = w - filled
      bar = Seq.fromList
              [ Span filledStyle (T.replicate filled "━")
              , Span emptyStyle (T.replicate unfilled "─")
              ]
  in Canvas (V.singleton bar) (Dimensions w 1)

-- ════════════════════════════════════════════════════════════════════════════
-- Complete
-- ════════════════════════════════════════════════════════════════════════════

completeWidget :: Dimensions -> [Package] -> Int -> Widget
completeWidget _dims pkgs makespan =
  vboxWith [Exact 3, Exact 1, Exact 4, Exact 1, Fill 1, Exact 1, Exact 1, Exact 1]
    [ headerWidget
    , space 0 1
    , statsRowWidget pkgs makespan
    , space 0 1
    , groupsWidget pkgs makespan
    , space 0 1
    , centered $
        hbox
          [ textStyled razorAccent "✓ "
          , textStyled razorBright "BUILD COMPLETE"
          , textStyled razorMuted " · "
          , textStyled razorBright (T.pack (show (length pkgs)) <> " TARGETS")
          , textStyled razorMuted " · "
          , textStyled razorBright (formatSecs makespan)
          ]
    , footerWidget
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- Footer
-- ════════════════════════════════════════════════════════════════════════════

footerWidget :: Widget
footerWidget =
  hbox
    [ textStyled razorMuted "SENSENET · DHALL + NIX + CAS"
    , fill defaultStyle ' '
    , textStyled razorDim "the one rectilinear chamber in the complex"
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- Helpers
-- ════════════════════════════════════════════════════════════════════════════

formatSecs :: Int -> Text
formatSecs ms =
  let s = fromIntegral ms / 1000.0 :: Double
      whole = floor s :: Int
      frac = round ((s - fromIntegral whole) * 10) :: Int
  in T.pack (show whole) <> "." <> T.pack (show (frac `mod` 10)) <> "s"

clamp01 :: Double -> Double
clamp01 x = max 0 (min 1 x)
