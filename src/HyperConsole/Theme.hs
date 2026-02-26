{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Straylight theme - tasteful defaults for terminal UI
--
-- "The sky above the port was the color of television,
--  tuned to a dead channel."
--
--                                      — Neuromancer
--
-- = Design Philosophy
--
-- The straylight aesthetic is defined by:
--
--   * __Restraint__ — Minimal color, maximum clarity
--   * __Cool tones__ — Cyan and blue for action, warmth reserved for attention
--   * __Hierarchy__ — Dim for secondary, bright for primary
--   * __Monospace harmony__ — Typography that breathes
--
-- = Color Palette
--
-- The palette is derived from terminal colors that work across
-- dark and light themes, with semantic meaning:
--
-- @
-- ┌────────────┬────────────┬─────────────────────────────┐
-- │ Semantic   │ Color      │ Usage                       │
-- ├────────────┼────────────┼─────────────────────────────┤
-- │ primary    │ white      │ Main content, emphasis      │
-- │ secondary  │ bright black│ Subdued, metadata          │
-- │ accent     │ cyan       │ Actions, progress, links    │
-- │ success    │ green      │ Completion, checkmarks      │
-- │ warning    │ yellow     │ Caution, deprecation        │
-- │ error      │ red        │ Failures, blocking issues   │
-- │ muted      │ dim white  │ Borders, separators         │
-- └────────────┴────────────┴─────────────────────────────┘
-- @
--
-- = Usage
--
-- @
-- import HyperConsole
-- import HyperConsole.Theme
--
-- main = do
--   let widget = vbox
--         [ textStyled themePrimary "sensenet"
--         , textStyled themeSecondary "v0.4.0"
--         , progressBar themeProgressFilled themeProgressEmpty themeSecondary 0.75
--         ]
--   ...
-- @
module HyperConsole.Theme
  ( -- * Semantic Styles
    -- $semantic
    themePrimary,
    themeSecondary,
    themeAccent,
    themeSuccess,
    themeWarning,
    themeError,
    themeMuted,

    -- * UI Element Styles
    -- $elements
    themeBorder,
    themeTitle,
    themeLabel,
    themeValue,
    themeSelected,
    themeHeader,

    -- * Progress Styles
    themeProgressFilled,
    themeProgressEmpty,
    themeSpinner,

    -- * Status Styles
    themeStatusOk,
    themeStatusPending,
    themeStatusFailed,
    themeStatusCached,

    -- * Box Drawing
    -- $boxdrawing
    boxLight,
    boxHeavy,
    boxDouble,
    boxRounded,

    -- * Glyphs
    -- $glyphs
    glyphCheck,
    glyphCross,
    glyphArrow,
    glyphBullet,
    glyphSpinner,
    glyphProgress,
    glyphProgressEmpty,
    glyphCached,
    glyphBuilding,
  )
where

import Data.Text (Text)
import HyperConsole.Style

-- ════════════════════════════════════════════════════════════════════════════
-- Semantic Styles
-- ════════════════════════════════════════════════════════════════════════════

-- $semantic
-- Core semantic styles that convey meaning through color.
-- These work on both dark and light terminal backgrounds.

-- | Primary content — the main focus
--
-- White text, default weight. Use for headings, important values,
-- and anything the user should see first.
themePrimary :: Style
themePrimary = fg White

-- | Secondary content — supporting information
--
-- Bright black (dark gray). Use for metadata, timestamps,
-- file paths, and context that supports but doesn't compete.
themeSecondary :: Style
themeSecondary = fg BrightBlack

-- | Accent — calls to action, links, in-progress
--
-- Cyan. The signature straylight color. Use for:
-- * Active/in-progress operations
-- * Interactive elements
-- * Emphasis within body text
themeAccent :: Style
themeAccent = fg Cyan

-- | Success — completion, confirmation
--
-- Green. Use sparingly for:
-- * Build success
-- * Tests passed
-- * Operations completed
themeSuccess :: Style
themeSuccess = fg Green

-- | Warning — caution, non-blocking issues
--
-- Yellow. Use for:
-- * Deprecation notices
-- * Non-fatal warnings
-- * Things that need attention but aren't errors
themeWarning :: Style
themeWarning = fg Yellow

-- | Error — failures, blocking issues
--
-- Red, bright. The only "loud" color. Use for:
-- * Build failures
-- * Test failures
-- * Fatal errors
themeError :: Style
themeError = fg BrightRed

-- | Muted — decorative, structural
--
-- Dim white. Use for:
-- * Borders and boxes
-- * Separators
-- * Background structure
themeMuted :: Style
themeMuted = dim (fg White)

-- ════════════════════════════════════════════════════════════════════════════
-- UI Element Styles
-- ════════════════════════════════════════════════════════════════════════════

-- $elements
-- Styles for common UI elements.

-- | Border style — box drawing characters
themeBorder :: Style
themeBorder = fg BrightBlack

-- | Title style — section headers in boxes
themeTitle :: Style
themeTitle = bold (fg White)

-- | Label style — keys in key-value pairs
themeLabel :: Style
themeLabel = fg BrightBlack

-- | Value style — values in key-value pairs
themeValue :: Style
themeValue = fg White

-- | Selected item — highlighted in a list
themeSelected :: Style
themeSelected = bold (bg BrightBlack <> fg White)

-- | Table header
themeHeader :: Style
themeHeader = bold (fg Cyan)

-- ════════════════════════════════════════════════════════════════════════════
-- Progress Styles
-- ════════════════════════════════════════════════════════════════════════════

-- | Filled portion of progress bar
themeProgressFilled :: Style
themeProgressFilled = fg Cyan

-- | Empty portion of progress bar
themeProgressEmpty :: Style
themeProgressEmpty = fg BrightBlack

-- | Spinner style
themeSpinner :: Style
themeSpinner = fg Cyan

-- ════════════════════════════════════════════════════════════════════════════
-- Status Styles
-- ════════════════════════════════════════════════════════════════════════════

-- | OK/success status
themeStatusOk :: Style
themeStatusOk = fg Green

-- | Pending/in-progress status
themeStatusPending :: Style
themeStatusPending = fg Cyan

-- | Failed status
themeStatusFailed :: Style
themeStatusFailed = fg BrightRed

-- | Cached/skipped status
themeStatusCached :: Style
themeStatusCached = fg BrightBlack

-- ════════════════════════════════════════════════════════════════════════════
-- Box Drawing Characters
-- ════════════════════════════════════════════════════════════════════════════

-- $boxdrawing
-- Unicode box drawing character sets for borders.
-- Each set provides (topLeft, top, topRight, left, right, bottomLeft, bottom, bottomRight)

-- | Light box drawing (single line)
--
-- @
-- ┌──────┐
-- │      │
-- └──────┘
-- @
boxLight :: (Char, Char, Char, Char, Char, Char, Char, Char)
boxLight = ('┌', '─', '┐', '│', '│', '└', '─', '┘')

-- | Heavy box drawing (thick lines)
--
-- @
-- ┏━━━━━━┓
-- ┃      ┃
-- ┗━━━━━━┛
-- @
boxHeavy :: (Char, Char, Char, Char, Char, Char, Char, Char)
boxHeavy = ('┏', '━', '┓', '┃', '┃', '┗', '━', '┛')

-- | Double box drawing
--
-- @
-- ╔══════╗
-- ║      ║
-- ╚══════╝
-- @
boxDouble :: (Char, Char, Char, Char, Char, Char, Char, Char)
boxDouble = ('╔', '═', '╗', '║', '║', '╚', '═', '╝')

-- | Rounded box drawing (curved corners)
--
-- @
-- ╭──────╮
-- │      │
-- ╰──────╯
-- @
boxRounded :: (Char, Char, Char, Char, Char, Char, Char, Char)
boxRounded = ('╭', '─', '╮', '│', '│', '╰', '─', '╯')

-- ════════════════════════════════════════════════════════════════════════════
-- Glyphs
-- ════════════════════════════════════════════════════════════════════════════

-- $glyphs
-- Unicode glyphs for common UI elements.
-- These are chosen for:
-- * Wide terminal support (most fonts have these)
-- * Clear meaning at small sizes
-- * Visual harmony in monospace

-- | Checkmark — success, done, enabled
glyphCheck :: Text
glyphCheck = "✓"

-- | Cross — failure, error, disabled
glyphCross :: Text
glyphCross = "✗"

-- | Arrow — output, result, points to
glyphArrow :: Text
glyphArrow = "→"

-- | Bullet — list item
glyphBullet :: Text
glyphBullet = "•"

-- | Spinner frames — animated progress indicator
--
-- Braille pattern spinner (smooth rotation):
-- ⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏
glyphSpinner :: Text
glyphSpinner = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

-- | Progress filled block
glyphProgress :: Text
glyphProgress = "█"

-- | Progress empty block
glyphProgressEmpty :: Text
glyphProgressEmpty = "░"

-- | Cached indicator — hollow circle
glyphCached :: Text
glyphCached = "◉"

-- | Building indicator — rotating arrows
glyphBuilding :: Text
glyphBuilding = "⟳"
