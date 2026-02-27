# HyperConsole v2 Architecture

> "The sky above the port was the color of television, tuned to a dead channel."
>
> — Neuromancer

## Overview

HyperConsole v2 is a production-grade TUI framework built on three pillars:

1. **notcurses** — High-performance terminal rendering via FFI
2. **libevring** — Deterministic async I/O with pure state machines
3. **hydrogen patterns** — Type-safe data fetching and UI state management

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           HYPERCONSOLE v2                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│  APPLICATION LAYER                                                          │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │ Sensenet Dashboard │ Agent Monitor │ Build System TUI │ Custom Apps    ││
│  └─────────────────────────────────────────────────────────────────────────┘│
├─────────────────────────────────────────────────────────────────────────────┤
│  WIDGET LAYER (Pure Functional)                                             │
│  ┌───────────────┬───────────────┬───────────────┬─────────────────────────┐│
│  │ Layout Engine │ Style System  │ Theme         │ Composable Widgets      ││
│  │ (flexbox)     │ (Nord/Razor)  │ (semantic)    │ (progress,table,tree)   ││
│  └───────────────┴───────────────┴───────────────┴─────────────────────────┘│
├─────────────────────────────────────────────────────────────────────────────┤
│  EVENT LAYER (libevring Machine abstraction)                                │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │ TuiMachine :: Machine where                                             ││
│  │   State = (AppState, RenderState)                                       ││
│  │   step :: State -> Event -> (State, [Operation])                        ││
│  └─────────────────────────────────────────────────────────────────────────┘│
├─────────────────────────────────────────────────────────────────────────────┤
│  RENDERING BACKEND (notcurses FFI)                                          │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │ HyperConsole.Backend.Notcurses                                          ││
│  │   - ncplane management (one per widget layer)                           ││
│  │   - Direct cell manipulation (no intermediate ANSI)                     ││
│  │   - Input handling (ncinput)                                            ││
│  │   - Sixel/Kitty graphics (ncvisual)                                     ││
│  └─────────────────────────────────────────────────────────────────────────┘│
├─────────────────────────────────────────────────────────────────────────────┤
│  I/O LAYER (libevring io_uring)                                             │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │ System.IoUring + Evring.Ring                                            ││
│  │   - Batched completions for input events                                ││
│  │   - Async file watching (build status)                                  ││
│  │   - Network events (agent communication)                                ││
│  │   - Timer events (animation frames)                                     ││
│  └─────────────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────────────┘
```

## Design Principles

### Pure Functional Core

Widgets remain pure functions from dimensions to canvases:

```haskell
newtype Widget = Widget { runWidget :: Dimensions -> Canvas }
```

No state, no effects, just pure rendering. IO happens at the edges.

### Deterministic Event Processing

The TUI is modeled as an evring `Machine`:

```haskell
class Machine m where
  type State m
  initial :: m -> State m
  step :: m -> State m -> Event -> StepResult (State m)
  done :: m -> State m -> Bool
```

This enables:
- **Replay testing** — Feed recorded events, verify final state
- **Time-travel debugging** — Step forward/backward through states
- **Deterministic builds** — Same events → same output

### Capability-Based Rendering

notcurses provides:
- True color without terminal detection heuristics
- Proper Unicode (grapheme clusters, not just codepoints)
- Sixel/Kitty image protocol for visualizations
- Built-in damage tracking (no manual diffing)

## Module Structure

```
src/
├── HyperConsole.hs                    -- Public API (stable)
├── HyperConsole/
│   ├── Widget.hs                      -- Pure widget combinators
│   ├── Layout.hs                      -- Flexbox constraint solver
│   ├── Style.hs                       -- Colors and attributes
│   ├── Theme.hs                       -- Semantic color palette
│   ├── Unicode.hs                     -- Display width calculation
│   │
│   ├── Machine.hs                     -- TUI state machine
│   ├── Event.hs                       -- Input/timer/app events
│   │
│   ├── Backend/
│   │   ├── Notcurses.hs               -- Primary backend
│   │   ├── Notcurses/
│   │   │   ├── FFI.hs                 -- Raw C bindings
│   │   │   ├── Plane.hs               -- ncplane operations
│   │   │   ├── Cell.hs                -- nccell manipulation
│   │   │   ├── Input.hs               -- ncinput parsing
│   │   │   └── Visual.hs              -- ncvisual (images)
│   │   └── ANSI.hs                    -- Fallback backend
│   │
│   ├── Data/
│   │   ├── RemoteData.hs              -- Async state (NotAsked|Loading|Failure|Success)
│   │   └── Query.hs                   -- Data fetching with caching
│   │
│   └── Terminal.hs                    -- Legacy API (deprecated)
```

## Event Model

### TUI Events

```haskell
data TuiEvent app
  = TuiInput InputEvent        -- Keyboard/mouse
  | TuiResize Int Int          -- Terminal resize
  | TuiTick Int                -- Animation frame (ms elapsed)
  | TuiAppEvent app            -- Application-specific
  | TuiFileChanged FilePath    -- inotify via io_uring
  | TuiNetworkEvent ByteString -- Socket data
```

### TUI Operations

```haskell
data TuiOp
  = TuiRender Widget           -- Render widget tree
  | TuiWatchFile FilePath      -- Start watching file
  | TuiConnect SockAddr        -- Open connection
  | TuiSend Handle ByteString  -- Send data
  | TuiSetTimer Int            -- Schedule tick
  | TuiQuit                    -- Exit
```

### Machine Definition

```haskell
data TuiMachine app = TuiMachine
  { tuiInitial :: app
  , tuiUpdate  :: app -> TuiEvent app -> (app, [TuiOp])
  , tuiView    :: app -> Widget
  , tuiDone    :: app -> Bool
  }

instance Machine (TuiMachine app) where
  type State (TuiMachine app) = app
  initial = tuiInitial
  step m s e = let (s', ops) = tuiUpdate m s (fromEvringEvent e)
               in StepResult s' (map toEvringOp ops)
  done = tuiDone
```

## Notcurses Backend

### Why notcurses?

| Feature | ANSI (current) | notcurses |
|---------|----------------|-----------|
| Color support | Detection heuristics | Always true color |
| Unicode | Manual width calc | Grapheme clusters |
| Damage tracking | Manual diffing | Built-in |
| Images | Not supported | Sixel/Kitty/iTerm2 |
| Input parsing | Basic | Full (mouse, paste, etc.) |
| Performance | N writes/frame | Single
render call |

### FFI Bindings

```haskell
-- Core initialization
foreign import ccall unsafe "notcurses_init"
  c_notcurses_init :: Ptr NcOptions -> Ptr CFile -> IO (Ptr Notcurses)

foreign import ccall unsafe "notcurses_stop"
  c_notcurses_stop :: Ptr Notcurses -> IO CInt

-- Rendering
foreign import ccall unsafe "notcurses_render"
  c_notcurses_render :: Ptr Notcurses -> IO CInt

-- Plane operations
foreign import ccall unsafe "ncplane_putc_yx"
  c_ncplane_putc_yx :: Ptr NcPlane -> CInt -> CInt -> Ptr NcCell -> IO CInt

foreign import ccall unsafe "ncplane_erase"
  c_ncplane_erase :: Ptr NcPlane -> IO ()

-- Input
foreign import ccall unsafe "notcurses_get"
  c_notcurses_get :: Ptr Notcurses -> Ptr Timespec -> Ptr NcInput -> IO Word32
```

### Canvas to Plane Translation

```haskell
renderCanvas :: NcPlane -> Canvas -> IO ()
renderCanvas plane canvas = do
  ncplane_erase plane
  V.iforM_ (canvasLines canvas) $ \y line ->
    renderLine plane y line
  where
    renderLine plane y spans = do
      let go _ [] = pure ()
          go x (Span style text : rest) = do
            cell <- styleToCell style text
            ncplane_putc_yx plane y x cell
            go (x + displayWidth text) rest
      go 0 (toList spans)
```

## libevring Integration

### io_uring Event Loop

```haskell
runTuiWithIoUring :: TuiMachine app -> IO app
runTuiWithIoUring machine =
  withIoUring defaultParams $ \ctx ->
    withNotcurses $ \nc -> do
      -- Register notcurses input fd
      let inputFd = ncInputFd nc
      registerFiles ctx [inputFd]
      
      -- Run machine
      run defaultRingConfig (wrapMachine nc machine)
```

### Replay Testing

```haskell
-- Record a session
runTuiTraced :: TuiMachine app -> IO (app, Trace)
runTuiTraced machine = runTraced defaultRingConfig (wrapMachine machine)

-- Replay for testing
testTui :: TuiMachine app -> [TuiEvent app] -> app
testTui machine events = replay (wrapMachine machine) (map toEvringEvent events)
```

## Hydrogen Patterns

### RemoteData

```haskell
data RemoteData e a
  = NotAsked              -- Haven't started
  | Loading               -- In progress
  | Failure e             -- Failed with error
  | Success a             -- Completed with value
  deriving (Eq, Show, Functor)

instance Applicative (RemoteData e) where ...
instance Monad (RemoteData e) where ...
```

### RemoteData-Aware Widgets

```haskell
remoteWidget
  :: RemoteData e a
  -> (a -> Widget)        -- Success renderer
  -> Widget               -- Loading state
  -> (e -> Widget)        -- Error renderer
  -> Widget
remoteWidget NotAsked _ loading _ = loading
remoteWidget Loading _ loading _ = loading
remoteWidget (Failure e) _ _ errW = errW e
remoteWidget (Success a) render _ _ = render a
```

### Example: Build Dashboard

```haskell
buildDashboard :: RemoteData BuildError BuildState -> Widget
buildDashboard status = remoteWidget status
  renderBuildState
  (centered $ vbox
    [ spinner themeSpinner 0
    , textStyled themeMuted "Loading build status..."
    ])
  (\err -> errorCard $ case err of
    ConnectionFailed -> "Cannot connect to build server"
    ParseError msg -> "Invalid response: " <> msg)
```

## Implementation Roadmap

### Phase 1: Notcurses FFI (2-3 weeks)

- [ ] Raw FFI bindings in `HyperConsole.Backend.Notcurses.FFI`
- [ ] `NcPlane` abstraction with RAII
- [ ] `renderCanvas :: NcPlane -> Canvas -> IO ()`
- [ ] Input event parsing
- [ ] Port demos to notcurses backend
- [ ] Fallback detection (use ANSI if notcurses unavailable)

### Phase 2: Evring Integration (2 weeks)

- [ ] `TuiEvent` and `TuiOp` types
- [ ] `Machine` instance for `TuiMachine`
- [ ] io_uring event loop integration
- [ ] `replay` testing infrastructure
- [ ] Timer and file-watch operations

### Phase 3: Hydrogen Patterns (1 week)

- [ ] `RemoteData` type with lawful instances
- [ ] `Query` abstraction for data fetching
- [ ] `remoteWidget` combinator
- [ ] Caching and deduplication

### Phase 4: Production Dashboard (2 weeks)

- [ ] Real sensenet event source
- [ ] Agent status monitoring
- [ ] Build cache visualization
- [ ] Network topology view
- [ ] Performance profiling

## Dependencies

### Required

- **notcurses** >= 3.0.17 (C library)
- **libevring** (Haskell bindings to io_uring)

### Build

```nix
# flake.nix additions
buildInputs = [
  pkgs.notcurses
  haskellPackages.libevring
];
```

## Compatibility

### Terminals

notcurses handles terminal detection. Tested on:
- kitty (full features including graphics)
- alacritty (true color, no graphics)
- tmux/screen (with passthrough)
- xterm (256 color fallback)
- Linux console (basic)

### Platforms

- **Linux**: Full support (io_uring + notcurses)
- **macOS**: notcurses only (kqueue fallback for events)
- **Windows**: Not supported (use WSL2)

## References

- [notcurses documentation](https://notcurses.com/)
- [libevring repository](https://github.com/straylight-software/libevring)
- [hydrogen framework](https://github.com/straylight-software/hydrogen)
- [io_uring documentation](https://kernel.dk/io_uring.pdf)
