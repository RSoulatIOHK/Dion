import Cleanode.TUI.Ansi
import Cleanode.TUI.State
import Cleanode.TUI.Layout
import Cleanode.TUI.Input

/-!
# TUI Render Loop

Manages the render lifecycle: switches to alternate screen buffer,
hides cursor, redraws every 200ms, and restores terminal on exit.
Also starts the keyboard input handler.
-/

namespace Cleanode.TUI.Render

open Cleanode.TUI.Ansi
open Cleanode.TUI.State
open Cleanode.TUI.Layout
open Cleanode.TUI.Input

/-- Initialize the terminal for TUI mode -/
def initTerminal : IO Unit := do
  IO.print Ansi.altScreenOn
  IO.print Ansi.hideCursor
  IO.print Ansi.clearScreen
  let _ ← IO.getStdout >>= (·.flush)

/-- Restore the terminal to normal mode -/
def restoreTerminal : IO Unit := do
  disableRawMode
  IO.print Ansi.showCursor
  IO.print Ansi.altScreenOff
  let _ ← IO.getStdout >>= (·.flush)

/-- Get current time in milliseconds (monotonic) -/
def nowMs : IO Nat := do
  let t ← IO.monoNanosNow
  return t / 1000000

/-- The main TUI render loop. Reads state, renders frame, sleeps. -/
partial def tuiRenderLoop (stateRef : IO.Ref TUIState) : IO Unit := do
  initTerminal
  -- Start keyboard input handler in background
  let _inputTask ← startInputHandler stateRef
  let rec loop : IO Unit := do
    let state ← stateRef.get
    let now ← nowMs
    let frame := renderFrame state now
    -- Move cursor home and overwrite (no clear = no flicker)
    IO.print (Ansi.home ++ frame)
    let _ ← IO.getStdout >>= (·.flush)
    IO.sleep 200  -- Faster refresh for responsive input
    loop
  loop

/-- Start the TUI render loop as a background task -/
def startTUI (stateRef : IO.Ref TUIState) : IO (Task (Except IO.Error Unit)) := do
  IO.asTask (tuiRenderLoop stateRef)

/-- Stop the TUI and restore terminal (call on shutdown) -/
def stopTUI : IO Unit :=
  restoreTerminal

end Cleanode.TUI.Render
