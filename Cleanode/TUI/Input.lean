import Cleanode.TUI.State

/-!
# TUI Keyboard Input

Raw terminal input handling via FFI. Reads keypresses and dispatches
them to TUIState navigation functions.
-/

namespace Cleanode.TUI.Input

open Cleanode.TUI.State

-- FFI declarations for terminal raw mode
@[extern "cleanode_terminal_enable_raw"]
opaque enableRawMode : IO Bool

@[extern "cleanode_terminal_disable_raw"]
opaque disableRawMode : IO Unit

@[extern "cleanode_terminal_read_key"]
opaque readKey : IO ByteArray

/-- Keyboard event type -/
inductive KeyEvent where
  | arrowUp
  | arrowDown
  | arrowLeft
  | arrowRight
  | enter
  | escape
  | space
  | charKey (c : Char)
  | none  -- No key available (timeout)
  deriving Repr

/-- Parse raw bytes from readKey into a KeyEvent -/
def parseKey (bytes : ByteArray) : KeyEvent :=
  if bytes.size == 0 then .none
  else if bytes.size == 3 && bytes[0]! == 27 && bytes[1]! == 91 then
    match bytes[2]! with
    | 65 => .arrowUp
    | 66 => .arrowDown
    | 67 => .arrowRight
    | 68 => .arrowLeft
    | _  => .none
  else if bytes.size == 1 then
    match bytes[0]! with
    | 13 => .enter
    | 27 => .escape
    | 32 => .space
    | b  => .charKey (Char.ofNat b.toNat)
  else .none

/-- Apply a key event to TUI state -/
def handleKey (key : KeyEvent) (s : TUIState) : TUIState :=
  match key with
  | .arrowUp    => s.selectUp
  | .arrowDown  => s.selectDown
  | .enter      => s.selectEnter
  | .escape     => s.selectBack
  | .space      => s.togglePause
  | .charKey 'c' => s.toggleConsensus
  | .charKey 'k' => s.selectUp
  | .charKey 'j' => s.selectDown
  | _           => s

/-- Input handler loop: reads keys and updates TUI state.
    Runs in a background task. -/
partial def inputLoop (stateRef : IO.Ref TUIState) : IO Unit := do
  let ok ← enableRawMode
  if !ok then
    IO.eprintln "[TUI] Warning: could not enable raw terminal mode"
    return
  let rec loop : IO Unit := do
    let bytes ← readKey
    let key := parseKey bytes
    match key with
    | .none => loop  -- No input, keep polling
    | .charKey 'q' =>
      -- 'q' quits — restore terminal handled by caller
      disableRawMode
      -- Signal quit by setting a flag (we'll just return and let the task end)
      return
    | k => do
      stateRef.modify (handleKey k)
      loop
  loop

/-- Start the input handler as a background task -/
def startInputHandler (stateRef : IO.Ref TUIState) : IO (Task (Except IO.Error Unit)) :=
  IO.asTask (inputLoop stateRef)

end Cleanode.TUI.Input
