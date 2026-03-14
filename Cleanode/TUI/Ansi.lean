/-!
# ANSI Escape Code Helpers

Raw ANSI escape sequences for terminal control: cursor movement,
screen clearing, alternate screen buffer, and cursor visibility.

Pigment handles color/style but not cursor positioning, so we
output these directly as string literals.
-/

namespace Cleanode.TUI.Ansi

/-- ESC[ control sequence introducer -/
def csi : String := "\x1b["

/-- Clear entire screen -/
def clearScreen : String := s!"{csi}2J"

/-- Move cursor to position (1-based row, col) -/
def moveTo (row col : Nat) : String := s!"{csi}{row};{col}H"

/-- Move cursor to top-left corner -/
def home : String := s!"{csi}H"

/-- Hide the cursor -/
def hideCursor : String := s!"{csi}?25l"

/-- Show the cursor -/
def showCursor : String := s!"{csi}?25h"

/-- Switch to alternate screen buffer (preserves scrollback) -/
def altScreenOn : String := s!"{csi}?1049h"

/-- Restore original screen buffer -/
def altScreenOff : String := s!"{csi}?1049l"

/-- Erase from cursor to end of line -/
def clearLine : String := s!"{csi}K"

/-- Erase entire line -/
def clearFullLine : String := s!"{csi}2K"

/-- Reset all ANSI attributes -/
def reset : String := s!"{csi}0m"

/-- Bold text -/
def bold : String := s!"{csi}1m"

/-- Dim text -/
def dim : String := s!"{csi}2m"

/-- Foreground color by code (30-37 basic, 90-97 bright) -/
def fg (code : Nat) : String := s!"{csi}{code}m"

/-- Cyan foreground -/
def cyan : String := fg 36

/-- Yellow foreground -/
def yellow : String := fg 33

/-- Green foreground -/
def green : String := fg 32

/-- Blue foreground -/
def blue : String := fg 34

/-- Red foreground -/
def red : String := fg 31

/-- Magenta foreground -/
def magenta : String := fg 35

/-- White foreground -/
def white : String := fg 37

/-- Bright cyan foreground -/
def brightCyan : String := fg 96

/-- Bright yellow foreground -/
def brightYellow : String := fg 93

/-- Bright green foreground -/
def brightGreen : String := fg 92

/-- Compute the visible length of a string, ignoring ANSI escape sequences.
    An ANSI sequence starts with ESC[ and ends at the first letter (m, H, J, K, etc). -/
def visibleLength (s : String) : Nat :=
  let chars := s.toList
  go chars 0 false
where
  go : List Char → Nat → Bool → Nat
    | [], count, _ => count
    | '\x1b' :: '[' :: rest, count, _ => go rest count true  -- start of CSI sequence
    | c :: rest, count, true =>
        -- Inside CSI: skip until we hit a letter (the terminator)
        if (c.toNat >= 64 && c.toNat <= 126) then go rest count false  -- terminator found
        else go rest count true  -- still inside sequence
    | _ :: rest, count, false => go rest (count + 1) false

/-- Pad or truncate a string to exactly `width` visible characters.
    ANSI escape codes are not counted toward width. -/
def padRight (s : String) (width : Nat) : String :=
  let vis := visibleLength s
  if vis >= width then s
  else s ++ String.mk (List.replicate (width - vis) ' ')

/-- Pad left with spaces (visible-width aware) -/
def padLeft (s : String) (width : Nat) : String :=
  let vis := visibleLength s
  if vis >= width then s
  else String.mk (List.replicate (width - vis) ' ') ++ s

end Cleanode.TUI.Ansi
