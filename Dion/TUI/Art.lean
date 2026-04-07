import Dion.TUI.Ansi

/-!
# ASCII Art Logo

The "DION" logo rendered in block letters with ANSI color codes.
-/

namespace Dion.TUI.Art

open Dion.TUI.Ansi

/-- The DION ASCII art logo lines (raw, no color) -/
def dionLogoLines : List String :=
  [ " ____  ___ ___  _   _ "
  , "|  _ \\|_ _/ _ \\| \\ | |"
  , "| | | || | | | |  \\| |"
  , "| |_| || | |_| | |\\  |"
  , "|____/|___\\___/|_| \\_|" ]

/-- Render the DION logo with cyan bold coloring -/
def renderLogo : List String :=
  dionLogoLines.map fun line =>
    s!"{Ansi.bold}{Ansi.brightCyan}{line}{Ansi.reset}"

/-- Subtitle line -/
def subtitle : String :=
  s!"{Ansi.cyan}{Ansi.dim}  A Cardano LEAN 4 Node{Ansi.reset}"

end Dion.TUI.Art
