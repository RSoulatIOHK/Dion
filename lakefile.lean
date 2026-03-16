import Lake
open Lake DSL

package cleanode where
  version := v!"0.1.0"
  moreLinkArgs := #["-L./native", "-lcleanode_native", "-L/opt/homebrew/opt/curl/lib", "-lcurl", "-L/opt/homebrew/opt/zstd/lib", "-lzstd"]

require Pigment from git
  "https://github.com/RSoulatIOHK/Pigment" @ "comp-4.24.0"

@[default_target]
lean_lib Cleanode

@[default_target]
lean_exe cleanode where
  root := `Main
  supportInterpreter := true

lean_exe test where
  root := `TestMain
  supportInterpreter := true
