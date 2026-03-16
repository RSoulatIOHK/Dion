import Lake
open Lake DSL

package cleanode where
  version := v!"0.1.0"
  moreLinkArgs := #["-L./native", "-lcleanode_native", "-L/opt/homebrew/opt/curl/lib", "-lcurl", "-L/opt/homebrew/opt/zstd/lib", "-lzstd", "-L/usr/local/lib", "-lblst", "/opt/homebrew/lib/libgmp.a"]

require Pigment from "../Pigment"

@[default_target]
lean_lib Cleanode

@[default_target]
lean_exe cleanode where
  root := `Main
  supportInterpreter := true

lean_exe test where
  root := `TestMain
  supportInterpreter := true
