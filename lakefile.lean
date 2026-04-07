import Lake
open Lake DSL

package dion where
  version := v!"0.1.0"
  moreLinkArgs := #["-L./native", "-ldion_native", "-lplutuz_ffi", "-lplutuz_blst", "-llean_shim", "-L/opt/homebrew/opt/curl/lib", "-lcurl", "-L/opt/homebrew/opt/zstd/lib", "-lzstd", "-L/usr/local/lib", "-lblst", "/opt/homebrew/lib/libgmp.a"]

require Pigment from "../Pigment"

@[default_target]
lean_lib Dion

@[default_target]
lean_exe dion where
  root := `Main
  supportInterpreter := true

lean_exe test where
  root := `TestMain
  supportInterpreter := true
