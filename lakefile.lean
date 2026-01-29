import Lake
open Lake DSL

package cleanode where
  version := v!"0.1.0"
  moreLinkArgs := #["-L./native", "-lcleanode_native"]

@[default_target]
lean_lib Cleanode

@[default_target]
lean_exe cleanode where
  root := `Main
  supportInterpreter := true
