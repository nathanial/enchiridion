import Lake
open Lake DSL System

package enchiridion where
  precompileModules := true

require terminus from git "https://github.com/nathanial/terminus" @ "v0.0.2"
require oracle from git "https://github.com/nathanial/oracle" @ "v0.0.1"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.1"
require staple from git "https://github.com/nathanial/staple" @ "v0.0.2"

-- Curl link args (inherited from wisp, but needed for our executables)
def curlLinkArgs : Array String :=
  if Platform.isOSX then
    #["-L/opt/homebrew/lib",
      "-L/usr/local/lib",
      "-L/opt/homebrew/anaconda3/lib",
      "-lcurl",
      "-Wl,-rpath,/opt/homebrew/lib",
      "-Wl,-rpath,/opt/homebrew/anaconda3/lib",
      "-Wl,-rpath,/usr/local/lib"]
  else if Platform.isWindows then
    #["-lcurl"]
  else
    #["-lcurl", "-Wl,-rpath,/usr/lib", "-Wl,-rpath,/usr/local/lib"]

@[default_target]
lean_lib Enchiridion where
  roots := #[`Enchiridion]
  moreLinkArgs := curlLinkArgs

lean_lib Tests where
  roots := #[`Tests]
  moreLinkArgs := curlLinkArgs

lean_exe enchiridion where
  root := `Main
  moreLinkArgs := curlLinkArgs

@[test_driver]
lean_exe tests where
  root := `Tests.Main
  moreLinkArgs := curlLinkArgs
