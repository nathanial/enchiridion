import Lake
open Lake DSL System

package enchiridion where
  precompileModules := true

-- Local workspace dependencies
require terminus from ".." / "terminus"
require oracle from ".." / "oracle"
require crucible from ".." / "crucible"

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
