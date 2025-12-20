/-
  Enchiridion Configuration
  Load settings from config files and environment
-/

import Lean.Data.Json

namespace Enchiridion

open Lean Json

/-- Application configuration -/
structure Config where
  openRouterApiKey : String := ""
  defaultModel : String := "anthropic/claude-3.5-sonnet"
  autoSaveEnabled : Bool := false
  autoSaveIntervalMs : Nat := 60000  -- 1 minute
  deriving Repr, Inhabited

namespace Config

/-- Default config file name -/
def configFileName : String := ".enchiridion.json"

/-- Parse config from JSON -/
def fromJson? (json : Json) : Option Config := do
  let apiKey := (json.getObjValAs? String "openRouterApiKey").toOption.getD ""
  let model := (json.getObjValAs? String "defaultModel").toOption.getD "anthropic/claude-3.5-sonnet"
  let autoSave := (json.getObjValAs? Bool "autoSaveEnabled").toOption.getD false
  let autoSaveInterval := (json.getObjValAs? Nat "autoSaveIntervalMs").toOption.getD 60000
  some {
    openRouterApiKey := apiKey
    defaultModel := model
    autoSaveEnabled := autoSave
    autoSaveIntervalMs := autoSaveInterval
  }

/-- Convert config to JSON -/
def toJson (config : Config) : Json :=
  Json.mkObj [
    ("openRouterApiKey", Json.str config.openRouterApiKey),
    ("defaultModel", Json.str config.defaultModel),
    ("autoSaveEnabled", Json.bool config.autoSaveEnabled),
    ("autoSaveIntervalMs", Json.num config.autoSaveIntervalMs)
  ]

/-- Try to load config from a file -/
def loadFromFile (path : String) : IO (Option Config) := do
  try
    let content ← IO.FS.readFile path
    match Json.parse content with
    | .ok json => return fromJson? json
    | .error _ => return none
  catch _ =>
    return none

/-- Try to find and load config file from standard locations -/
def findAndLoad : IO Config := do
  -- Try current directory first
  if let some config ← loadFromFile configFileName then
    return config

  -- Try home directory
  if let some home ← IO.getEnv "HOME" then
    if let some config ← loadFromFile s!"{home}/{configFileName}" then
      return config

  -- Try XDG config directory
  if let some xdgConfig ← IO.getEnv "XDG_CONFIG_HOME" then
    if let some config ← loadFromFile s!"{xdgConfig}/enchiridion/config.json" then
      return config

  -- Fall back to defaults with environment variable for API key
  let apiKey ← IO.getEnv "OPENROUTER_API_KEY"
  return { openRouterApiKey := apiKey.getD "" }

/-- Save config to a file -/
def saveToFile (config : Config) (path : String) : IO (Except String Unit) := do
  try
    let json := config.toJson
    IO.FS.writeFile path json.pretty
    return .ok ()
  catch e =>
    return .error s!"Failed to save config: {e}"

/-- Create a sample config file -/
def createSampleConfig : IO Unit := do
  let sample : Config := {
    openRouterApiKey := "your-api-key-here"
    defaultModel := "anthropic/claude-3.5-sonnet"
    autoSaveEnabled := false
    autoSaveIntervalMs := 60000
  }
  let _ ← saveToFile sample configFileName
  IO.println s!"Created sample config file: {configFileName}"
  IO.println "Please edit it with your OpenRouter API key."

end Config

end Enchiridion
