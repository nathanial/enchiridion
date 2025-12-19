/-
  Enchiridion File I/O
  Save and load projects to/from JSON files
-/

import Lean.Data.Json
import Enchiridion.Model.Project

namespace Enchiridion.Storage

open Lean Json

/-- File extension for Enchiridion projects -/
def projectExtension : String := ".enchiridion"

/-- Result type for file operations -/
inductive FileResult (α : Type)
  | ok : α → FileResult α
  | error : String → FileResult α
  deriving Inhabited

namespace FileResult

def isOk : FileResult α → Bool
  | .ok _ => true
  | .error _ => false

def getD (r : FileResult α) (default : α) : α :=
  match r with
  | .ok a => a
  | .error _ => default

def map (f : α → β) : FileResult α → FileResult β
  | .ok a => .ok (f a)
  | .error e => .error e

end FileResult

/-- Save a project to a JSON file -/
def saveProject (project : Project) (path : String) : IO (FileResult Unit) := do
  try
    let json := toJson project
    let content := json.pretty
    IO.FS.writeFile path content
    return .ok ()
  catch e =>
    return .error s!"Failed to save project: {e}"

/-- Load a project from a JSON file -/
def loadProject (path : String) : IO (FileResult Project) := do
  try
    let content ← IO.FS.readFile path
    match Json.parse content with
    | .ok json =>
      match fromJson? json with
      | .ok (project : Project) =>
        -- Set the file path on the loaded project
        return .ok { project with filePath := some path }
      | .error e =>
        return .error s!"Failed to parse project data: {e}"
    | .error e =>
      return .error s!"Invalid JSON: {e}"
  catch e =>
    return .error s!"Failed to read file: {e}"

/-- Check if a file exists -/
def fileExists (path : String) : IO Bool := do
  try
    let _ ← IO.FS.readFile path
    return true
  catch _ =>
    return false

/-- Get a default save path for a project -/
def defaultSavePath (project : Project) : String :=
  let sanitizedTitle := project.novel.title.replace " " "_"
  s!"{sanitizedTitle}{projectExtension}"

/-- Ensure path has correct extension -/
def ensureExtension (path : String) : String :=
  if path.endsWith projectExtension then path
  else s!"{path}{projectExtension}"

end Enchiridion.Storage
