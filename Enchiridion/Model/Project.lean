/-
  Enchiridion Project Model
  Container for novel and project metadata
-/

import Lean.Data.Json
import Enchiridion.Core.Types
import Enchiridion.Core.Json
import Enchiridion.Model.Novel
import Enchiridion.Model.Character
import Enchiridion.Model.WorldNote

namespace Enchiridion

open Lean Json

/-- Project metadata -/
structure ProjectMeta where
  version : String := "1.0"
  lastOpenedChapterId : Option EntityId := none
  lastOpenedSceneId : Option EntityId := none
  deriving Repr, Inhabited

/-- Complete project containing novel and all notes -/
structure Project where
  novel : Novel
  characters : Array Character := #[]
  worldNotes : Array WorldNote := #[]
  metadata : ProjectMeta := {}
  filePath : Option String := none
  isDirty : Bool := false
  deriving Repr, Inhabited

namespace Project

/-- Create a new empty project -/
def create (title : String) (author : String := "") : IO Project := do
  let novel ← Novel.create title author
  return {
    novel := novel
    characters := #[]
    worldNotes := #[]
  }

/-- Create a default empty project -/
def empty : IO Project := create "Untitled Novel"

/-- Mark project as modified -/
def markDirty (project : Project) : Project :=
  { project with isDirty := true }

/-- Mark project as saved -/
def markClean (project : Project) : Project :=
  { project with isDirty := false }

/-- Set file path -/
def setFilePath (project : Project) (path : String) : Project :=
  { project with filePath := some path }

/-- Add a character -/
def addCharacter (project : Project) (char : Character) : Project :=
  { project with characters := project.characters.push char, isDirty := true }

/-- Get character by ID -/
def getCharacter (project : Project) (charId : EntityId) : Option Character :=
  project.characters.find? (·.id == charId)

/-- Update a character -/
def updateCharacter (project : Project) (charId : EntityId) (f : Character → Character) : Project :=
  let chars := project.characters.map fun c =>
    if c.id == charId then f c else c
  { project with characters := chars, isDirty := true }

/-- Add a world note -/
def addWorldNote (project : Project) (note : WorldNote) : Project :=
  { project with worldNotes := project.worldNotes.push note, isDirty := true }

/-- Get world note by ID -/
def getWorldNote (project : Project) (noteId : EntityId) : Option WorldNote :=
  project.worldNotes.find? (·.id == noteId)

/-- Update a world note -/
def updateWorldNote (project : Project) (noteId : EntityId) (f : WorldNote → WorldNote) : Project :=
  let notes := project.worldNotes.map fun n =>
    if n.id == noteId then f n else n
  { project with worldNotes := notes, isDirty := true }

/-- Update the novel -/
def updateNovel (project : Project) (f : Novel → Novel) : Project :=
  { project with novel := f project.novel, isDirty := true }

/-- Get world notes by category -/
def getWorldNotesByCategory (project : Project) (cat : NoteCategory) : Array WorldNote :=
  project.worldNotes.filter (·.category == cat)

/-- Total word count -/
def totalWordCount (project : Project) : Nat :=
  project.novel.totalWordCount

/-- Character count -/
def characterCount (project : Project) : Nat :=
  project.characters.size

/-- World note count -/
def worldNoteCount (project : Project) : Nat :=
  project.worldNotes.size

end Project

-- ProjectMeta JSON instances
instance : ToJson ProjectMeta where
  toJson m := Json.mkObj [
    ("version", Json.str m.version),
    ("lastOpenedChapterId", match m.lastOpenedChapterId with
      | some id => toJson id
      | none => Json.null),
    ("lastOpenedSceneId", match m.lastOpenedSceneId with
      | some id => toJson id
      | none => Json.null)
  ]

instance : FromJson ProjectMeta where
  fromJson? json := do
    let version ← json.getObjValAs? String "version" <|> pure "1.0"
    let lastChapterId ← match json.getObjVal? "lastOpenedChapterId" with
      | .ok j => if j.isNull then pure none else some <$> fromJson? j
      | .error _ => pure none
    let lastSceneId ← match json.getObjVal? "lastOpenedSceneId" with
      | .ok j => if j.isNull then pure none else some <$> fromJson? j
      | .error _ => pure none
    return {
      version := version
      lastOpenedChapterId := lastChapterId
      lastOpenedSceneId := lastSceneId
    }

-- Project JSON instances
instance : ToJson Project where
  toJson p := Json.mkObj [
    ("version", Json.str p.metadata.version),
    ("novel", toJson p.novel),
    ("characters", toJson p.characters),
    ("worldNotes", toJson p.worldNotes),
    ("metadata", toJson p.metadata)
  ]

instance : FromJson Project where
  fromJson? json := do
    let novel ← (json.getObjVal? "novel") >>= fromJson?
    let charsJson ← json.getObjVal? "characters" <|> pure (Json.arr #[])
    let characters ← fromJson? charsJson <|> pure #[]
    let notesJson ← json.getObjVal? "worldNotes" <|> pure (Json.arr #[])
    let worldNotes ← fromJson? notesJson <|> pure #[]
    let metadata ← (json.getObjVal? "metadata") >>= fromJson? <|> pure {}
    return {
      novel := novel
      characters := characters
      worldNotes := worldNotes
      metadata := metadata
      filePath := none
      isDirty := false
    }

end Enchiridion
