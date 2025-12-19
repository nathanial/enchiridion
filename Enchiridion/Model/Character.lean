/-
  Enchiridion Character Model
  Data structures for character notes and tracking
-/

import Lean.Data.Json
import Enchiridion.Core.Types
import Enchiridion.Core.Json

namespace Enchiridion

open Lean Json

/-- Character profile for tracking -/
structure Character where
  id : EntityId
  name : String
  aliases : Array String := #[]
  description : String := ""
  notes : String := ""
  traits : Array String := #[]
  createdAt : Timestamp := Timestamp.zero
  modifiedAt : Timestamp := Timestamp.zero
  deriving Repr, Inhabited

namespace Character

/-- Create a new character with generated ID -/
def create (name : String) : IO Character := do
  let id ← EntityId.generate
  let now ← Timestamp.now
  return {
    id := id
    name := name
    createdAt := now
    modifiedAt := now
  }

/-- Add an alias to the character -/
def addAlias (char : Character) (alias : String) : Character :=
  { char with aliases := char.aliases.push alias }

/-- Add a trait to the character -/
def addTrait (char : Character) (trait : String) : Character :=
  { char with traits := char.traits.push trait }

/-- Check if character has a given alias -/
def hasAlias (char : Character) (alias : String) : Bool :=
  char.aliases.contains alias

/-- Get display name (name with first alias if any) -/
def displayName (char : Character) : String :=
  if char.aliases.isEmpty then
    char.name
  else
    s!"{char.name} ({char.aliases[0]!})"

end Character

-- Character JSON instances
instance : ToJson Character where
  toJson c := Json.mkObj [
    ("id", toJson c.id),
    ("name", Json.str c.name),
    ("aliases", Json.arr (c.aliases.map Json.str)),
    ("description", Json.str c.description),
    ("notes", Json.str c.notes),
    ("traits", Json.arr (c.traits.map Json.str)),
    ("createdAt", toJson c.createdAt),
    ("modifiedAt", toJson c.modifiedAt)
  ]

private def parseStringArray (json : Json) : Array String :=
  if let some jsonArr := json.getArr?.toOption then
    jsonArr.filterMap (fun j => j.getStr?.toOption)
  else
    #[]

instance : FromJson Character where
  fromJson? json := do
    let id ← (json.getObjVal? "id") >>= fromJson?
    let name ← json.getObjValAs? String "name"
    let aliasesJson := (json.getObjVal? "aliases").toOption.getD (Json.arr #[])
    let aliases := parseStringArray aliasesJson
    let description ← json.getObjValAs? String "description" <|> pure ""
    let notes ← json.getObjValAs? String "notes" <|> pure ""
    let traitsJson := (json.getObjVal? "traits").toOption.getD (Json.arr #[])
    let traits := parseStringArray traitsJson
    let createdAt ← (json.getObjVal? "createdAt") >>= fromJson? <|> pure Timestamp.zero
    let modifiedAt ← (json.getObjVal? "modifiedAt") >>= fromJson? <|> pure Timestamp.zero
    return {
      id := id
      name := name
      aliases := aliases
      description := description
      notes := notes
      traits := traits
      createdAt := createdAt
      modifiedAt := modifiedAt
    }

end Enchiridion
