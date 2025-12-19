/-
  Enchiridion Character Model
  Data structures for character notes and tracking
-/

import Enchiridion.Core.Types

namespace Enchiridion

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

end Enchiridion
