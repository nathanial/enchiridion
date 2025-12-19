/-
  Enchiridion WorldNote Model
  Data structures for world-building notes
-/

import Enchiridion.Core.Types

namespace Enchiridion

/-- Categories for world-building notes -/
inductive NoteCategory where
  | location
  | item
  | lore
  | timeline
  | other
  deriving Repr, BEq, Inhabited, DecidableEq

namespace NoteCategory

def toString : NoteCategory → String
  | .location => "Location"
  | .item => "Item"
  | .lore => "Lore"
  | .timeline => "Timeline"
  | .other => "Other"

instance : ToString NoteCategory where
  toString := NoteCategory.toString

def all : Array NoteCategory := #[.location, .item, .lore, .timeline, .other]

end NoteCategory

/-- World-building note -/
structure WorldNote where
  id : EntityId
  title : String
  category : NoteCategory := .other
  content : String := ""
  tags : Array String := #[]
  createdAt : Timestamp := Timestamp.zero
  modifiedAt : Timestamp := Timestamp.zero
  deriving Repr, Inhabited

namespace WorldNote

/-- Create a new world note with generated ID -/
def create (title : String) (category : NoteCategory := .other) : IO WorldNote := do
  let id ← EntityId.generate
  let now ← Timestamp.now
  return {
    id := id
    title := title
    category := category
    createdAt := now
    modifiedAt := now
  }

/-- Add a tag to the note -/
def addTag (note : WorldNote) (tag : String) : WorldNote :=
  { note with tags := note.tags.push tag }

/-- Check if note has a given tag -/
def hasTag (note : WorldNote) (tag : String) : Bool :=
  note.tags.contains tag

/-- Get display title with category -/
def displayTitle (note : WorldNote) : String :=
  s!"[{note.category}] {note.title}"

end WorldNote

end Enchiridion
