/-
  Enchiridion WorldNote Model
  Data structures for world-building notes
-/

import Lean.Data.Json
import Enchiridion.Core.Types
import Enchiridion.Core.Json

namespace Enchiridion

open Lean Json

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

def fromString (s : String) : NoteCategory :=
  match s with
  | "location" => .location
  | "item" => .item
  | "lore" => .lore
  | "timeline" => .timeline
  | _ => .other

def toJsonString : NoteCategory → String
  | .location => "location"
  | .item => "item"
  | .lore => "lore"
  | .timeline => "timeline"
  | .other => "other"

end NoteCategory

-- NoteCategory JSON instances
instance : ToJson NoteCategory where
  toJson cat := Json.str cat.toJsonString

instance : FromJson NoteCategory where
  fromJson? json := do
    let s ← json.getStr?
    return NoteCategory.fromString s

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

-- WorldNote JSON instances
instance : ToJson WorldNote where
  toJson n := Json.mkObj [
    ("id", toJson n.id),
    ("title", Json.str n.title),
    ("category", toJson n.category),
    ("content", Json.str n.content),
    ("tags", Json.arr (n.tags.map Json.str)),
    ("createdAt", toJson n.createdAt),
    ("modifiedAt", toJson n.modifiedAt)
  ]

private def parseStringArray (json : Json) : Array String :=
  if let some jsonArr := json.getArr?.toOption then
    jsonArr.filterMap (fun j => j.getStr?.toOption)
  else
    #[]

instance : FromJson WorldNote where
  fromJson? json := do
    let id ← (json.getObjVal? "id") >>= fromJson?
    let title ← json.getObjValAs? String "title"
    let category ← (json.getObjVal? "category") >>= fromJson? <|> pure NoteCategory.other
    let content ← json.getObjValAs? String "content" <|> pure ""
    let tagsJson := (json.getObjVal? "tags").toOption.getD (Json.arr #[])
    let tags := parseStringArray tagsJson
    let createdAt ← (json.getObjVal? "createdAt") >>= fromJson? <|> pure Timestamp.zero
    let modifiedAt ← (json.getObjVal? "modifiedAt") >>= fromJson? <|> pure Timestamp.zero
    return {
      id := id
      title := title
      category := category
      content := content
      tags := tags
      createdAt := createdAt
      modifiedAt := modifiedAt
    }

end Enchiridion
