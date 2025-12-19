/-
  Enchiridion Novel Model
  Core data structures for novels, chapters, and scenes
-/

import Lean.Data.Json
import Enchiridion.Core.Types
import Enchiridion.Core.Json

namespace Enchiridion

open Lean Json

/-- A single scene within a chapter -/
structure Scene where
  id : EntityId
  title : String
  content : String := ""
  synopsis : String := ""
  notes : String := ""
  wordCount : Nat := 0
  createdAt : Timestamp := Timestamp.zero
  modifiedAt : Timestamp := Timestamp.zero
  deriving Repr, Inhabited

namespace Scene

/-- Create a new scene with generated ID -/
def create (title : String) : IO Scene := do
  let id ← EntityId.generate
  let now ← Timestamp.now
  return {
    id := id
    title := title
    createdAt := now
    modifiedAt := now
  }

/-- Update word count based on content -/
def updateWordCount (scene : Scene) : Scene :=
  let words := scene.content.splitOn " " |>.filter (·.length > 0) |>.length
  { scene with wordCount := words }

/-- Check if scene has content -/
def isEmpty (scene : Scene) : Bool :=
  scene.content.trim.isEmpty

end Scene

-- Scene JSON instances
instance : ToJson Scene where
  toJson s := Json.mkObj [
    ("id", toJson s.id),
    ("title", Json.str s.title),
    ("content", Json.str s.content),
    ("synopsis", Json.str s.synopsis),
    ("notes", Json.str s.notes),
    ("wordCount", Json.num s.wordCount),
    ("createdAt", toJson s.createdAt),
    ("modifiedAt", toJson s.modifiedAt)
  ]

instance : FromJson Scene where
  fromJson? json := do
    let id ← (json.getObjVal? "id") >>= fromJson?
    let title ← json.getObjValAs? String "title"
    let content ← json.getObjValAs? String "content" <|> pure ""
    let synopsis ← json.getObjValAs? String "synopsis" <|> pure ""
    let notes ← json.getObjValAs? String "notes" <|> pure ""
    let wordCount ← json.getObjValAs? Nat "wordCount" <|> pure 0
    let createdAt ← (json.getObjVal? "createdAt") >>= fromJson? <|> pure Timestamp.zero
    let modifiedAt ← (json.getObjVal? "modifiedAt") >>= fromJson? <|> pure Timestamp.zero
    return {
      id := id
      title := title
      content := content
      synopsis := synopsis
      notes := notes
      wordCount := wordCount
      createdAt := createdAt
      modifiedAt := modifiedAt
    }

/-- A chapter containing multiple scenes -/
structure Chapter where
  id : EntityId
  title : String
  scenes : Array Scene := #[]
  synopsis : String := ""
  notes : String := ""
  collapsed : Bool := false
  createdAt : Timestamp := Timestamp.zero
  modifiedAt : Timestamp := Timestamp.zero
  deriving Repr, Inhabited

namespace Chapter

/-- Create a new chapter with generated ID -/
def create (title : String) : IO Chapter := do
  let id ← EntityId.generate
  let now ← Timestamp.now
  return {
    id := id
    title := title
    createdAt := now
    modifiedAt := now
  }

/-- Add a scene to the chapter -/
def addScene (chapter : Chapter) (scene : Scene) : Chapter :=
  { chapter with scenes := chapter.scenes.push scene }

/-- Get scene by ID -/
def getScene (chapter : Chapter) (sceneId : EntityId) : Option Scene :=
  chapter.scenes.find? (·.id == sceneId)

/-- Update a scene in the chapter -/
def updateScene (chapter : Chapter) (sceneId : EntityId) (f : Scene → Scene) : Chapter :=
  let scenes := chapter.scenes.map fun s =>
    if s.id == sceneId then f s else s
  { chapter with scenes := scenes }

/-- Total word count of all scenes -/
def totalWordCount (chapter : Chapter) : Nat :=
  chapter.scenes.foldl (fun acc s => acc + s.wordCount) 0

/-- Number of scenes -/
def sceneCount (chapter : Chapter) : Nat :=
  chapter.scenes.size

end Chapter

-- Chapter JSON instances
instance : ToJson Chapter where
  toJson c := Json.mkObj [
    ("id", toJson c.id),
    ("title", Json.str c.title),
    ("scenes", toJson c.scenes),
    ("synopsis", Json.str c.synopsis),
    ("notes", Json.str c.notes),
    ("collapsed", Json.bool c.collapsed),
    ("createdAt", toJson c.createdAt),
    ("modifiedAt", toJson c.modifiedAt)
  ]

instance : FromJson Chapter where
  fromJson? json := do
    let id ← (json.getObjVal? "id") >>= fromJson?
    let title ← json.getObjValAs? String "title"
    let scenesJson ← json.getObjVal? "scenes" <|> pure (Json.arr #[])
    let scenes ← fromJson? scenesJson <|> pure #[]
    let synopsis ← json.getObjValAs? String "synopsis" <|> pure ""
    let notes ← json.getObjValAs? String "notes" <|> pure ""
    let collapsed ← json.getObjValAs? Bool "collapsed" <|> pure false
    let createdAt ← (json.getObjVal? "createdAt") >>= fromJson? <|> pure Timestamp.zero
    let modifiedAt ← (json.getObjVal? "modifiedAt") >>= fromJson? <|> pure Timestamp.zero
    return {
      id := id
      title := title
      scenes := scenes
      synopsis := synopsis
      notes := notes
      collapsed := collapsed
      createdAt := createdAt
      modifiedAt := modifiedAt
    }

/-- The novel itself -/
structure Novel where
  id : EntityId
  title : String
  author : String := ""
  genre : String := ""
  synopsis : String := ""
  chapters : Array Chapter := #[]
  createdAt : Timestamp := Timestamp.zero
  modifiedAt : Timestamp := Timestamp.zero
  deriving Repr, Inhabited

namespace Novel

/-- Create a new novel with generated ID -/
def create (title : String) (author : String := "") : IO Novel := do
  let id ← EntityId.generate
  let now ← Timestamp.now
  return {
    id := id
    title := title
    author := author
    createdAt := now
    modifiedAt := now
  }

/-- Create a default empty novel -/
def empty : IO Novel := create "Untitled Novel"

/-- Add a chapter to the novel -/
def addChapter (novel : Novel) (chapter : Chapter) : Novel :=
  { novel with chapters := novel.chapters.push chapter }

/-- Get chapter by ID -/
def getChapter (novel : Novel) (chapterId : EntityId) : Option Chapter :=
  novel.chapters.find? (·.id == chapterId)

/-- Update a chapter in the novel -/
def updateChapter (novel : Novel) (chapterId : EntityId) (f : Chapter → Chapter) : Novel :=
  let chapters := novel.chapters.map fun c =>
    if c.id == chapterId then f c else c
  { novel with chapters := chapters }

/-- Get a scene by chapter and scene ID -/
def getScene (novel : Novel) (chapterId : EntityId) (sceneId : EntityId) : Option Scene := do
  let chapter ← novel.getChapter chapterId
  chapter.getScene sceneId

/-- Update a scene in the novel -/
def updateScene (novel : Novel) (chapterId : EntityId) (sceneId : EntityId) (f : Scene → Scene) : Novel :=
  novel.updateChapter chapterId (·.updateScene sceneId f)

/-- Total word count of the entire novel -/
def totalWordCount (novel : Novel) : Nat :=
  novel.chapters.foldl (fun acc c => acc + c.totalWordCount) 0

/-- Total number of chapters -/
def chapterCount (novel : Novel) : Nat :=
  novel.chapters.size

/-- Total number of scenes -/
def sceneCount (novel : Novel) : Nat :=
  novel.chapters.foldl (fun acc c => acc + c.sceneCount) 0

end Novel

-- Novel JSON instances
instance : ToJson Novel where
  toJson n := Json.mkObj [
    ("id", toJson n.id),
    ("title", Json.str n.title),
    ("author", Json.str n.author),
    ("genre", Json.str n.genre),
    ("synopsis", Json.str n.synopsis),
    ("chapters", toJson n.chapters),
    ("createdAt", toJson n.createdAt),
    ("modifiedAt", toJson n.modifiedAt)
  ]

instance : FromJson Novel where
  fromJson? json := do
    let id ← (json.getObjVal? "id") >>= fromJson?
    let title ← json.getObjValAs? String "title"
    let author ← json.getObjValAs? String "author" <|> pure ""
    let genre ← json.getObjValAs? String "genre" <|> pure ""
    let synopsis ← json.getObjValAs? String "synopsis" <|> pure ""
    let chaptersJson ← json.getObjVal? "chapters" <|> pure (Json.arr #[])
    let chapters ← fromJson? chaptersJson <|> pure #[]
    let createdAt ← (json.getObjVal? "createdAt") >>= fromJson? <|> pure Timestamp.zero
    let modifiedAt ← (json.getObjVal? "modifiedAt") >>= fromJson? <|> pure Timestamp.zero
    return {
      id := id
      title := title
      author := author
      genre := genre
      synopsis := synopsis
      chapters := chapters
      createdAt := createdAt
      modifiedAt := modifiedAt
    }

end Enchiridion
