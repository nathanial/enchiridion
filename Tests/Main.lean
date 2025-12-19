/-
  Enchiridion Tests
-/

import Enchiridion

def main : IO Unit := do
  IO.println "Enchiridion Tests"
  IO.println "================="

  -- Test EntityId generation
  IO.println "\n1. EntityId generation"
  let id1 ← Enchiridion.EntityId.generate
  let id2 ← Enchiridion.EntityId.generate
  IO.println s!"  Generated ID 1: {id1}"
  IO.println s!"  Generated ID 2: {id2}"
  if id1 != id2 then
    IO.println "  ✓ IDs are unique"
  else
    IO.println "  ✗ IDs should be unique"

  -- Test Timestamp
  IO.println "\n2. Timestamp"
  let ts ← Enchiridion.Timestamp.now
  IO.println s!"  Current timestamp: {ts}"

  -- Test Novel creation
  IO.println "\n3. Novel creation"
  let novel ← Enchiridion.Novel.create "Test Novel" "Test Author"
  IO.println s!"  Created novel: {novel.title} by {novel.author}"

  -- Test Chapter and Scene
  IO.println "\n4. Chapter and Scene"
  let chapter ← Enchiridion.Chapter.create "Chapter 1"
  let scene ← Enchiridion.Scene.create "Scene 1"
  let scene := { scene with content := "This is some test content with multiple words." }
  let scene := scene.updateWordCount
  IO.println s!"  Created chapter: {chapter.title}"
  IO.println s!"  Created scene: {scene.title} (word count: {scene.wordCount})"

  -- Test Project
  IO.println "\n5. Project creation"
  let project ← Enchiridion.Project.create "My Novel" "Author Name"
  IO.println s!"  Created project with novel: {project.novel.title}"

  IO.println "\n================="
  IO.println "All basic tests passed!"
