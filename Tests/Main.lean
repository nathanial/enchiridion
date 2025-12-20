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

  -- Test Character
  IO.println "\n6. Character CRUD"
  let char ← Enchiridion.Character.create "Sarah"
  let char := { char with description := "The protagonist" }
  IO.println s!"  Created character: {char.name}"
  IO.println s!"  Description: {char.description}"
  if char.name == "Sarah" && char.description == "The protagonist" then
    IO.println "  ✓ Character creation works"
  else
    IO.println "  ✗ Character creation failed"

  -- Test WorldNote
  IO.println "\n7. WorldNote CRUD"
  let note ← Enchiridion.WorldNote.create "The Old Kingdom"
  let note := { note with content := "An ancient land of mystery", category := .location }
  IO.println s!"  Created note: {note.title}"
  IO.println s!"  Category: {note.category}"
  IO.println s!"  Content: {note.content}"
  if note.title == "The Old Kingdom" && note.category == .location then
    IO.println "  ✓ WorldNote creation works"
  else
    IO.println "  ✗ WorldNote creation failed"

  -- Test AppState character operations
  IO.println "\n8. AppState Character Operations"
  let mut state := Enchiridion.AppState.fromProject project

  -- Add character
  let char1 ← Enchiridion.Character.create "Character 1"
  state := state.addNewCharacter char1
  if state.project.characters.size == 1 then
    IO.println "  ✓ addNewCharacter works"
  else
    IO.println "  ✗ addNewCharacter failed"

  -- Add another character
  let char2 ← Enchiridion.Character.create "Character 2"
  state := state.addNewCharacter char2
  if state.project.characters.size == 2 && state.selectedCharacterIdx == 1 then
    IO.println "  ✓ Adding second character updates selection"
  else
    IO.println "  ✗ Selection not updated correctly"

  -- Edit character (enter edit mode)
  state := { state with selectedCharacterIdx := 0 }
  state := state.editSelectedCharacter
  if state.notesEditMode && state.notesNameInput.value == "Character 1" then
    IO.println "  ✓ editSelectedCharacter enters edit mode"
  else
    IO.println "  ✗ editSelectedCharacter failed"

  -- Modify and save
  state := { state with
    notesNameInput := state.notesNameInput.withValue "Updated Name"
    notesContentArea := Terminus.TextArea.fromString "New description" }
  state := state.saveCharacterEdits
  if !state.notesEditMode && state.project.characters[0]!.name == "Updated Name" then
    IO.println "  ✓ saveCharacterEdits works"
  else
    IO.println "  ✗ saveCharacterEdits failed"

  -- Delete character
  state := { state with selectedCharacterIdx := 1 }
  state := state.deleteSelectedCharacter
  if state.project.characters.size == 1 && state.selectedCharacterIdx == 0 then
    IO.println "  ✓ deleteSelectedCharacter works"
  else
    IO.println "  ✗ deleteSelectedCharacter failed"

  -- Test AppState world note operations
  IO.println "\n9. AppState WorldNote Operations"

  -- Add world note
  let note1 ← Enchiridion.WorldNote.create "Note 1"
  state := state.addNewWorldNote note1
  if state.project.worldNotes.size == 1 then
    IO.println "  ✓ addNewWorldNote works"
  else
    IO.println "  ✗ addNewWorldNote failed"

  -- Add another note
  let note2 ← Enchiridion.WorldNote.create "Note 2"
  state := state.addNewWorldNote note2
  if state.project.worldNotes.size == 2 && state.selectedNoteIdx == 1 then
    IO.println "  ✓ Adding second note updates selection"
  else
    IO.println "  ✗ Selection not updated correctly"

  -- Edit world note
  state := { state with selectedNoteIdx := 0, notesTab := 1 }
  state := state.editSelectedWorldNote
  if state.notesEditMode && state.notesNameInput.value == "Note 1" then
    IO.println "  ✓ editSelectedWorldNote enters edit mode"
  else
    IO.println "  ✗ editSelectedWorldNote failed"

  -- Modify and save
  state := { state with
    notesNameInput := state.notesNameInput.withValue "Updated Note"
    notesContentArea := Terminus.TextArea.fromString "New content" }
  state := state.saveWorldNoteEdits
  if !state.notesEditMode && state.project.worldNotes[0]!.title == "Updated Note" then
    IO.println "  ✓ saveWorldNoteEdits works"
  else
    IO.println "  ✗ saveWorldNoteEdits failed"

  -- Delete world note
  state := { state with selectedNoteIdx := 1 }
  state := state.deleteSelectedWorldNote
  if state.project.worldNotes.size == 1 && state.selectedNoteIdx == 0 then
    IO.println "  ✓ deleteSelectedWorldNote works"
  else
    IO.println "  ✗ deleteSelectedWorldNote failed"

  -- Test pending action flags
  IO.println "\n10. Pending Action Flags"
  state := state.requestNewCharacter
  if state.pendingNewCharacter then
    IO.println "  ✓ requestNewCharacter sets flag"
  else
    IO.println "  ✗ requestNewCharacter failed"

  state := state.requestNewWorldNote
  if state.pendingNewWorldNote then
    IO.println "  ✓ requestNewWorldNote sets flag"
  else
    IO.println "  ✗ requestNewWorldNote failed"

  if state.hasPendingActions then
    IO.println "  ✓ hasPendingActions detects flags"
  else
    IO.println "  ✗ hasPendingActions failed"

  state := state.clearPendingActions
  if !state.hasPendingActions then
    IO.println "  ✓ clearPendingActions works"
  else
    IO.println "  ✗ clearPendingActions failed"

  IO.println "\n================="
  IO.println "All tests completed!"
