/-
  Enchiridion Tests
-/

import Crucible
import Enchiridion
import Staple

namespace Enchiridion.Tests

open Crucible
open Staple (String.containsSubstr)

testSuite "Enchiridion Tests"

/-! ## EntityId Tests -/

test "EntityId generation produces unique IDs" := do
  let id1 ← Enchiridion.EntityId.generate
  let id2 ← Enchiridion.EntityId.generate
  ensure (id1 != id2) "IDs should be unique"

/-! ## Timestamp Tests -/

test "Timestamp.now returns valid timestamp" := do
  let ts ← Enchiridion.Timestamp.now
  ensure (ts.unixMs > 0) "Timestamp should be positive"

/-! ## Novel Tests -/

test "Novel creation" := do
  let novel ← Enchiridion.Novel.create "Test Novel" "Test Author"
  novel.title ≡ "Test Novel"
  novel.author ≡ "Test Author"

/-! ## Chapter and Scene Tests -/

test "Chapter and Scene creation with word count" := do
  let chapter ← Enchiridion.Chapter.create "Chapter 1"
  let scene ← Enchiridion.Scene.create "Scene 1"
  let scene := { scene with content := "This is some test content with multiple words." }
  let scene := scene.updateWordCount
  chapter.title ≡ "Chapter 1"
  scene.title ≡ "Scene 1"
  ensure (scene.wordCount > 0) "Word count should be positive"

/-! ## Project Tests -/

test "Project creation" := do
  let project ← Enchiridion.Project.create "My Novel" "Author Name"
  project.novel.title ≡ "My Novel"

/-! ## Character Tests -/

test "Character CRUD" := do
  let char ← Enchiridion.Character.create "Sarah"
  let char := { char with description := "The protagonist" }
  char.name ≡ "Sarah"
  char.description ≡ "The protagonist"

/-! ## WorldNote Tests -/

test "WorldNote CRUD" := do
  let note ← Enchiridion.WorldNote.create "The Old Kingdom"
  let note := { note with content := "An ancient land of mystery", category := .location }
  note.title ≡ "The Old Kingdom"
  note.category ≡ Enchiridion.NoteCategory.location
  note.content ≡ "An ancient land of mystery"

/-! ## AppState Character Operations -/

test "addNewCharacter works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let char1 ← Enchiridion.Character.create "Character 1"
  state := state.addNewCharacter char1
  state.project.characters.size ≡ 1

test "Adding second character updates selection" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let char1 ← Enchiridion.Character.create "Character 1"
  state := state.addNewCharacter char1
  let char2 ← Enchiridion.Character.create "Character 2"
  state := state.addNewCharacter char2
  state.project.characters.size ≡ 2
  state.selectedCharacterIdx ≡ 1

test "editSelectedCharacter enters edit mode" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let char1 ← Enchiridion.Character.create "Character 1"
  state := state.addNewCharacter char1
  state := { state with selectedCharacterIdx := 0 }
  state := state.editSelectedCharacter
  ensure state.notesEditMode "Should be in edit mode"
  state.notesNameInput.value ≡ "Character 1"

test "saveCharacterEdits works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let char1 ← Enchiridion.Character.create "Character 1"
  state := state.addNewCharacter char1
  state := { state with selectedCharacterIdx := 0 }
  state := state.editSelectedCharacter
  state := { state with
    notesNameInput := state.notesNameInput.withValue "Updated Name"
    notesContentArea := Terminus.TextArea.fromString "New description" }
  state := state.saveCharacterEdits
  ensure (!state.notesEditMode) "Should exit edit mode"
  state.project.characters[0]!.name ≡ "Updated Name"

test "deleteSelectedCharacter works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let char1 ← Enchiridion.Character.create "Character 1"
  state := state.addNewCharacter char1
  let char2 ← Enchiridion.Character.create "Character 2"
  state := state.addNewCharacter char2
  state := { state with selectedCharacterIdx := 1 }
  state := state.deleteSelectedCharacter
  state.project.characters.size ≡ 1
  state.selectedCharacterIdx ≡ 0

/-! ## AppState WorldNote Operations -/

test "addNewWorldNote works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let note1 ← Enchiridion.WorldNote.create "Note 1"
  state := state.addNewWorldNote note1
  state.project.worldNotes.size ≡ 1

test "Adding second note updates selection" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let note1 ← Enchiridion.WorldNote.create "Note 1"
  state := state.addNewWorldNote note1
  let note2 ← Enchiridion.WorldNote.create "Note 2"
  state := state.addNewWorldNote note2
  state.project.worldNotes.size ≡ 2
  state.selectedNoteIdx ≡ 1

test "editSelectedWorldNote enters edit mode" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let note1 ← Enchiridion.WorldNote.create "Note 1"
  state := state.addNewWorldNote note1
  state := { state with selectedNoteIdx := 0, notesTab := 1 }
  state := state.editSelectedWorldNote
  ensure state.notesEditMode "Should be in edit mode"
  state.notesNameInput.value ≡ "Note 1"

test "saveWorldNoteEdits works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let note1 ← Enchiridion.WorldNote.create "Note 1"
  state := state.addNewWorldNote note1
  state := { state with selectedNoteIdx := 0, notesTab := 1 }
  state := state.editSelectedWorldNote
  state := { state with
    notesNameInput := state.notesNameInput.withValue "Updated Note"
    notesContentArea := Terminus.TextArea.fromString "New content" }
  state := state.saveWorldNoteEdits
  ensure (!state.notesEditMode) "Should exit edit mode"
  state.project.worldNotes[0]!.title ≡ "Updated Note"

test "deleteSelectedWorldNote works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let note1 ← Enchiridion.WorldNote.create "Note 1"
  state := state.addNewWorldNote note1
  let note2 ← Enchiridion.WorldNote.create "Note 2"
  state := state.addNewWorldNote note2
  state := { state with selectedNoteIdx := 1 }
  state := state.deleteSelectedWorldNote
  state.project.worldNotes.size ≡ 1
  state.selectedNoteIdx ≡ 0

/-! ## Pending Action Flags -/

test "requestNewCharacter sets flag" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestNewCharacter
  ensure state.pendingNewCharacter "Flag should be set"

test "requestNewWorldNote sets flag" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestNewWorldNote
  ensure state.pendingNewWorldNote "Flag should be set"

test "hasPendingActions detects flags" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestNewCharacter
  ensure state.hasPendingActions "Should detect pending actions"

test "clearPendingActions works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestNewCharacter
  state := state.clearPendingActions
  ensure (!state.hasPendingActions) "Should clear pending actions"

/-! ## AI Writing Action Types -/

test "continue_ shouldInsertIntoEditor = true" := do
  ensure Enchiridion.AIWritingAction.continue_.shouldInsertIntoEditor
    "continue_ should insert into editor"

test "brainstorm shouldInsertIntoEditor = false" := do
  ensure (!Enchiridion.AIWritingAction.brainstorm.shouldInsertIntoEditor)
    "brainstorm should not insert into editor"

test "AIWritingAction instructions defined" := do
  ensure (Enchiridion.AIWritingAction.continue_.instruction.length > 0)
    "Instructions should be defined"

/-! ## AI Writing Action State -/

test "requestAIWritingAction works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestAIWritingAction .continue_
  state.pendingAIWritingAction ≡ some .continue_
  ensure state.insertAIResponseIntoEditor "Should set insert flag"

test "clearAIWritingAction works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestAIWritingAction .continue_
  state := state.clearAIWritingAction
  ensure state.pendingAIWritingAction.isNone "Should clear action"

test "brainstorm sets insertAIResponseIntoEditor = false" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestAIWritingAction .brainstorm
  ensure (!state.insertAIResponseIntoEditor) "Should not insert for brainstorm"

/-! ## Editor Text Manipulation -/

test "appendTextToEditor works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  let testContent := "Line 1\nLine 2\nLine 3"
  state := { state with editorTextArea := Terminus.TextArea.fromString testContent }
  state := state.appendTextToEditor "Appended text"
  let editorText := state.editorTextArea.text
  ensure (editorText.endsWith "Appended text") "Text should be appended"

test "replaceEditorContent works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := { state with editorTextArea := Terminus.TextArea.fromString "Old content" }
  state := state.replaceEditorContent "Completely new content"
  state.editorTextArea.text ≡ "Completely new content"

test "insertTextAtCursor (single line) works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := { state with editorTextArea := Terminus.TextArea.fromString "Hello World" }
  state := { state with editorTextArea := { state.editorTextArea with cursorCol := 6 } }
  state := state.insertTextAtCursor "Beautiful "
  state.editorTextArea.text ≡ "Hello Beautiful World"

/-! ## Handle AI Writing Response -/

test "handleAIWritingResponse appends for continue" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestAIWritingAction .continue_
  state := { state with editorTextArea := Terminus.TextArea.fromString "Original content" }
  state := state.handleAIWritingResponse "AI generated text"
  ensure (state.editorTextArea.text.endsWith "AI generated text")
    "Should append AI response"

test "handleAIWritingResponse replaces for rewrite" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestAIWritingAction .rewrite
  state := { state with editorTextArea := Terminus.TextArea.fromString "Old content" }
  state := state.handleAIWritingResponse "Rewritten content"
  state.editorTextArea.text ≡ "Rewritten content"

test "handleAIWritingResponse ignores brainstorm" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestAIWritingAction .brainstorm
  state := { state with editorTextArea := Terminus.TextArea.fromString "Original" }
  let beforeBrainstorm := state.editorTextArea.text
  state := state.handleAIWritingResponse "Ideas that shouldn't appear in editor"
  state.editorTextArea.text ≡ beforeBrainstorm

/-! ## Help Mode Toggle -/

test "showHelp enters help mode" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.hideHelp
  state := state.showHelp
  state.mode ≡ Enchiridion.AppMode.help

test "toggleHelp exits help mode" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.showHelp
  state := state.toggleHelp
  ensure (state.mode != Enchiridion.AppMode.help) "Should exit help mode"

test "toggleHelp enters help mode again" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.hideHelp
  state := state.toggleHelp
  state.mode ≡ Enchiridion.AppMode.help

/-! ## Word Count Stats -/

test "Word count stats contains word count" := do
  let mut testNovel ← Enchiridion.Novel.create "Test Novel" "Test Author"
  let mut testChapter ← Enchiridion.Chapter.create "Chapter 1"
  let mut testScene ← Enchiridion.Scene.create "Test Scene"
  testScene := { testScene with content := "One two three four five." }
  testScene := testScene.updateWordCount
  testChapter := testChapter.addScene testScene
  testNovel := testNovel.addChapter testChapter
  let testProject : Enchiridion.Project := {
    novel := testNovel
    characters := #[]
    worldNotes := #[]
  }
  let state := Enchiridion.AppState.fromProject testProject
  let stats := state.getWordCountStats
  ensure (String.containsSubstr stats "5 words") "Should contain word count"

test "Word count stats contains chapter count" := do
  let mut testNovel ← Enchiridion.Novel.create "Test Novel" "Test Author"
  let testChapter ← Enchiridion.Chapter.create "Chapter 1"
  testNovel := testNovel.addChapter testChapter
  let testProject : Enchiridion.Project := {
    novel := testNovel
    characters := #[]
    worldNotes := #[]
  }
  let state := Enchiridion.AppState.fromProject testProject
  let stats := state.getWordCountStats
  ensure (String.containsSubstr stats "1 chapters") "Should contain chapter count"

test "Word count stats contains scene count" := do
  let mut testNovel ← Enchiridion.Novel.create "Test Novel" "Test Author"
  let mut testChapter ← Enchiridion.Chapter.create "Chapter 1"
  let testScene ← Enchiridion.Scene.create "Test Scene"
  testChapter := testChapter.addScene testScene
  testNovel := testNovel.addChapter testChapter
  let testProject : Enchiridion.Project := {
    novel := testNovel
    characters := #[]
    worldNotes := #[]
  }
  let state := Enchiridion.AppState.fromProject testProject
  let stats := state.getWordCountStats
  ensure (String.containsSubstr stats "1 scenes") "Should contain scene count"

/-! ## Export to Markdown -/

test "Markdown export contains title" := do
  let mut testNovel ← Enchiridion.Novel.create "Test Novel" "Test Author"
  let testProject : Enchiridion.Project := {
    novel := testNovel
    characters := #[]
    worldNotes := #[]
  }
  let state := Enchiridion.AppState.fromProject testProject
  let markdown := state.exportToMarkdown
  ensure (String.containsSubstr markdown "# Test Novel") "Should contain title"

test "Markdown export contains chapter" := do
  let mut testNovel ← Enchiridion.Novel.create "Test Novel" "Test Author"
  let testChapter ← Enchiridion.Chapter.create "Chapter 1"
  testNovel := testNovel.addChapter testChapter
  let testProject : Enchiridion.Project := {
    novel := testNovel
    characters := #[]
    worldNotes := #[]
  }
  let state := Enchiridion.AppState.fromProject testProject
  let markdown := state.exportToMarkdown
  ensure (String.containsSubstr markdown "## Chapter 1") "Should contain chapter"

test "Markdown export contains scene" := do
  let mut testNovel ← Enchiridion.Novel.create "Test Novel" "Test Author"
  let mut testChapter ← Enchiridion.Chapter.create "Chapter 1"
  let testScene ← Enchiridion.Scene.create "Test Scene"
  testChapter := testChapter.addScene testScene
  testNovel := testNovel.addChapter testChapter
  let testProject : Enchiridion.Project := {
    novel := testNovel
    characters := #[]
    worldNotes := #[]
  }
  let state := Enchiridion.AppState.fromProject testProject
  let markdown := state.exportToMarkdown
  ensure (String.containsSubstr markdown "### Test Scene") "Should contain scene"

test "Markdown export contains scene content" := do
  let mut testNovel ← Enchiridion.Novel.create "Test Novel" "Test Author"
  let mut testChapter ← Enchiridion.Chapter.create "Chapter 1"
  let mut testScene ← Enchiridion.Scene.create "Test Scene"
  testScene := { testScene with content := "One two three four five." }
  testChapter := testChapter.addScene testScene
  testNovel := testNovel.addChapter testChapter
  let testProject : Enchiridion.Project := {
    novel := testNovel
    characters := #[]
    worldNotes := #[]
  }
  let state := Enchiridion.AppState.fromProject testProject
  let markdown := state.exportToMarkdown
  ensure (String.containsSubstr markdown "One two three four five.") "Should contain scene content"

/-! ## Export Request -/

test "requestExport sets pendingExport flag" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.requestExport
  ensure state.pendingExport "Flag should be set"

/-! ## Config Tests -/

test "Config parsing" := do
  let configJson := Lean.Json.mkObj [
    ("openRouterApiKey", Lean.Json.str "test-key"),
    ("defaultModel", Lean.Json.str "test-model"),
    ("autoSaveEnabled", Lean.Json.bool true),
    ("autoSaveIntervalMs", Lean.Json.num 30000)
  ]
  match Enchiridion.Config.fromJson? configJson with
  | some config =>
    config.openRouterApiKey ≡ "test-key"
    config.defaultModel ≡ "test-model"
    ensure config.autoSaveEnabled "autoSaveEnabled should be true"
    config.autoSaveIntervalMs ≡ 30000
  | none =>
    throw <| IO.userError "Config parsing failed"

test "Config round-trip" := do
  let sampleConfig : Enchiridion.Config := {
    openRouterApiKey := "round-trip-key"
    defaultModel := "round-trip-model"
    autoSaveEnabled := false
    autoSaveIntervalMs := 45000
  }
  let configJson := sampleConfig.toJson
  match Enchiridion.Config.fromJson? configJson with
  | some parsedConfig =>
    parsedConfig.openRouterApiKey ≡ sampleConfig.openRouterApiKey
    parsedConfig.defaultModel ≡ sampleConfig.defaultModel
    parsedConfig.autoSaveEnabled ≡ sampleConfig.autoSaveEnabled
    parsedConfig.autoSaveIntervalMs ≡ sampleConfig.autoSaveIntervalMs
  | none =>
    throw <| IO.userError "Config round-trip parsing failed"

/-! ## Error/Status Message Handling -/

test "setError works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.setError "Test error"
  state.errorMessage ≡ some "Test error"

test "clearError works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.setError "Test error"
  state := state.clearError
  ensure state.errorMessage.isNone "Error should be cleared"

test "setStatus works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.setStatus "Test status"
  state.statusMessage ≡ some "Test status"

test "clearStatus works" := do
  let project ← Enchiridion.Project.create "Test" "Author"
  let mut state := Enchiridion.AppState.fromProject project
  state := state.setStatus "Test status"
  state := state.clearStatus
  ensure state.statusMessage.isNone "Status should be cleared"

end Enchiridion.Tests

def main : IO Unit := do
  IO.println "╔════════════════════════════════════════╗"
  IO.println "║       Enchiridion Test Suite           ║"
  IO.println "╚════════════════════════════════════════╝"
  IO.println ""

  let exitCode ← runAllSuites

  IO.println ""
  if exitCode == 0 then
    IO.println "✓ All tests passed!"
  else
    IO.println "✗ Some tests failed"
    IO.Process.exit 1
