/-
  Enchiridion Application State
  Main state structure for the application
-/

import Terminus
import Enchiridion.Core.Types
import Enchiridion.Model.Novel
import Enchiridion.Model.Project
import Enchiridion.State.Focus

namespace Enchiridion

/-- Chat message for AI conversation -/
structure ChatMessage where
  id : EntityId
  role : String  -- "user", "assistant", or "system"
  content : String
  timestamp : Timestamp
  isStreaming : Bool := false
  deriving Repr, Inhabited

namespace ChatMessage

def create (role : String) (content : String) : IO ChatMessage := do
  let id ← EntityId.generate
  let ts ← Timestamp.now
  return { id := id, role := role, content := content, timestamp := ts }

def isUser (msg : ChatMessage) : Bool := msg.role == "user"
def isAssistant (msg : ChatMessage) : Bool := msg.role == "assistant"
def isSystem (msg : ChatMessage) : Bool := msg.role == "system"

end ChatMessage

/-- Main application state -/
structure AppState where
  -- Data
  project : Project

  -- UI Focus
  focus : PanelFocus := .editor
  mode : AppMode := .normal

  -- Navigation panel state
  selectedChapterIdx : Nat := 0
  selectedSceneIdx : Nat := 0
  navCollapsed : Array Bool := #[]  -- Track collapsed state per chapter

  -- Editor panel state
  editorTextArea : Terminus.TextArea := Terminus.TextArea.new
  currentChapterId : Option EntityId := none
  currentSceneId : Option EntityId := none

  -- Chat panel state
  chatMessages : Array ChatMessage := #[]
  chatInput : Terminus.TextInput := Terminus.TextInput.new
  isStreaming : Bool := false
  streamBuffer : String := ""
  cancelStreaming : Bool := false  -- Set by Escape to cancel streaming

  -- Notes panel state
  notesTab : Nat := 0  -- 0 = Characters, 1 = World
  selectedCharacterIdx : Nat := 0
  selectedNoteIdx : Nat := 0
  notesEditMode : Bool := false  -- True when editing a character/note
  notesEditField : Nat := 0  -- 0 = name/title, 1 = description/content
  notesNameInput : Terminus.TextInput := Terminus.TextInput.new
  notesContentArea : Terminus.TextArea := Terminus.TextArea.new

  -- AI configuration
  openRouterApiKey : String := ""
  selectedModel : String := "anthropic/claude-3.5-sonnet"

  -- Status
  statusMessage : Option String := none
  errorMessage : Option String := none

  -- Pending actions (to be executed in IO context)
  pendingNewChapter : Bool := false
  pendingNewScene : Bool := false
  pendingSave : Bool := false
  pendingAIMessage : Option String := none  -- User message to send to AI
  pendingNewCharacter : Bool := false
  pendingNewWorldNote : Bool := false

  -- Quit confirmation (for unsaved changes warning)
  quitConfirmPending : Bool := false

  deriving Inhabited

namespace AppState

/-- Create initial app state with empty project -/
def create : IO AppState := do
  let project ← Project.empty
  return { project := project }

/-- Create app state with a given project -/
def fromProject (project : Project) : AppState :=
  { project := project
    navCollapsed := project.novel.chapters.map (fun _ => false) }

/-- Switch focus to next panel -/
def nextFocus (state : AppState) : AppState :=
  if state.mode.allowsPanelSwitch then
    { state with focus := state.focus.next }
  else
    state

/-- Switch focus to previous panel -/
def prevFocus (state : AppState) : AppState :=
  if state.mode.allowsPanelSwitch then
    { state with focus := state.focus.prev }
  else
    state

/-- Set focus directly -/
def setFocus (state : AppState) (focus : PanelFocus) : AppState :=
  if state.mode.allowsPanelSwitch then
    { state with focus := focus }
  else
    state

/-- Set status message -/
def setStatus (state : AppState) (msg : String) : AppState :=
  { state with statusMessage := some msg }

/-- Clear status message -/
def clearStatus (state : AppState) : AppState :=
  { state with statusMessage := none }

/-- Set error message -/
def setError (state : AppState) (msg : String) : AppState :=
  { state with errorMessage := some msg }

/-- Clear error message -/
def clearError (state : AppState) : AppState :=
  { state with errorMessage := none }

/-- Get current scene content -/
def getCurrentSceneContent (state : AppState) : Option String := do
  let chapterId ← state.currentChapterId
  let sceneId ← state.currentSceneId
  let scene ← state.project.novel.getScene chapterId sceneId
  return scene.content

/-- Get current scene title -/
def getCurrentSceneTitle (state : AppState) : String :=
  match state.currentChapterId, state.currentSceneId with
  | some chapterId, some sceneId =>
    match state.project.novel.getScene chapterId sceneId with
    | some scene => scene.title
    | none => "No Scene"
  | _, _ => "No Scene Selected"

/-- Load scene into editor -/
def loadScene (state : AppState) (chapterId : EntityId) (sceneId : EntityId) : AppState :=
  match state.project.novel.getScene chapterId sceneId with
  | some scene =>
    let textArea := Terminus.TextArea.fromString scene.content
    -- Find the chapter and scene indices for navigation sync
    let novel := state.project.novel
    let chapterIdx := novel.chapters.findIdx? (·.id == chapterId) |>.getD state.selectedChapterIdx
    let sceneIdx := match novel.getChapter chapterId with
      | some chapter => chapter.scenes.findIdx? (·.id == sceneId) |>.getD state.selectedSceneIdx
      | none => state.selectedSceneIdx
    { state with
        currentChapterId := some chapterId
        currentSceneId := some sceneId
        editorTextArea := textArea
        selectedChapterIdx := chapterIdx
        selectedSceneIdx := sceneIdx }
  | none => state

/-- Save current editor content to scene -/
def saveCurrentScene (state : AppState) : AppState :=
  match state.currentChapterId, state.currentSceneId with
  | some chapterId, some sceneId =>
    let content := state.editorTextArea.text
    let project := state.project.updateNovel fun novel =>
      novel.updateScene chapterId sceneId fun scene =>
        { scene.updateWordCount with content := content }
    { state with project := project }
  | _, _ => state

/-- Add a chat message -/
def addChatMessage (state : AppState) (msg : ChatMessage) : AppState :=
  { state with chatMessages := state.chatMessages.push msg }

/-- Update the last chat message (for streaming) -/
def updateLastChatMessage (state : AppState) (content : String) : AppState :=
  if state.chatMessages.isEmpty then state
  else
    let lastIdx := state.chatMessages.size - 1
    let msgs := state.chatMessages.modify lastIdx fun msg =>
      { msg with content := content }
    { state with chatMessages := msgs }

/-- Add a new chapter to the novel -/
def addNewChapter (state : AppState) (chapter : Chapter) : AppState :=
  let project := state.project.updateNovel (·.addChapter chapter)
  let chapterIdx := project.novel.chapters.size - 1
  { state with
      project := project
      selectedChapterIdx := chapterIdx
      selectedSceneIdx := 0
      navCollapsed := state.navCollapsed.push false }

/-- Add a new scene to the current chapter -/
def addNewScene (state : AppState) (scene : Scene) : AppState :=
  let novel := state.project.novel
  if state.selectedChapterIdx < novel.chapters.size then
    let chapter := novel.chapters[state.selectedChapterIdx]!
    let project := state.project.updateNovel fun n =>
      n.updateChapter chapter.id (·.addScene scene)
    let sceneIdx := chapter.scenes.size  -- New scene will be at this index
    { state with
        project := project
        selectedSceneIdx := sceneIdx }
  else
    state

/-- Delete a chapter by index -/
def deleteChapter (state : AppState) (chapterIdx : Nat) : AppState :=
  let novel := state.project.novel
  if h : chapterIdx < novel.chapters.size then
    let chapters := novel.chapters.eraseIdx chapterIdx
    let project := { state.project with novel := { novel with chapters := chapters }, isDirty := true }
    let newSelectedIdx := if chapterIdx > 0 then chapterIdx - 1 else 0
    let newNavCollapsed := if h2 : chapterIdx < state.navCollapsed.size then
      state.navCollapsed.eraseIdx chapterIdx
    else
      state.navCollapsed
    { state with
        project := project
        selectedChapterIdx := newSelectedIdx
        selectedSceneIdx := 0
        navCollapsed := newNavCollapsed
        currentChapterId := none
        currentSceneId := none }
  else
    state

/-- Delete a scene from the current chapter -/
def deleteScene (state : AppState) (sceneIdx : Nat) : AppState :=
  let novel := state.project.novel
  if state.selectedChapterIdx < novel.chapters.size then
    let chapter := novel.chapters[state.selectedChapterIdx]!
    if h : sceneIdx < chapter.scenes.size then
      let scenes := chapter.scenes.eraseIdx sceneIdx
      let project := state.project.updateNovel fun n =>
        n.updateChapter chapter.id fun c => { c with scenes := scenes }
      let newSelectedIdx := if sceneIdx > 0 then sceneIdx - 1 else 0
      { state with
          project := project
          selectedSceneIdx := newSelectedIdx
          currentChapterId := none
          currentSceneId := none }
    else
      state
  else
    state

/-- Request creation of a new chapter (will be executed in IO context) -/
def requestNewChapter (state : AppState) : AppState :=
  { state with pendingNewChapter := true }

/-- Request creation of a new scene (will be executed in IO context) -/
def requestNewScene (state : AppState) : AppState :=
  { state with pendingNewScene := true }

/-- Request save (will be executed in IO context) -/
def requestSave (state : AppState) : AppState :=
  { state with pendingSave := true }

/-- Request AI message (will be executed in IO context) -/
def requestAIMessage (state : AppState) (message : String) : AppState :=
  { state with pendingAIMessage := some message }

/-- Request new character (will be executed in IO context) -/
def requestNewCharacter (state : AppState) : AppState :=
  { state with pendingNewCharacter := true }

/-- Request new world note (will be executed in IO context) -/
def requestNewWorldNote (state : AppState) : AppState :=
  { state with pendingNewWorldNote := true }

/-- Clear pending actions -/
def clearPendingActions (state : AppState) : AppState :=
  { state with
      pendingNewChapter := false
      pendingNewScene := false
      pendingSave := false
      pendingAIMessage := none
      pendingNewCharacter := false
      pendingNewWorldNote := false }

/-- Check if there are any pending actions -/
def hasPendingActions (state : AppState) : Bool :=
  state.pendingNewChapter || state.pendingNewScene || state.pendingSave ||
  state.pendingAIMessage.isSome || state.pendingNewCharacter || state.pendingNewWorldNote

/-- Enter edit mode for the selected character -/
def editSelectedCharacter (state : AppState) : AppState :=
  if state.selectedCharacterIdx < state.project.characters.size then
    let char := state.project.characters[state.selectedCharacterIdx]!
    let nameInput := Terminus.TextInput.new.withValue char.name
    let contentArea := Terminus.TextArea.fromString char.description
    { state with
        notesEditMode := true
        notesEditField := 0
        notesNameInput := nameInput
        notesContentArea := contentArea }
  else
    state

/-- Enter edit mode for the selected world note -/
def editSelectedWorldNote (state : AppState) : AppState :=
  if state.selectedNoteIdx < state.project.worldNotes.size then
    let note := state.project.worldNotes[state.selectedNoteIdx]!
    let nameInput := Terminus.TextInput.new.withValue note.title
    let contentArea := Terminus.TextArea.fromString note.content
    { state with
        notesEditMode := true
        notesEditField := 0
        notesNameInput := nameInput
        notesContentArea := contentArea }
  else
    state

/-- Save edits to the selected character -/
def saveCharacterEdits (state : AppState) : AppState :=
  if state.selectedCharacterIdx < state.project.characters.size then
    let chars := state.project.characters.modify state.selectedCharacterIdx fun char =>
      { char with
          name := state.notesNameInput.value
          description := state.notesContentArea.text }
    let project := { state.project with characters := chars, isDirty := true }
    { state with
        project := project
        notesEditMode := false }
  else
    { state with notesEditMode := false }

/-- Save edits to the selected world note -/
def saveWorldNoteEdits (state : AppState) : AppState :=
  if state.selectedNoteIdx < state.project.worldNotes.size then
    let notes := state.project.worldNotes.modify state.selectedNoteIdx fun note =>
      { note with
          title := state.notesNameInput.value
          content := state.notesContentArea.text }
    let project := { state.project with worldNotes := notes, isDirty := true }
    { state with
        project := project
        notesEditMode := false }
  else
    { state with notesEditMode := false }

/-- Add a new character to the project -/
def addNewCharacter (state : AppState) (char : Character) : AppState :=
  let project := { state.project with
    characters := state.project.characters.push char
    isDirty := true }
  { state with
      project := project
      selectedCharacterIdx := project.characters.size - 1 }

/-- Add a new world note to the project -/
def addNewWorldNote (state : AppState) (note : WorldNote) : AppState :=
  let project := { state.project with
    worldNotes := state.project.worldNotes.push note
    isDirty := true }
  { state with
      project := project
      selectedNoteIdx := project.worldNotes.size - 1 }

/-- Delete the selected character -/
def deleteSelectedCharacter (state : AppState) : AppState :=
  if h : state.selectedCharacterIdx < state.project.characters.size then
    let chars := state.project.characters.eraseIdx state.selectedCharacterIdx
    let project := { state.project with characters := chars, isDirty := true }
    let newIdx := if state.selectedCharacterIdx > 0 then state.selectedCharacterIdx - 1 else 0
    { state with
        project := project
        selectedCharacterIdx := newIdx }
  else
    state

/-- Delete the selected world note -/
def deleteSelectedWorldNote (state : AppState) : AppState :=
  if h : state.selectedNoteIdx < state.project.worldNotes.size then
    let notes := state.project.worldNotes.eraseIdx state.selectedNoteIdx
    let project := { state.project with worldNotes := notes, isDirty := true }
    let newIdx := if state.selectedNoteIdx > 0 then state.selectedNoteIdx - 1 else 0
    { state with
        project := project
        selectedNoteIdx := newIdx }
  else
    state

end AppState

end Enchiridion
