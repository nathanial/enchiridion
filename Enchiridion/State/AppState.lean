/-
  Enchiridion Application State
  Main state structure for the application
-/

import Terminus
import Enchiridion.Core.Types
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

  -- Notes panel state
  notesTab : Nat := 0  -- 0 = Characters, 1 = World
  selectedCharacterIdx : Nat := 0
  selectedNoteIdx : Nat := 0

  -- AI configuration
  openRouterApiKey : String := ""
  selectedModel : String := "anthropic/claude-3.5-sonnet"

  -- Status
  statusMessage : Option String := none
  errorMessage : Option String := none

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
    { state with
        currentChapterId := some chapterId
        currentSceneId := some sceneId
        editorTextArea := textArea }
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

end AppState

end Enchiridion
