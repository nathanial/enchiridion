/-
  Enchiridion UI Update
  Input handling and state updates
-/

import Terminus
import Enchiridion.State.AppState

namespace Enchiridion.UI

open Terminus

/-- Handle navigation panel input -/
def updateNavigation (state : AppState) (key : KeyEvent) : AppState :=
  let novel := state.project.novel

  match key.code with
  | .up =>
    if state.selectedSceneIdx > 0 then
      { state with selectedSceneIdx := state.selectedSceneIdx - 1 }
    else if state.selectedChapterIdx > 0 then
      -- Move to previous chapter's last scene
      let prevChapterIdx := state.selectedChapterIdx - 1
      let prevChapter := novel.chapters.getD prevChapterIdx default
      { state with
          selectedChapterIdx := prevChapterIdx
          selectedSceneIdx := if prevChapter.scenes.isEmpty then 0 else prevChapter.scenes.size - 1 }
    else
      state

  | .down =>
    let chapter := novel.chapters.getD state.selectedChapterIdx default
    if state.selectedSceneIdx + 1 < chapter.scenes.size then
      { state with selectedSceneIdx := state.selectedSceneIdx + 1 }
    else if state.selectedChapterIdx + 1 < novel.chapters.size then
      -- Move to next chapter
      { state with
          selectedChapterIdx := state.selectedChapterIdx + 1
          selectedSceneIdx := 0 }
    else
      state

  | .enter =>
    -- Auto-save current scene, then load selected scene
    let state := state.saveCurrentScene
    let chapter := novel.chapters.getD state.selectedChapterIdx default
    let scene := chapter.scenes.getD state.selectedSceneIdx default
    state.loadScene chapter.id scene.id

  | .char ' ' =>
    -- Toggle chapter collapse
    let currentVal := state.navCollapsed.getD state.selectedChapterIdx false
    let collapsed := if state.selectedChapterIdx < state.navCollapsed.size then
      state.navCollapsed.set! state.selectedChapterIdx (!currentVal)
    else
      -- Extend array if needed
      let padding := List.replicate (state.selectedChapterIdx + 1 - state.navCollapsed.size) false
      let extended := state.navCollapsed ++ padding.toArray
      extended.set! state.selectedChapterIdx (!currentVal)
    { state with navCollapsed := collapsed }

  | .delete =>
    -- Delete selected scene (or chapter if no scenes)
    let chapter := novel.chapters.getD state.selectedChapterIdx default
    if chapter.scenes.isEmpty then
      -- Delete the chapter
      state.deleteChapter state.selectedChapterIdx
    else
      -- Delete the selected scene
      state.deleteScene state.selectedSceneIdx

  | _ => state

/-- Handle editor panel input -/
def updateEditor (state : AppState) (key : KeyEvent) : AppState :=
  -- Let TextArea handle the key
  let textArea := state.editorTextArea.handleKey key
  let state := { state with editorTextArea := textArea }
  -- Mark project as dirty
  { state with project := state.project.markDirty }

/-- Handle chat panel input -/
def updateChat (state : AppState) (key : KeyEvent) : AppState :=
  if state.isStreaming then
    -- During streaming, only allow Escape to cancel
    match key.code with
    | .escape => { state with cancelStreaming := true }
    | _ => state
  else
    match key.code with
    | .enter =>
      -- Send message to AI
      let content := state.chatInput.value
      if content.trim.isEmpty then state
      else
        -- Clear input and request AI response
        let state := { state with chatInput := Terminus.TextInput.new }
        state.requestAIMessage content
    | _ =>
      -- Let TextInput handle other keys
      let input := state.chatInput.handleKey key
      { state with chatInput := input }

/-- Handle notes panel input in list mode -/
def updateNotesListMode (state : AppState) (key : KeyEvent) : AppState :=
  match key.code with
  | .left =>
    if state.notesTab > 0 then
      { state with notesTab := state.notesTab - 1 }
    else
      state

  | .right =>
    if state.notesTab < 1 then
      { state with notesTab := state.notesTab + 1 }
    else
      state

  | .up =>
    if state.notesTab == 0 then
      if state.selectedCharacterIdx > 0 then
        { state with selectedCharacterIdx := state.selectedCharacterIdx - 1 }
      else
        state
    else
      if state.selectedNoteIdx > 0 then
        { state with selectedNoteIdx := state.selectedNoteIdx - 1 }
      else
        state

  | .down =>
    if state.notesTab == 0 then
      if state.selectedCharacterIdx + 1 < state.project.characters.size then
        { state with selectedCharacterIdx := state.selectedCharacterIdx + 1 }
      else
        state
    else
      if state.selectedNoteIdx + 1 < state.project.worldNotes.size then
        { state with selectedNoteIdx := state.selectedNoteIdx + 1 }
      else
        state

  | .enter =>
    -- Enter edit mode for selected item
    if state.notesTab == 0 then
      state.editSelectedCharacter
    else
      state.editSelectedWorldNote

  | .char 'n' =>
    -- Create new character or note
    if state.notesTab == 0 then
      state.requestNewCharacter
    else
      state.requestNewWorldNote

  | .delete =>
    -- Delete selected item
    if state.notesTab == 0 then
      state.deleteSelectedCharacter
    else
      state.deleteSelectedWorldNote

  | _ => state

/-- Handle notes panel input in edit mode -/
def updateNotesEditMode (state : AppState) (key : KeyEvent) : AppState :=
  match key.code with
  | .escape =>
    -- Cancel editing, discard changes
    { state with notesEditMode := false }

  | .tab =>
    -- Switch between name and content fields
    if state.notesEditField == 0 then
      { state with notesEditField := 1 }
    else
      { state with notesEditField := 0 }

  | _ =>
    -- Pass key to the focused input
    if state.notesEditField == 0 then
      -- Name input
      let input := state.notesNameInput.handleKey key
      { state with notesNameInput := input }
    else
      -- Content area
      let area := state.notesContentArea.handleKey key
      { state with notesContentArea := area }

/-- Handle notes panel input -/
def updateNotes (state : AppState) (key : KeyEvent) : AppState :=
  if state.notesEditMode then
    updateNotesEditMode state key
  else
    updateNotesListMode state key

/-- Main update function -/
def update (state : AppState) (keyEvent : Option KeyEvent) : AppState × Bool :=
  match keyEvent with
  | none => (state, false)  -- No input, no change
  | some key =>
    -- Clear error message on any keypress
    let state := if state.errorMessage.isSome then state.clearError else state

    -- Global key handlers first
    -- Ctrl+Q to quit (with unsaved changes warning)
    if key.code == .char 'q' && key.modifiers.ctrl then
      if state.project.isDirty && !state.quitConfirmPending then
        -- First Ctrl+Q with unsaved changes: show warning
        let state := state.setStatus "Unsaved changes! Press Ctrl+Q again to quit without saving, or Ctrl+S to save."
        ({ state with quitConfirmPending := true }, false)
      else
        -- Either no unsaved changes, or confirmation already shown
        (state, true)

    -- Escape clears quit confirmation or closes help
    else if key.code == .escape && state.quitConfirmPending then
      let state := state.clearStatus
      ({ state with quitConfirmPending := false }, false)

    else if key.code == .escape && state.mode == .help then
      (state.hideHelp, false)

    -- ? to show help
    else if key.code == .char '?' then
      (state.toggleHelp, false)

    -- F1 also shows help
    else if key.code == .f 1 then
      (state.toggleHelp, false)

    -- Tab to cycle focus (but not in notes edit mode where Tab switches fields)
    else if key.code == .tab && !key.modifiers.shift && !(state.focus == .notes && state.notesEditMode) then
      (state.nextFocus, false)

    -- Shift+Tab to cycle focus backwards
    else if key.code == .tab && key.modifiers.shift && !(state.focus == .notes && state.notesEditMode) then
      (state.prevFocus, false)

    -- Ctrl+E to export to markdown
    else if key.code == .char 'e' && key.modifiers.ctrl then
      -- Save current scene first, then export
      let state := state.saveCurrentScene
      (state.requestExport, false)

    -- Ctrl+S to save
    else if key.code == .char 's' && key.modifiers.ctrl then
      -- In notes edit mode, save the current note/character
      if state.focus == .notes && state.notesEditMode then
        let state := if state.notesTab == 0 then
          state.saveCharacterEdits
        else
          state.saveWorldNoteEdits
        let state := state.setStatus "Saved changes"
        (state, false)
      else
        -- Save current scene content first, then request save
        let state := state.saveCurrentScene
        (state.requestSave, false)

    -- Ctrl+N for new chapter (when in navigation panel)
    else if key.code == .char 'n' && key.modifiers.ctrl && !key.modifiers.shift then
      if state.focus == .navigation then
        (state.requestNewChapter, false)
      else
        (state, false)

    -- Ctrl+Shift+N for new scene (when in navigation panel)
    else if key.code == .char 'n' && key.modifiers.ctrl && key.modifiers.shift then
      if state.focus == .navigation then
        (state.requestNewScene, false)
      else
        (state, false)

    -- AI Writing Actions (only when in editor and not already streaming)
    -- Ctrl+Enter: Continue writing
    else if key.code == .enter && key.modifiers.ctrl && state.focus == .editor && !state.isStreaming then
      let state := state.requestAIWritingAction .continue_
      let state := state.setStatus "AI: Continuing story..."
      (state, false)

    -- Ctrl+R: Rewrite current content
    else if key.code == .char 'r' && key.modifiers.ctrl && state.focus == .editor && !state.isStreaming then
      let state := state.requestAIWritingAction .rewrite
      let state := state.setStatus "AI: Rewriting scene..."
      (state, false)

    -- Ctrl+B: Brainstorm ideas
    else if key.code == .char 'b' && key.modifiers.ctrl && state.focus == .editor && !state.isStreaming then
      let state := state.requestAIWritingAction .brainstorm
      let state := state.setStatus "AI: Brainstorming ideas..."
      (state, false)

    -- Ctrl+D: Add dialogue
    else if key.code == .char 'd' && key.modifiers.ctrl && state.focus == .editor && !state.isStreaming then
      let state := state.requestAIWritingAction .dialogue
      let state := state.setStatus "AI: Writing dialogue..."
      (state, false)

    -- Ctrl+G: Add description
    else if key.code == .char 'g' && key.modifiers.ctrl && state.focus == .editor && !state.isStreaming then
      let state := state.requestAIWritingAction .description
      let state := state.setStatus "AI: Writing description..."
      (state, false)

    -- Panel-specific handlers
    else
      let state := match state.focus with
        | .navigation => updateNavigation state key
        | .editor => updateEditor state key
        | .chat => updateChat state key
        | .notes => updateNotes state key
      (state, false)

end Enchiridion.UI
