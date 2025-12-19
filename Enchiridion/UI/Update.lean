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
    | .escape => { state with isStreaming := false, streamBuffer := "" }
    | _ => state
  else
    match key.code with
    | .enter =>
      -- Send message (would trigger AI request in real implementation)
      let content := state.chatInput.value
      if content.trim.isEmpty then state
      else
        let state := { state with chatInput := TextInput.new }
        -- For now, just add user message (AI integration comes later)
        state
    | _ =>
      -- Let TextInput handle other keys
      let input := state.chatInput.handleKey key
      { state with chatInput := input }

/-- Handle notes panel input -/
def updateNotes (state : AppState) (key : KeyEvent) : AppState :=
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

  | _ => state

/-- Main update function -/
def update (state : AppState) (keyEvent : Option KeyEvent) : AppState × Bool :=
  match keyEvent with
  | none => (state, false)  -- No input, no change
  | some key =>
    -- Global key handlers first
    -- Ctrl+Q to quit
    if key.code == .char 'q' && key.modifiers.ctrl then
      (state, true)

    -- Tab to cycle focus
    else if key.code == .tab && !key.modifiers.shift then
      (state.nextFocus, false)

    -- Shift+Tab to cycle focus backwards
    else if key.code == .tab && key.modifiers.shift then
      (state.prevFocus, false)

    -- Ctrl+S to save (placeholder)
    else if key.code == .char 's' && key.modifiers.ctrl then
      let state := state.setStatus "Saving... (not implemented)"
      (state, false)

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

    -- Panel-specific handlers
    else
      let state := match state.focus with
        | .navigation => updateNavigation state key
        | .editor => updateEditor state key
        | .chat => updateChat state key
        | .notes => updateNotes state key
      (state, false)

end Enchiridion.UI
