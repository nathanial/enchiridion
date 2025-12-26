/-
  Enchiridion UI App
  Main application loop
-/

import Terminus
import Enchiridion.State.AppState
import Enchiridion.Model.Novel
import Enchiridion.Model.Character
import Enchiridion.Model.WorldNote
import Enchiridion.Storage.FileIO
import Enchiridion.Core.Config
import Enchiridion.AI.OpenRouter
import Enchiridion.AI.Prompts
import Enchiridion.AI.Streaming
import Enchiridion.UI.Draw
import Enchiridion.UI.Update

namespace Enchiridion.UI

open Terminus
open Enchiridion

/-- Create initial app state with a sample project for testing -/
def createSampleProject : IO AppState := do
  -- Create a sample novel for testing
  let mut novel ← Novel.create "The Great Adventure" "Test Author"
  novel := { novel with genre := "Fantasy", synopsis := "A tale of mystery and magic" }

  -- Add a sample chapter
  let mut chapter1 ← Chapter.create "Chapter 1: The Beginning"
  chapter1 := { chapter1 with synopsis := "Where our story begins" }

  -- Add sample scenes
  let mut scene1 ← Scene.create "The Awakening"
  scene1 := { scene1 with
    content := "The morning sun crept through the dusty window, casting long shadows across the floor. Sarah opened her eyes slowly, unsure of where she was or how she had gotten there.\n\nThe room was unfamiliar—stone walls covered in faded tapestries depicting scenes from battles long forgotten. A draft whistled through cracks in the ancient mortar."
  }
  scene1 := scene1.updateWordCount

  let mut scene2 ← Scene.create "The Discovery"
  scene2 := { scene2 with
    content := "On the table beside the bed lay a leather-bound journal, its pages yellowed with age. Sarah reached for it with trembling hands."
  }
  scene2 := scene2.updateWordCount

  chapter1 := chapter1.addScene scene1
  chapter1 := chapter1.addScene scene2
  novel := novel.addChapter chapter1

  -- Add another chapter
  let mut chapter2 ← Chapter.create "Chapter 2: The Journey"
  let mut scene3 ← Scene.create "Setting Out"
  scene3 := { scene3 with content := "With the journal tucked safely in her pack, Sarah stepped out into the morning light..." }
  scene3 := scene3.updateWordCount
  chapter2 := chapter2.addScene scene3
  novel := novel.addChapter chapter2

  -- Create project
  let project : Project := {
    novel := novel
    characters := #[]
    worldNotes := #[]
  }

  -- Create app state and load first scene
  let mut state := AppState.fromProject project
  let firstChapter := novel.chapters[0]!
  let firstScene := firstChapter.scenes[0]!
  state := state.loadScene firstChapter.id firstScene.id

  return state

/-- Process pending actions that require IO -/
def processPendingActions (state : AppState) : IO AppState := do
  let mut state := state

  -- Handle new chapter request
  if state.pendingNewChapter then
    let chapterNum := state.project.novel.chapters.size + 1
    let chapter ← Chapter.create s!"Chapter {chapterNum}"
    state := state.addNewChapter chapter
    state := state.setStatus s!"Created new chapter: {chapter.title}"

  -- Handle new scene request
  if state.pendingNewScene then
    let novel := state.project.novel
    if state.selectedChapterIdx < novel.chapters.size then
      let chapter := novel.chapters[state.selectedChapterIdx]!
      let sceneNum := chapter.scenes.size + 1
      let scene ← Scene.create s!"Scene {sceneNum}"
      state := state.addNewScene scene
      state := state.setStatus s!"Created new scene: {scene.title}"

  -- Handle save request
  if state.pendingSave then
    let path := state.project.filePath.getD (Storage.defaultSavePath state.project)
    let result ← Storage.saveProject state.project path
    match result with
    | .ok _ =>
      let project := state.project.markClean |>.setFilePath path
      state := { state with project := project }
      state := state.setStatus s!"Saved to {path}"
    | .error msg =>
      state := state.setError msg

  -- Handle new character request
  if state.pendingNewCharacter then
    let charNum := state.project.characters.size + 1
    let char ← Character.create s!"Character {charNum}"
    state := state.addNewCharacter char
    state := state.editSelectedCharacter  -- Enter edit mode for the new character
    state := state.setStatus s!"Created new character"

  -- Handle new world note request
  if state.pendingNewWorldNote then
    let noteNum := state.project.worldNotes.size + 1
    let note ← WorldNote.create s!"Note {noteNum}"
    state := state.addNewWorldNote note
    state := state.editSelectedWorldNote  -- Enter edit mode for the new note
    state := state.setStatus s!"Created new note"

  -- Handle export request
  if state.pendingExport then
    let markdown := state.exportToMarkdown
    let novelTitle := state.project.novel.title.replace " " "_"
    let exportPath := s!"{novelTitle}_export.md"
    try
      IO.FS.writeFile exportPath markdown
      state := state.setStatus s!"Exported to {exportPath}"
    catch _ =>
      state := state.setError s!"Failed to export to {exportPath}"

  -- Note: AI message handling is done in the main loop with streaming support

  -- Clear the pending flags (except pendingAIMessage which is handled separately)
  state := { state with
    pendingNewChapter := false
    pendingNewScene := false
    pendingSave := false
    pendingExport := false
    pendingNewCharacter := false
    pendingNewWorldNote := false }
  return state

/-- Custom update wrapper that handles IO actions -/
def updateWithIO (state : AppState) (keyEvent : Option KeyEvent) : IO (AppState × Bool) := do
  -- First run the pure update
  let (state, shouldQuit) := update state keyEvent

  -- Then process any pending IO actions
  let state ← processPendingActions state

  return (state, shouldQuit)

/-- Build API messages for a chat request -/
def buildAPIMessages (state : AppState) (userMessage : String) : IO (Array ChatMessage) := do
  let sceneContent := state.editorTextArea.text
  let currentChapter := match state.currentChapterId with
    | some cid => state.project.novel.getChapter cid
    | none => none
  let currentScene := match state.currentChapterId, state.currentSceneId with
    | some cid, some sid => state.project.novel.getScene cid sid
    | _, _ => none
  let fullPrompt := AI.buildPrompt .custom state.project sceneContent currentChapter currentScene userMessage
  let systemMsg ← ChatMessage.create "system" AI.systemPrompt
  let userApiMsg ← ChatMessage.create "user" fullPrompt
  return #[systemMsg, userApiMsg]

/-- Build API messages for an AI writing action -/
def buildWritingActionMessages (state : AppState) (action : AIWritingAction) : IO (Array ChatMessage) := do
  let sceneContent := state.editorTextArea.text
  let currentChapter := match state.currentChapterId with
    | some cid => state.project.novel.getChapter cid
    | none => none
  let currentScene := match state.currentChapterId, state.currentSceneId with
    | some cid, some sid => state.project.novel.getScene cid sid
    | _, _ => none
  let fullPrompt := AI.buildWritingActionPrompt action state.project sceneContent currentChapter currentScene
  let systemMsg ← ChatMessage.create "system" AI.systemPrompt
  let userApiMsg ← ChatMessage.create "user" fullPrompt
  return #[systemMsg, userApiMsg]

/-- Custom app loop with IO action support and streaming -/
partial def runLoop (app : App AppState) (drawFn : Frame → AppState → Frame)
    (sessionRef : IO.Ref (Option AI.StreamingSession)) : IO Unit := do
  if app.shouldQuit then return

  let mut state := app.state
  let mut session ← sessionRef.get

  -- Handle starting a new streaming request from chat
  if let some userMessage := state.pendingAIMessage then
    if state.openRouterApiKey.isEmpty then
      state := state.setError "OpenRouter API key not configured. Set OPENROUTER_API_KEY env var."
      state := { state with pendingAIMessage := none }
    else
      -- Add user message to chat
      let userMsg ← ChatMessage.create "user" userMessage
      state := state.addChatMessage userMsg

      -- Build config and messages
      let config : AI.OpenRouterConfig := {
        apiKey := state.openRouterApiKey
        model := state.selectedModel
      }
      let apiMessages ← buildAPIMessages state userMessage

      state := { state with
        isStreaming := true
        streamBuffer := ""
        cancelStreaming := false
        pendingAIMessage := none
      }
      state := state.setStatus "Connecting..."

      -- Start streaming request (blocks until headers received)
      let result ← AI.startStreamingCompletionSync config apiMessages
      match result with
      | .ok newSession =>
        session := some newSession
        state := state.setStatus "Streaming..."
      | .error msg =>
        state := { state with isStreaming := false, streamBuffer := "" }
        state := state.setError s!"AI Error: {msg}"

  -- Handle AI writing actions (Continue, Rewrite, etc.)
  if let some action := state.pendingAIWritingAction then
    if state.openRouterApiKey.isEmpty then
      state := state.setError "OpenRouter API key not configured. Set OPENROUTER_API_KEY env var."
      state := state.clearAIWritingAction
    else
      -- Add system message to chat showing the action
      let actionName := toString action
      let systemMsg ← ChatMessage.create "system" s!"[{actionName}] Generating..."
      state := state.addChatMessage systemMsg

      -- Build config and messages for writing action
      let config : AI.OpenRouterConfig := {
        apiKey := state.openRouterApiKey
        model := state.selectedModel
      }
      let apiMessages ← buildWritingActionMessages state action

      state := { state with
        isStreaming := true
        streamBuffer := ""
        cancelStreaming := false
        pendingAIWritingAction := none
      }

      -- Start streaming request
      let result ← AI.startStreamingCompletionSync config apiMessages
      match result with
      | .ok newSession =>
        session := some newSession
        state := state.setStatus s!"AI: {actionName}..."
      | .error msg =>
        state := { state with isStreaming := false, streamBuffer := "", insertAIResponseIntoEditor := false }
        state := state.setError s!"AI Error: {msg}"

  -- Poll active streaming session
  if let some activeSession := session then
    -- Check for cancellation
    if state.cancelStreaming then
      -- Cancel streaming
      let content ← activeSession.getContent
      if !content.isEmpty then
        let assistantMsg ← ChatMessage.create "assistant" (content ++ "\n\n[Cancelled]")
        state := state.addChatMessage assistantMsg
      session := none
      state := { state with isStreaming := false, streamBuffer := "", cancelStreaming := false }
      state := state.clearStatus
    else
      -- Poll for next chunk
      let chunk? ← activeSession.pollChunk
      match chunk? with
      | some chunk =>
        -- Update stream buffer with new content
        state := { state with streamBuffer := state.streamBuffer ++ chunk }
      | none =>
        -- Check if done
        let done ← activeSession.isDone
        if done then
          let content ← activeSession.getContent
          let assistantMsg ← ChatMessage.create "assistant" content
          state := state.addChatMessage assistantMsg
          session := none

          -- Insert into editor if this was a writing action that should insert
          if state.insertAIResponseIntoEditor then
            state := state.handleAIWritingResponse content
            state := state.setStatus "AI content added to editor"
          else
            state := state.clearStatus

          state := { state with isStreaming := false, streamBuffer := "", insertAIResponseIntoEditor := false }

  -- Save session state
  sessionRef.set session

  -- Poll for input
  let event ← Events.poll

  -- Extract key event if any
  let keyEvent := match event with
    | .key k => some k
    | _ => none

  -- Run update
  let (newState, shouldQuit) := update state keyEvent
  state ← processPendingActions newState

  -- Update app state
  let app := { app with state := state, shouldQuit := app.shouldQuit || shouldQuit }

  if app.shouldQuit then return

  -- Create frame and render
  let frame := Frame.new app.terminal.area
  let frame := drawFn frame app.state

  -- Update terminal buffer and flush
  let term := app.terminal.setBuffer frame.buffer
  let term ← term.flush frame.commands

  let app := { app with terminal := term }

  -- Shorter sleep when streaming for responsiveness
  if session.isSome then
    IO.sleep 8  -- ~120 FPS during streaming
  else
    IO.sleep 16  -- ~60 FPS normally

  runLoop app drawFn sessionRef

/-- Run the application with custom loop -/
def runAppWithIO (initialState : AppState) (drawFn : Frame → AppState → Frame) : IO Unit := do
  Terminal.setup
  try
    let app ← App.new initialState
    -- Initial draw
    let term ← app.terminal.draw
    let app := { app with terminal := term }
    -- Create session ref
    let sessionRef ← IO.mkRef none
    -- Run main loop
    runLoop app drawFn sessionRef
  finally
    Terminal.teardown

/-- Run the application -/
def run : IO Unit := do
  IO.println "Starting Enchiridion..."

  -- Load configuration from file or environment
  let config ← Config.findAndLoad

  -- Create initial state
  let mut initialState ← createSampleProject

  -- Apply configuration
  if config.openRouterApiKey.isEmpty then
    IO.eprintln "Warning: No API key configured. AI features will not work."
    IO.eprintln s!"Set OPENROUTER_API_KEY environment variable or create {Config.configFileName}"
  else
    initialState := { initialState with
      openRouterApiKey := config.openRouterApiKey
      selectedModel := config.defaultModel }

  -- Run the app with our custom IO-aware loop
  runAppWithIO initialState draw

end Enchiridion.UI
