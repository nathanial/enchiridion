/-
  Enchiridion UI App
  Main application loop
-/

import Terminus
import Enchiridion.State.AppState
import Enchiridion.Model.Novel
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

  -- Clear the pending flags
  state := state.clearPendingActions
  return state

/-- Custom update wrapper that handles IO actions -/
def updateWithIO (state : AppState) (keyEvent : Option KeyEvent) : IO (AppState × Bool) := do
  -- First run the pure update
  let (state, shouldQuit) := update state keyEvent

  -- Then process any pending IO actions
  let state ← processPendingActions state

  return (state, shouldQuit)

/-- Custom app loop with IO action support -/
partial def runLoop (app : App AppState) (drawFn : Frame → AppState → Frame) : IO Unit := do
  if app.shouldQuit then return

  -- Poll for input
  let event ← Events.poll

  -- Extract key event if any
  let keyEvent := match event with
    | .key k => some k
    | _ => none

  -- Run update with IO support
  let (newState, shouldQuit) ← updateWithIO app.state keyEvent

  -- Update app state
  let app := { app with state := newState, shouldQuit := app.shouldQuit || shouldQuit }

  if app.shouldQuit then return

  -- Create frame and render
  let frame := Frame.new app.terminal.area
  let frame := drawFn frame app.state

  -- Update terminal buffer and flush
  let term := app.terminal.setBuffer frame.buffer
  let term ← term.flush frame.commands

  let app := { app with terminal := term }

  IO.sleep 16  -- ~60 FPS
  runLoop app drawFn

/-- Run the application with custom loop -/
def runAppWithIO (initialState : AppState) (drawFn : Frame → AppState → Frame) : IO Unit := do
  Terminal.setup
  try
    let app ← App.new initialState
    -- Initial draw
    let term ← app.terminal.draw
    let app := { app with terminal := term }
    -- Run main loop
    runLoop app drawFn
  finally
    Terminal.teardown

/-- Run the application -/
def run : IO Unit := do
  IO.println "Starting Enchiridion..."

  -- Create initial state
  let initialState ← createSampleProject

  -- Run the app with our custom IO-aware loop
  runAppWithIO initialState draw

end Enchiridion.UI
