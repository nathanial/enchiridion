/-
  Enchiridion UI App
  Main application loop
-/

import Terminus
import Enchiridion.State.AppState
import Enchiridion.UI.Draw
import Enchiridion.UI.Update

namespace Enchiridion.UI

open Terminus

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

/-- Run the application -/
def run : IO Unit := do
  IO.println "Starting Enchiridion..."

  -- Create initial state
  let initialState ← createSampleProject

  -- Run the app using Terminus App framework
  App.runApp initialState draw update

end Enchiridion.UI
