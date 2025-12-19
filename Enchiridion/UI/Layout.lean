/-
  Enchiridion UI Layout
  Panel layout and area calculations
-/

import Terminus

namespace Enchiridion.UI

open Terminus

/-- Layout configuration for panel sizes -/
structure LayoutConfig where
  leftPanelWidth : Constraint := .percent 25
  rightPanelWidth : Constraint := .percent 30
  navHeight : Constraint := .percent 35
  chatHeight : Constraint := .percent 60
  statusHeight : Constraint := .fixed 1

/-- Default layout configuration -/
def defaultLayout : LayoutConfig := {
  leftPanelWidth := .percent 25
  rightPanelWidth := .percent 30
  navHeight := .percent 35
  chatHeight := .percent 60
  statusHeight := .fixed 1
}

/-- Computed panel areas for rendering -/
structure PanelAreas where
  navigation : Rect
  editor : Rect
  chat : Rect
  notes : Rect
  status : Rect
  deriving Inhabited

/-- Calculate panel areas from terminal size -/
def layoutPanels (area : Rect) (config : LayoutConfig := defaultLayout) : PanelAreas :=
  -- First split: main content | status bar
  let mainSplit := vsplit area [.fill, config.statusHeight]
  let mainArea := mainSplit[0]!
  let statusArea := mainSplit[1]!

  -- Main horizontal split: left | center | right
  let hSplit := hsplit mainArea [config.leftPanelWidth, .fill, config.rightPanelWidth]
  let leftArea := hSplit[0]!
  let centerArea := hSplit[1]!
  let rightArea := hSplit[2]!

  -- Left panel: navigation (top) - full left area for now
  let navigationArea := leftArea

  -- Center: editor takes full center area
  let editorArea := centerArea

  -- Right panel: chat (top) | notes (bottom)
  let rightSplit := vsplit rightArea [config.chatHeight, .fill]
  let chatArea := rightSplit[0]!
  let notesArea := rightSplit[1]!

  {
    navigation := navigationArea
    editor := editorArea
    chat := chatArea
    notes := notesArea
    status := statusArea
  }

end Enchiridion.UI
