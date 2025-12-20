/-
  Enchiridion UI Draw
  Main rendering functions
-/

import Terminus
import Enchiridion.State.AppState
import Enchiridion.UI.Layout

namespace Enchiridion.UI

open Terminus

/-- Get border style based on focus -/
def borderStyle (focused : Bool) : Style :=
  if focused then
    Style.fgColor Color.yellow
  else
    Style.default

/-- Draw the navigation panel (chapter/scene tree) -/
def drawNavigation (frame : Frame) (state : AppState) (area : Rect) (focused : Bool) : Frame :=
  let block := Block.rounded
    |>.withTitle "Chapters"
    |>.withBorderStyle (borderStyle focused)

  -- Render block
  let frame := frame.render block area
  let inner := block.innerArea area

  -- Build tree items from novel structure
  let novel := state.project.novel

  let lines := Id.run do
    let mut result : Array Line := #[]
    let mut chapterIdx := 0
    for chapter in novel.chapters do
      let isSelectedChapter := chapterIdx == state.selectedChapterIdx
      let chapterStyle := if isSelectedChapter && focused then
        Style.default.withBg Color.blue |>.withFg Color.white
      else
        Style.default

      let pfx := if state.navCollapsed.getD chapterIdx false then "▸ " else "▾ "
      result := result.push (Line.styled s!"{pfx}{chapter.title}" chapterStyle)

      -- Add scenes if not collapsed
      if !(state.navCollapsed.getD chapterIdx false) then
        let mut sceneIdx := 0
        for scene in chapter.scenes do
          let isSelectedScene := isSelectedChapter && sceneIdx == state.selectedSceneIdx
          let sceneStyle := if isSelectedScene && focused then
            Style.default.withBg Color.blue |>.withFg Color.white
          else
            Style.fgColor Color.gray

          result := result.push (Line.styled s!"  └ {scene.title}" sceneStyle)
          sceneIdx := sceneIdx + 1
      chapterIdx := chapterIdx + 1
    result

  let para := Paragraph.new lines.toList
  frame.render para inner

/-- Draw the editor panel -/
def drawEditor (frame : Frame) (state : AppState) (area : Rect) (focused : Bool) : Frame :=
  let title := state.getCurrentSceneTitle
  let block := Block.rounded
    |>.withTitle s!"Editor - {title}"
    |>.withBorderStyle (borderStyle focused)

  -- Configure text area with focus state
  let textArea := { state.editorTextArea with focused := focused }
    |>.withBlock block
    |>.showNumbers

  frame.render textArea area

/-- Format a single chat message -/
def formatChatMessage (msg : ChatMessage) : List Line :=
  let roleStyle := if msg.isUser then
    Style.fgColor Color.cyan |>.withModifier Modifier.mkBold
  else if msg.isAssistant then
    Style.fgColor Color.green |>.withModifier Modifier.mkBold
  else
    Style.fgColor Color.gray

  let roleName := if msg.isUser then "[You]" else if msg.isAssistant then "[AI]" else "[System]"

  let headerLine := Line.styled roleName roleStyle
  let contentLines := msg.content.splitOn "\n" |>.map Line.raw

  [headerLine] ++ contentLines ++ [Line.raw ""]

/-- Draw the chat panel -/
def drawChat (frame : Frame) (state : AppState) (area : Rect) (focused : Bool) : Frame :=
  let block := Block.rounded
    |>.withTitle (if state.isStreaming then "AI Chat (streaming...)" else "AI Chat")
    |>.withBorderStyle (borderStyle focused)

  let frame := frame.render block area
  let inner := block.innerArea area

  -- Split inner area: messages | input
  let sections := vsplit inner [.fill, .fixed 3]
  let messagesArea := sections[0]!
  let inputArea := sections[1]!

  -- Draw messages
  let allLines := (state.chatMessages.toList.map formatChatMessage).flatten

  -- If streaming, add the stream buffer
  let allLines := if state.isStreaming && !state.streamBuffer.isEmpty then
    let aiHeader := Line.styled "[AI]" (Style.fgColor Color.green |>.withModifier Modifier.mkBold)
    let bufferLines := state.streamBuffer.splitOn "\n" |>.map Line.raw
    allLines ++ [aiHeader] ++ bufferLines
  else
    allLines

  let msgPara := Paragraph.new allLines
  let frame := frame.render msgPara messagesArea

  -- Draw input field
  let inputBlock := Block.single.withTitle "Message"
  let input := { state.chatInput with focused := focused && !state.isStreaming }
    |>.withBlock inputBlock
  frame.render input inputArea

/-- Draw notes panel in list mode -/
def drawNotesListMode (frame : Frame) (state : AppState) (inner : Rect) (focused : Bool) : Frame :=
  -- Draw tabs for Characters / World
  let charTab := if state.notesTab == 0 then "[Characters]" else " Characters "
  let worldTab := if state.notesTab == 1 then "[World]" else " World "

  let tabSpans := [
    Span.styled charTab (if state.notesTab == 0 then Style.bold else Style.default),
    Span.raw " ",
    Span.styled worldTab (if state.notesTab == 1 then Style.bold else Style.default)
  ]

  let tabLine := Line.new tabSpans
  let tabPara := Paragraph.new [tabLine]
  let tabArea := { inner with height := 1 }
  let frame := frame.render tabPara tabArea

  -- Draw list based on selected tab
  let listArea := { inner with y := inner.y + 1, height := inner.height - 1 }

  let items := if state.notesTab == 0 then
    state.project.characters.map (·.name)
  else
    state.project.worldNotes.map (·.displayTitle)

  if items.isEmpty then
    let emptyMsg := if state.notesTab == 0 then "No characters (n=new)" else "No notes (n=new)"
    let para := Paragraph.fromString emptyMsg |>.withStyle (Style.fgColor Color.gray)
    frame.render para listArea
  else
    let selectedIdx := if state.notesTab == 0 then state.selectedCharacterIdx else state.selectedNoteIdx
    let lines := Id.run do
      let mut result : List Line := []
      let mut idx := 0
      for item in items do
        let style := if idx == selectedIdx && focused then
          Style.default.withBg Color.blue |>.withFg Color.white
        else
          Style.default
        result := result ++ [Line.styled item style]
        idx := idx + 1
      result
    let para := Paragraph.new lines
    frame.render para listArea

/-- Draw notes panel in edit mode -/
def drawNotesEditMode (frame : Frame) (state : AppState) (inner : Rect) (focused : Bool) : Frame :=
  let typeLabel := if state.notesTab == 0 then "Character" else "World Note"

  -- Split into sections: header, name input, content area
  let sections := vsplit inner [.fixed 1, .fixed 3, .fill]
  let headerArea := sections[0]!
  let nameArea := sections[1]!
  let contentArea := sections[2]!

  -- Draw header with instructions
  let headerText := s!"{typeLabel} - Tab: switch field | Ctrl+S: save | Esc: cancel"
  let headerPara := Paragraph.fromString headerText |>.withStyle (Style.fgColor Color.cyan)
  let frame := frame.render headerPara headerArea

  -- Draw name input
  let nameBlock := Block.single.withTitle "Name"
  let nameInput := { state.notesNameInput with focused := focused && state.notesEditField == 0 }
    |>.withBlock nameBlock
  let frame := frame.render nameInput nameArea

  -- Draw content area
  let contentBlock := Block.single.withTitle "Description"
  let contentTextArea := { state.notesContentArea with focused := focused && state.notesEditField == 1 }
    |>.withBlock contentBlock
  frame.render contentTextArea contentArea

/-- Draw the notes panel -/
def drawNotes (frame : Frame) (state : AppState) (area : Rect) (focused : Bool) : Frame :=
  let title := if state.notesEditMode then
    if state.notesTab == 0 then "Edit Character" else "Edit Note"
  else
    "Notes"

  let block := Block.rounded
    |>.withTitle title
    |>.withBorderStyle (borderStyle focused)

  let frame := frame.render block area
  let inner := block.innerArea area

  if state.notesEditMode then
    drawNotesEditMode frame state inner focused
  else
    drawNotesListMode frame state inner focused

/-- Draw the status bar -/
def drawStatus (frame : Frame) (state : AppState) (area : Rect) : Frame :=
  -- Check for error or status messages first
  match state.errorMessage with
  | some err =>
    let errStyle := Style.default.withBg Color.red |>.withFg Color.white
    let fillLine := String.ofList (List.replicate area.width ' ')
    let frame := frame.writeString area.x area.y fillLine errStyle
    let errMsg := s!" Error: {err.take (area.width - 10)}"
    frame.writeString area.x area.y errMsg errStyle
  | none =>
    match state.statusMessage with
    | some status =>
      let statusStyle := Style.default.withBg Color.blue |>.withFg Color.white
      let fillLine := String.ofList (List.replicate area.width ' ')
      let frame := frame.writeString area.x area.y fillLine statusStyle
      let statusMsg := s!" {status.take (area.width - 4)}"
      frame.writeString area.x area.y statusMsg statusStyle
    | none =>
      -- Normal status bar
      let novel := state.project.novel
      let wordCount := state.project.totalWordCount
      let sceneInfo := state.getCurrentSceneTitle
      let dirtyIndicator := if state.project.isDirty then " [*]" else ""

      let leftInfo := s!" {novel.title}{dirtyIndicator} | {sceneInfo}"
      let rightInfo := s!"Words: {wordCount} | {state.focus} "

      let bgStyle := Style.default.withBg Color.gray |>.withFg Color.white

      -- Fill with background color
      let fillLine := String.ofList (List.replicate area.width ' ')
      let frame := frame.writeString area.x area.y fillLine bgStyle

      -- Write left info
      let frame := frame.writeString area.x area.y leftInfo bgStyle

      -- Calculate right position and write right info
      let rightX := if area.width > rightInfo.length then area.x + area.width - rightInfo.length else area.x
      frame.writeString rightX area.y rightInfo bgStyle

/-- Draw the help overlay -/
def drawHelp (frame : Frame) (area : Rect) : Frame := Id.run do
  -- Calculate centered popup area
  let popupWidth := Nat.min 60 (area.width - 4)
  let popupHeight := Nat.min 28 (area.height - 4)
  let popupX := area.x + (area.width - popupWidth) / 2
  let popupY := area.y + (area.height - popupHeight) / 2
  let popupArea : Rect := { x := popupX, y := popupY, width := popupWidth, height := popupHeight }

  -- Draw background overlay (dim the background by filling with spaces)
  let dimStyle := Style.dim
  let dimRow := String.ofList (List.replicate area.width ' ')
  let mut result := frame
  for row in [:area.height] do
    result := result.writeString area.x (area.y + row) dimRow dimStyle

  -- Draw popup border
  let block := Block.rounded
    |>.withTitle "Keyboard Shortcuts (? to close)"
    |>.withBorderStyle (Style.fgColor Color.cyan)
  result := result.render block popupArea
  let inner := block.innerArea popupArea

  -- Define shortcuts
  let shortcuts : List (String × String) := [
    ("General", ""),
    ("  Tab / Shift+Tab", "Switch panels"),
    ("  Ctrl+S", "Save project"),
    ("  Ctrl+E", "Export to markdown"),
    ("  Ctrl+Q", "Quit (with confirmation)"),
    ("  ?", "Show/hide this help"),
    ("", ""),
    ("Navigation Panel", ""),
    ("  Up/Down", "Navigate chapters/scenes"),
    ("  Enter", "Load selected scene"),
    ("  Space", "Toggle chapter collapse"),
    ("  Ctrl+N", "New chapter"),
    ("  Ctrl+Shift+N", "New scene"),
    ("  Delete", "Delete chapter/scene"),
    ("", ""),
    ("Editor Panel (AI Writing)", ""),
    ("  Ctrl+Enter", "Continue writing"),
    ("  Ctrl+R", "Rewrite scene"),
    ("  Ctrl+B", "Brainstorm ideas"),
    ("  Ctrl+D", "Add dialogue"),
    ("  Ctrl+G", "Add description"),
    ("", ""),
    ("Notes Panel", ""),
    ("  Left/Right", "Switch tabs"),
    ("  n", "New character/note"),
    ("  Enter", "Edit selected"),
    ("  Tab", "Switch fields (edit mode)"),
    ("  Escape", "Cancel editing")
  ]

  -- Render shortcuts
  let mut y := inner.y
  for (key, desc) in shortcuts do
    if y < inner.y + inner.height then
      if desc.isEmpty then
        -- Section header
        let headerStyle := Style.bold.withFg Color.yellow
        result := result.writeString inner.x y key headerStyle
      else
        -- Shortcut line
        let keyStyle := Style.fgColor Color.cyan
        let descStyle := Style.default
        result := result.writeString inner.x y key keyStyle
        let descX := inner.x + 22
        result := result.writeString descX y desc descStyle
    y := y + 1

  return result

/-- Main draw function -/
def draw (frame : Frame) (state : AppState) : Frame :=
  let areas := layoutPanels frame.area

  -- Draw all panels
  let frame := drawNavigation frame state areas.navigation (state.focus == .navigation)
  let frame := drawEditor frame state areas.editor (state.focus == .editor)
  let frame := drawChat frame state areas.chat (state.focus == .chat)
  let frame := drawNotes frame state areas.notes (state.focus == .notes)
  let frame := drawStatus frame state areas.status

  -- Draw help overlay if in help mode
  let frame := if state.mode == .help then
    drawHelp frame frame.area
  else
    frame

  frame

end Enchiridion.UI
