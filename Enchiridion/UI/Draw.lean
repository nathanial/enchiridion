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

/-- Draw the notes panel -/
def drawNotes (frame : Frame) (state : AppState) (area : Rect) (focused : Bool) : Frame :=
  let block := Block.rounded
    |>.withTitle "Notes"
    |>.withBorderStyle (borderStyle focused)

  let frame := frame.render block area
  let inner := block.innerArea area

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
    let emptyMsg := if state.notesTab == 0 then "No characters yet" else "No notes yet"
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

/-- Draw the status bar -/
def drawStatus (frame : Frame) (state : AppState) (area : Rect) : Frame :=
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

/-- Main draw function -/
def draw (frame : Frame) (state : AppState) : Frame :=
  let areas := layoutPanels frame.area

  -- Draw all panels
  let frame := drawNavigation frame state areas.navigation (state.focus == .navigation)
  let frame := drawEditor frame state areas.editor (state.focus == .editor)
  let frame := drawChat frame state areas.chat (state.focus == .chat)
  let frame := drawNotes frame state areas.notes (state.focus == .notes)
  let frame := drawStatus frame state areas.status

  frame

end Enchiridion.UI
