/-
  Enchiridion AI Prompts
  System prompts and templates for writing assistance
-/

import Enchiridion.Model.Novel
import Enchiridion.Model.Character
import Enchiridion.Model.WorldNote
import Enchiridion.Model.Project
import Enchiridion.State.AppState

namespace Enchiridion.AI

open Enchiridion

/-- Base system prompt for novel writing assistance -/
def systemPrompt : String :=
"You are an expert creative writing assistant helping an author write their novel. You should:

1. Match the author's writing style and tone based on their existing content
2. Maintain consistency with established characters, plot points, and world-building
3. Provide creative and engaging prose that advances the story
4. Respect the author's creative vision while offering improvements when asked
5. Keep responses focused and relevant to the current scene or chapter

When continuing a story, write in the same narrative voice and tense as the provided text.
When asked for suggestions, provide concrete, actionable ideas.
When asked to rewrite, preserve the core meaning while improving the prose."

/-- Build context about the novel for the AI -/
def buildNovelContext (novel : Novel) : String :=
  let parts := #[
    s!"Novel: {novel.title}",
    if novel.author.isEmpty then "" else s!"Author: {novel.author}",
    if novel.genre.isEmpty then "" else s!"Genre: {novel.genre}",
    if novel.synopsis.isEmpty then "" else s!"Synopsis: {novel.synopsis}"
  ]
  "\n".intercalate (parts.filter (·.length > 0)).toList

/-- Build context about a character -/
def buildCharacterContext (char : Character) : String :=
  let parts := #[
    s!"- {char.name}",
    if char.description.isEmpty then "" else s!"  Description: {char.description}",
    if char.traits.isEmpty then "" else s!"  Traits: {", ".intercalate char.traits.toList}"
  ]
  "\n".intercalate (parts.filter (·.length > 0)).toList

/-- Build context about characters -/
def buildCharactersContext (characters : Array Character) : String :=
  if characters.isEmpty then ""
  else
    let charDescs := characters.map buildCharacterContext
    "Characters:\n" ++ "\n".intercalate charDescs.toList

/-- Build context about world notes -/
def buildWorldContext (notes : Array WorldNote) : String :=
  if notes.isEmpty then ""
  else
    let noteDescs := notes.map fun n =>
      s!"- [{n.category}] {n.title}: {n.content.take 200}"
    "World Notes:\n" ++ "\n".intercalate noteDescs.toList

/-- Build full context for the AI from a project -/
def buildProjectContext (project : Project) : String :=
  let parts := #[
    buildNovelContext project.novel,
    buildCharactersContext project.characters,
    buildWorldContext project.worldNotes
  ]
  "\n\n".intercalate (parts.filter (·.length > 0)).toList

/-- Build context for the current scene -/
def buildSceneContext (chapter : Chapter) (scene : Scene) : String :=
  let parts := #[
    s!"Current Chapter: {chapter.title}",
    if chapter.synopsis.isEmpty then "" else s!"Chapter Synopsis: {chapter.synopsis}",
    s!"Current Scene: {scene.title}",
    if scene.synopsis.isEmpty then "" else s!"Scene Synopsis: {scene.synopsis}"
  ]
  "\n".intercalate (parts.filter (·.length > 0)).toList

/-- Prompt types for different writing actions (legacy, use AIWritingAction instead) -/
inductive PromptType where
  | continue     -- Continue writing from current position
  | rewrite      -- Rewrite selected text
  | brainstorm   -- Generate ideas
  | dialogue     -- Help with dialogue
  | description  -- Help with descriptions
  | custom       -- User's custom prompt
  deriving Repr, BEq, Inhabited

namespace PromptType

def toString : PromptType → String
  | .continue => "continue"
  | .rewrite => "rewrite"
  | .brainstorm => "brainstorm"
  | .dialogue => "dialogue"
  | .description => "description"
  | .custom => "custom"

instance : ToString PromptType where
  toString := PromptType.toString

/-- Get instruction text for each prompt type -/
def instruction : PromptType → String
  | .continue => "Continue the story from where it left off, maintaining the same style and tone. Write 2-3 paragraphs."
  | .rewrite => "Rewrite the following text, improving the prose while keeping the same meaning and events."
  | .brainstorm => "Suggest 3-5 creative ideas for what could happen next in this scene. Be specific and consider the established characters and plot."
  | .dialogue => "Write natural, character-appropriate dialogue for this scene. Include dialogue tags and brief action beats."
  | .description => "Write a vivid description that engages the senses and sets the mood. Be specific and evocative."
  | .custom => ""

end PromptType

/-- Build a full prompt for the AI -/
def buildPrompt (promptType : PromptType) (project : Project) (currentContent : String)
    (chapter : Option Chapter) (scene : Option Scene) (userMessage : String) : String :=
  let projectCtx := buildProjectContext project
  let sceneCtx := match chapter, scene with
    | some ch, some sc => buildSceneContext ch sc
    | _, _ => ""

  let contentSection := if currentContent.isEmpty then ""
    else s!"Current scene content:\n---\n{currentContent}\n---"

  let instruction := match promptType with
    | .custom => userMessage
    | other => s!"{other.instruction}\n\n{userMessage}"

  let parts := #[
    projectCtx,
    sceneCtx,
    contentSection,
    instruction
  ]
  "\n\n".intercalate (parts.filter (·.length > 0)).toList

/-- Build a prompt for an AI writing action -/
def buildWritingActionPrompt (action : AIWritingAction) (project : Project) (currentContent : String)
    (chapter : Option Chapter) (scene : Option Scene) : String :=
  let projectCtx := buildProjectContext project
  let sceneCtx := match chapter, scene with
    | some ch, some sc => buildSceneContext ch sc
    | _, _ => ""

  let contentSection := if currentContent.isEmpty then ""
    else s!"Current scene content:\n---\n{currentContent}\n---"

  let instruction := action.instruction

  let parts := #[
    projectCtx,
    sceneCtx,
    contentSection,
    instruction
  ]
  "\n\n".intercalate (parts.filter (·.length > 0)).toList

/-- Quick prompts for common actions -/
def quickPrompts : Array (String × PromptType × String) := #[
  ("Continue", .continue, "Continue writing from here."),
  ("Add Dialogue", .dialogue, "Add a dialogue exchange between the characters."),
  ("Describe Setting", .description, "Describe the current setting in more detail."),
  ("What If?", .brainstorm, "What interesting things could happen next?")
]

/-- Available AI writing actions with their keyboard shortcuts -/
def writingActionShortcuts : Array (AIWritingAction × String) := #[
  (.continue_, "Ctrl+Enter"),
  (.rewrite, "Ctrl+R"),
  (.brainstorm, "Ctrl+B"),
  (.dialogue, "Ctrl+D"),
  (.description, "Ctrl+G")
]

end Enchiridion.AI
