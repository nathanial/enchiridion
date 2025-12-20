/-
  Enchiridion Focus Management
  Panel focus state and transitions
-/

namespace Enchiridion

/-- Which panel currently has focus -/
inductive PanelFocus where
  | navigation  -- Chapter/scene tree
  | editor      -- Main text editor
  | chat        -- AI chat panel
  | notes       -- Character/world notes
  deriving Repr, BEq, Inhabited, DecidableEq

namespace PanelFocus

/-- Get display name for panel -/
def toString : PanelFocus → String
  | .navigation => "Navigation"
  | .editor => "Editor"
  | .chat => "Chat"
  | .notes => "Notes"

instance : ToString PanelFocus where
  toString := PanelFocus.toString

/-- Cycle to next panel (clockwise) -/
def next : PanelFocus → PanelFocus
  | .navigation => .editor
  | .editor => .chat
  | .chat => .notes
  | .notes => .navigation

/-- Cycle to previous panel (counter-clockwise) -/
def prev : PanelFocus → PanelFocus
  | .navigation => .notes
  | .editor => .navigation
  | .chat => .editor
  | .notes => .chat

end PanelFocus

/-- Application mode -/
inductive AppMode where
  | normal       -- Normal editing mode
  | aiStreaming  -- AI is streaming a response
  | saving       -- Save dialog open
  | loading      -- Load dialog open
  | command      -- Command palette open
  | confirm      -- Confirmation dialog
  | help         -- Help overlay showing shortcuts
  deriving Repr, BEq, Inhabited, DecidableEq

namespace AppMode

def toString : AppMode → String
  | .normal => "Normal"
  | .aiStreaming => "AI Streaming..."
  | .saving => "Saving..."
  | .loading => "Loading..."
  | .command => "Command"
  | .confirm => "Confirm"
  | .help => "Help"

instance : ToString AppMode where
  toString := AppMode.toString

/-- Check if mode allows panel switching -/
def allowsPanelSwitch : AppMode → Bool
  | .normal => true
  | _ => false

/-- Check if mode allows editing -/
def allowsEditing : AppMode → Bool
  | .normal => true
  | _ => false

end AppMode

end Enchiridion
