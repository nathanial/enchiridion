/-
  Enchiridion OpenRouter API Client
  HTTP client for OpenRouter AI API
-/

import Lean.Data.Json
import Wisp
import Enchiridion.State.AppState

namespace Enchiridion.AI

open Lean Json
open Wisp

/-- OpenRouter API configuration -/
structure OpenRouterConfig where
  apiKey : String
  model : String := "anthropic/claude-3.5-sonnet"
  siteUrl : String := "https://github.com/enchiridion"
  siteName : String := "Enchiridion"
  maxTokens : Nat := 4096
  temperature : Float := 0.7
  deriving Repr, Inhabited

/-- Chat message role -/
inductive MessageRole where
  | system
  | user
  | assistant
  deriving Repr, BEq, Inhabited

namespace MessageRole

def toString : MessageRole → String
  | .system => "system"
  | .user => "user"
  | .assistant => "assistant"

instance : ToString MessageRole where
  toString := MessageRole.toString

end MessageRole

/-- API chat message format -/
structure APIMessage where
  role : MessageRole
  content : String
  deriving Repr, Inhabited

namespace APIMessage

def toJson (msg : APIMessage) : Json :=
  Json.mkObj [
    ("role", Json.str msg.role.toString),
    ("content", Json.str msg.content)
  ]

end APIMessage

/-- Convert app ChatMessage to API format -/
def chatMessageToAPI (msg : ChatMessage) : APIMessage :=
  let role := match msg.role with
    | "system" => MessageRole.system
    | "assistant" => MessageRole.assistant
    | _ => MessageRole.user
  { role := role, content := msg.content }

/-- Build the request JSON for OpenRouter API -/
def buildRequestJson (config : OpenRouterConfig) (messages : Array APIMessage) : String :=
  let msgArray := Json.arr (messages.map APIMessage.toJson)
  -- Use built-in ToJson Float instance
  let tempJson := toJson config.temperature
  let reqObj := Json.mkObj [
    ("model", Json.str config.model),
    ("messages", msgArray),
    ("max_tokens", Json.num config.maxTokens),
    ("temperature", tempJson),
    ("stream", Json.bool false)
  ]
  reqObj.compress

/-- Create an OpenRouter chat completion request -/
def createChatRequest (config : OpenRouterConfig) (messages : Array APIMessage) : Request :=
  Request.post "https://openrouter.ai/api/v1/chat/completions"
    |>.withBearerToken config.apiKey
    |>.withJson (buildRequestJson config messages)
    |>.withHeader "HTTP-Referer" config.siteUrl
    |>.withHeader "X-Title" config.siteName

/-- Parse the content from an OpenRouter response -/
def parseResponseContent (body : String) : Option String := do
  let json ← Json.parse body |>.toOption
  let choices ← json.getObjVal? "choices" |>.toOption
  let choicesArr ← choices.getArr?.toOption
  if h : 0 < choicesArr.size then
    let firstChoice := choicesArr[0]
    let message ← firstChoice.getObjVal? "message" |>.toOption
    let content ← message.getObjValAs? String "content" |>.toOption
    return content
  else
    none

/-- Parse error message from OpenRouter response -/
def parseErrorMessage (body : String) : String :=
  match Json.parse body with
  | .ok json =>
    match json.getObjVal? "error" with
    | .ok errObj =>
      match errObj.getObjValAs? String "message" with
      | .ok msg => msg
      | .error _ => "Unknown API error"
    | .error _ => body
  | .error _ => body

/-- Result of an API call -/
inductive APIResult where
  | ok (content : String)
  | error (message : String)
  deriving Repr

/-- Execute a chat completion request (non-streaming) -/
def sendChatCompletion (config : OpenRouterConfig) (messages : Array APIMessage) : IO APIResult := do
  let client := HTTP.Client.new
  let req := createChatRequest config messages
  let task ← client.execute req
  let result := task.get

  match result with
  | .ok response =>
    let bodyStr := String.fromUTF8! response.body
    if response.status == 200 then
      match parseResponseContent bodyStr with
      | some content => return .ok content
      | none => return .error s!"Failed to parse response: {bodyStr}"
    else
      return .error (parseErrorMessage bodyStr)
  | .error e =>
    return .error s!"Request failed: {e}"

/-- Available models on OpenRouter -/
def availableModels : Array (String × String) := #[
  ("anthropic/claude-3.5-sonnet", "Claude 3.5 Sonnet"),
  ("anthropic/claude-3-opus", "Claude 3 Opus"),
  ("openai/gpt-4-turbo", "GPT-4 Turbo"),
  ("openai/gpt-4o", "GPT-4o"),
  ("google/gemini-pro-1.5", "Gemini Pro 1.5"),
  ("meta-llama/llama-3.1-70b-instruct", "Llama 3.1 70B")
]

end Enchiridion.AI
