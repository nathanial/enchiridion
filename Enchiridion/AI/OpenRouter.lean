/-
  Enchiridion OpenRouter API Client
  Uses the Oracle library for OpenRouter API interactions
-/

import Oracle
import Enchiridion.State.AppState

namespace Enchiridion.AI

open Oracle

/-- OpenRouter API configuration for Enchiridion -/
structure OpenRouterConfig where
  apiKey : String
  model : String := "anthropic/claude-3.5-sonnet"
  siteUrl : String := "https://github.com/enchiridion"
  siteName : String := "Enchiridion"
  maxTokens : Nat := 4096
  temperature : Float := 0.7
  deriving Repr, Inhabited

/-- Convert Enchiridion ChatMessage to Oracle Message -/
def chatMessageToOracleMessage (msg : ChatMessage) : Oracle.Message :=
  let role := match msg.role with
    | "system" => Oracle.Role.system
    | "assistant" => Oracle.Role.assistant
    | _ => Oracle.Role.user
  { role := role, content := .string msg.content }

/-- Result of an API call -/
inductive APIResult where
  | ok (content : String)
  | error (message : String)
  deriving Repr

/-- Create an Oracle client from Enchiridion config -/
def createClient (config : OpenRouterConfig) : Oracle.Client :=
  let oracleConfig : Oracle.Config := {
    apiKey := config.apiKey
    model := config.model
    siteUrl := some config.siteUrl
    siteName := some config.siteName
  }
  Oracle.Client.new oracleConfig

/-- Execute a chat completion request (non-streaming) -/
def sendChatCompletion (config : OpenRouterConfig) (messages : Array ChatMessage) : IO APIResult := do
  let client := createClient config
  let oracleMessages := messages.map chatMessageToOracleMessage
  let opts : Oracle.ChatOptions := {
    temperature := some config.temperature
    maxTokens := some config.maxTokens
  }

  match ← client.complete oracleMessages opts with
  | .ok content => return .ok content
  | .error e => return .error (toString e)

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
