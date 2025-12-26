/-
  Enchiridion AI Streaming
  SSE streaming for AI completions using Oracle library
-/

import Oracle
import Enchiridion.AI.OpenRouter
import Enchiridion.State.AppState

namespace Enchiridion.AI

open Oracle

/-- State for an active streaming session -/
structure StreamingSession where
  /-- The Oracle chat stream -/
  stream : Oracle.ChatStream
  /-- Accumulated content so far -/
  contentRef : IO.Ref String
  /-- Whether streaming is complete -/
  doneRef : IO.Ref Bool
  /-- Error message if any -/
  errorRef : IO.Ref (Option String)

namespace StreamingSession

/-- Create a new streaming session from an Oracle chat stream -/
def create (stream : Oracle.ChatStream) : IO StreamingSession := do
  let contentRef ← IO.mkRef ""
  let doneRef ← IO.mkRef false
  let errorRef ← IO.mkRef none
  return { stream, contentRef, doneRef, errorRef }

/-- Get current accumulated content -/
def getContent (s : StreamingSession) : IO String :=
  s.contentRef.get

/-- Check if streaming is done -/
def isDone (s : StreamingSession) : IO Bool :=
  s.doneRef.get

/-- Get error if any -/
def getError (s : StreamingSession) : IO (Option String) :=
  s.errorRef.get

/-- Poll for next chunk
    Returns the new content chunk if any -/
def pollChunk (s : StreamingSession) : IO (Option String) := do
  let done ← s.doneRef.get
  if done then
    return none
  else
    -- Try to receive next chunk
    let chunk? ← s.stream.recv
    match chunk? with
    | none =>
      -- Stream ended
      s.doneRef.set true
      return none
    | some chunk =>
      match chunk.content with
      | none =>
        -- No content in this chunk (e.g., role-only delta), continue
        return some ""
      | some content =>
        -- Append to accumulated content
        let current ← s.contentRef.get
        s.contentRef.set (current ++ content)
        return some content

end StreamingSession

/-- Start a streaming completion request
    Returns the streaming session or an error -/
def startStreamingCompletionSync (config : OpenRouterConfig) (messages : Array ChatMessage) :
    IO (Except String StreamingSession) := do
  let client := createClient config
  let oracleMessages := messages.map chatMessageToOracleMessage
  let opts : Oracle.ChatOptions := {
    temperature := some config.temperature
    maxTokens := some config.maxTokens
  }

  match ← client.completeStream oracleMessages opts with
  | .ok stream =>
    let session ← StreamingSession.create stream
    return Except.ok session
  | .error e =>
    return Except.error (toString e)

/-- Convenience function to run streaming with a callback for each chunk -/
partial def streamCompletion (config : OpenRouterConfig) (messages : Array ChatMessage)
    (onChunk : String → IO Unit) (onDone : String → IO Unit) (onError : String → IO Unit) : IO Unit := do
  let result ← startStreamingCompletionSync config messages
  match result with
  | .ok session =>
    let rec loop : IO Unit := do
      let chunk? ← session.pollChunk
      match chunk? with
      | some chunk =>
        if !chunk.isEmpty then
          onChunk chunk
        loop
      | none =>
        let done ← session.isDone
        if done then
          let content ← session.getContent
          onDone content
    loop
  | .error msg =>
    onError msg

end Enchiridion.AI
