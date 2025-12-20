/-
  Enchiridion AI Streaming
  SSE streaming for AI completions
-/

import Lean.Data.Json
import Wisp
import Enchiridion.AI.OpenRouter

namespace Enchiridion.AI

open Lean Json
open Wisp

/-- Build streaming request JSON for OpenRouter API -/
def buildStreamingRequestJson (config : OpenRouterConfig) (messages : Array APIMessage) : String :=
  let msgArray := Json.arr (messages.map APIMessage.toJson)
  let tempJson := toJson config.temperature
  let reqObj := Json.mkObj [
    ("model", Json.str config.model),
    ("messages", msgArray),
    ("max_tokens", Json.num config.maxTokens),
    ("temperature", tempJson),
    ("stream", Json.bool true)  -- Enable streaming
  ]
  reqObj.compress

/-- Create a streaming chat completion request -/
def createStreamingRequest (config : OpenRouterConfig) (messages : Array APIMessage) : Request :=
  Request.post "https://openrouter.ai/api/v1/chat/completions"
    |>.withBearerToken config.apiKey
    |>.withJson (buildStreamingRequestJson config messages)
    |>.withHeader "HTTP-Referer" config.siteUrl
    |>.withHeader "X-Title" config.siteName

/-- Parse a single SSE data chunk for content delta
    OpenAI format: {"choices":[{"delta":{"content":"text"}}]}
-/
def parseStreamDelta (data : String) : Option String := do
  -- Check for [DONE] marker
  if data.trim == "[DONE]" then
    none
  else
    let json ← Json.parse data |>.toOption
    let choices ← json.getObjVal? "choices" |>.toOption
    let choicesArr ← choices.getArr?.toOption
    if h : 0 < choicesArr.size then
      let firstChoice := choicesArr[0]
      let delta ← firstChoice.getObjVal? "delta" |>.toOption
      -- Content may not exist in every delta (e.g., first one has role)
      match delta.getObjValAs? String "content" with
      | .ok content => some content
      | .error _ => some ""  -- No content in this delta, return empty
    else
      none

/-- State for an active streaming session -/
structure StreamingSession where
  /-- The SSE stream -/
  stream : HTTP.SSE.Stream
  /-- Accumulated content so far -/
  contentRef : IO.Ref String
  /-- Whether streaming is complete -/
  doneRef : IO.Ref Bool
  /-- Error message if any -/
  errorRef : IO.Ref (Option String)

namespace StreamingSession

/-- Create a new streaming session from a streaming response -/
def create (resp : StreamingResponse) : IO StreamingSession := do
  let stream ← HTTP.SSE.Stream.fromStreaming resp
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

/-- Poll for next chunk (non-blocking via tryRecv would be ideal, but we use recv)
    Returns the new content chunk if any -/
def pollChunk (s : StreamingSession) : IO (Option String) := do
  let done ← s.doneRef.get
  if done then
    return none
  else
    -- Try to receive next event
    let event? ← s.stream.recv
    match event? with
    | none =>
      -- Stream ended
      s.doneRef.set true
      return none
    | some event =>
      -- Parse the delta
      match parseStreamDelta event.data with
      | none =>
        -- [DONE] or parse error, mark as complete
        s.doneRef.set true
        return none
      | some chunk =>
        -- Append to accumulated content
        let current ← s.contentRef.get
        s.contentRef.set (current ++ chunk)
        return some chunk

end StreamingSession

/-- Start a streaming completion request synchronously (for simplicity)
    This will block until headers are received, then return the session -/
def startStreamingCompletionSync (config : OpenRouterConfig) (messages : Array APIMessage) :
    IO (Except String StreamingSession) := do
  let client := HTTP.Client.new
  let req := createStreamingRequest config messages

  -- Execute streaming request
  let task ← client.executeStreaming req

  -- Wait for the task to complete
  let result := task.get
  match result with
  | .ok resp =>
    if resp.status == 200 then
      let session ← StreamingSession.create resp
      return Except.ok session
    else
      -- Read error body
      let body ← resp.readAllBody
      let bodyStr := String.fromUTF8! body
      return Except.error (parseErrorMessage bodyStr)
  | .error e =>
    return Except.error s!"Request failed: {e}"

/-- Convenience function to run streaming with a callback for each chunk -/
partial def streamCompletion (config : OpenRouterConfig) (messages : Array APIMessage)
    (onChunk : String → IO Unit) (onDone : String → IO Unit) (onError : String → IO Unit) : IO Unit := do
  let result ← startStreamingCompletionSync config messages
  match result with
  | .ok session =>
    let rec loop : IO Unit := do
      let chunk? ← session.pollChunk
      match chunk? with
      | some chunk =>
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
