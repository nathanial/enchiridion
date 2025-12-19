/-
  Enchiridion Core Types
  Basic types used throughout the application
-/

namespace Enchiridion

/-- Unique identifier for entities (chapters, scenes, characters, etc.) -/
structure EntityId where
  value : String
  deriving Repr, BEq, Hashable, Inhabited, DecidableEq

namespace EntityId

/-- Generate a simple unique ID based on current time and random component -/
def generate : IO EntityId := do
  let time ← IO.monoNanosNow
  let rand ← IO.rand 0 999999
  return { value := s!"{time}-{rand}" }

/-- Create an EntityId from a string -/
def fromString (s : String) : EntityId := { value := s }

/-- Convert to string -/
def toString (id : EntityId) : String := id.value

instance : ToString EntityId where
  toString := EntityId.toString

end EntityId

/-- Timestamp in milliseconds since Unix epoch -/
structure Timestamp where
  unixMs : UInt64
  deriving Repr, BEq, Inhabited

namespace Timestamp

/-- Get current timestamp -/
def now : IO Timestamp := do
  let nanos ← IO.monoNanosNow
  -- Convert nanoseconds to milliseconds
  return { unixMs := (nanos / 1000000).toUInt64 }

/-- Zero timestamp -/
def zero : Timestamp := { unixMs := 0 }

instance : Ord Timestamp where
  compare a b := compare a.unixMs b.unixMs

instance : ToString Timestamp where
  toString t := s!"{t.unixMs}ms"

end Timestamp

end Enchiridion
