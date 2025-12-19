/-
  Enchiridion JSON Serialization
  JSON encoding/decoding for all model types
-/

import Lean.Data.Json
import Enchiridion.Core.Types

namespace Enchiridion

open Lean Json

-- EntityId JSON instances
instance : ToJson EntityId where
  toJson id := Json.str id.value

instance : FromJson EntityId where
  fromJson? json := do
    let s ← json.getStr?
    return { value := s }

-- Timestamp JSON instances
instance : ToJson Timestamp where
  toJson ts := Json.num ts.unixMs.toNat

instance : FromJson Timestamp where
  fromJson? json := do
    let n ← json.getNat?
    return { unixMs := n.toUInt64 }

end Enchiridion
