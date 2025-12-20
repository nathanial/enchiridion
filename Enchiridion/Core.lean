/-
  Enchiridion Core Module
  Re-exports core types and utilities
-/

import Enchiridion.Core.Types
import Enchiridion.Core.Json
import Enchiridion.Core.Config

namespace Enchiridion.Core

-- Re-export common types at the Core namespace level
export Enchiridion (EntityId Timestamp Config)

end Enchiridion.Core
