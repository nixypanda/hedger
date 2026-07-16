module Hedger.Phase0
  ( workspaceEstablished,
  ) where

-- | Phase 0 deliberately contains no server implementation. This marker lets
-- the workspace compile before Phase 1 introduces the application runtime.
workspaceEstablished :: Bool
workspaceEstablished = True
