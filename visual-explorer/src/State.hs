module State
    ( State (..)
    ) where

import Data.ByteString (ByteString)
import Gerrit.Store.Acid (AcidState)
import Gerrit.Store.Types (CommitStore)
import System.Log.FastLogger (LoggerSet)

data State =
    State { logger      :: LoggerSet
          , database    :: AcidState CommitStore
          , credentials :: ByteString
          }
