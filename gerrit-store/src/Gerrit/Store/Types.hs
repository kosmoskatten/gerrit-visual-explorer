module Gerrit.Store.Types
    ( CommitData (..)
    , CommitEntry (..)
    , Change (..)
    , File (..)
    , CommitSet 
    , FileMap
    , emptyCommitData
    , emptyFile
    ) where

import Control.Concurrent.STM (STM, TVar, newTVar)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Vector as Vector

data CommitEntry = 
        CommitEntry { timeStamp :: !UTCTime
                    , commitId  :: !Text
                    , subject   :: !Text
                    , changes   :: !(Vector Change)
                    }
    deriving Show

data Change =
        Change { insertions :: {-# UNPACK #-} !Int
               , deletions  :: {-# UNPACK #-} !Int
               , file       :: !File
               }
    deriving Show

data File = 
    File { name   :: !Text
         , partOf :: TVar TimeLine
         -- ^ The timeline shall be mutable. Its mutable by means
         -- of TVar. 
         }

instance Show File where
    show f = ""

type TimeLine  = Vector CommitEntry
type FileMap   = HashMap Text File
type CommitSet = HashSet Text

data CommitData =
        CommitData { timeLine  :: !TimeLine
                   , fileMap   :: !FileMap
                   , commitSet :: !CommitSet
                   }
    deriving Show

emptyCommitData :: CommitData
emptyCommitData = CommitData { timeLine  = Vector.empty
                             , fileMap   = HashMap.empty
                             , commitSet = HashSet.empty } 

emptyFile :: Text -> STM File
emptyFile name' = File name' <$> newTVar Vector.empty
