{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Gerrit.Store.Acid
    ( AddCommits (..)
    , GetCommits (..)
    ) where

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid (Update, Query, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Gerrit.Fetch.Types ( GerritCommitEntry
                          , GerritCommitInfo
                          , GerritFileInfo
                          )
import Gerrit.Store.Import (prependCommits)
import Gerrit.Store.Types (Change, CommitEntry, File, CommitStore)

-- | Derive SafeCopy instances for all types.
$(deriveSafeCopy 0 'base ''Change)
$(deriveSafeCopy 0 'base ''CommitEntry)
$(deriveSafeCopy 0 'base ''File)
$(deriveSafeCopy 0 'base ''CommitStore)
$(deriveSafeCopy 0 'base ''GerritCommitInfo)
$(deriveSafeCopy 0 'base ''GerritFileInfo)

-- | Add (prepent) a new set of commit entries to the commit store.
addCommits :: [GerritCommitEntry] -> Update CommitStore ()
addCommits xs = modify (prependCommits xs)

-- | Get out the commit store.
getCommits :: Query CommitStore CommitStore
getCommits = ask

$(makeAcidic ''CommitStore ['addCommits, 'getCommits])
