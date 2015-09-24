{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Gerrit.Store.Acid
    ( AddCommits (..)
    , GetCommits (..)
    ) where

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid
import Data.SafeCopy
import Gerrit.Fetch.Types
import Gerrit.Store.Import
import Gerrit.Store.Types
import Gerrit.Store.Pure

$(deriveSafeCopy 0 'base ''Change)
$(deriveSafeCopy 0 'base ''CommitEntry)
$(deriveSafeCopy 0 'base ''File)
$(deriveSafeCopy 0 'base ''CommitStore)
$(deriveSafeCopy 0 'base ''GerritCommitInfo)
$(deriveSafeCopy 0 'base ''GerritFileInfo)

addCommits :: [GerritCommitEntry] -> Update CommitStore ()
addCommits xs = modify (importCommits xs)

getCommits :: Query CommitStore CommitStore
getCommits = ask

$(makeAcidic ''CommitStore ['addCommits, 'getCommits])
