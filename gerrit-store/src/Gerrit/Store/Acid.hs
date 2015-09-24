{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Gerrit.Store.Acid
    (
    ) where

import Control.Monad.State (modify)
import Data.Acid
import Data.SafeCopy
import Gerrit.Source.Types
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

$(makeAcidic ''CommitStore ['addCommits])
