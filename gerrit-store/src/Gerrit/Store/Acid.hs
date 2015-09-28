{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Gerrit.Store.Acid
    ( AddCommits (..)
    , GetCommits (..)
    , AcidState
    , filterCommits
    , openLocalState
    , update
    , query
    ) where

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid ( AcidState
                 , Update
                 , Query
                 , openLocalState
                 , makeAcidic
                 , update
                 , query
                 )
import Data.SafeCopy (base, deriveSafeCopy)
import Gerrit.Fetch.Types ( GerritCommitEntry
                          , GerritCommitInfo (..)
                          , GerritFileInfo
                          , GerritCommitFilter
                          )
import Gerrit.Store.Import (prependCommits)
import Gerrit.Store.Types (Change, CommitEntry, File, CommitStore (..))
import qualified Data.Set as Set

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

-- | Filter the list of Gerrit commits to include only the commits not
-- present in the database.
filterCommits :: AcidState CommitStore -> GerritCommitFilter
filterCommits db xs = do
    commits <- commitSet <$>  query db GetCommits
    return $ filter (\x -> not (Set.member (changeId x) commits)) xs
