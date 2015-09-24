module Main
    ( main
    ) where

import Data.Acid
import Gerrit.Source.Fetcher
import Gerrit.Store.Acid
import Gerrit.Store.Types
import System.Environment
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [cred] <- getArgs
    db     <- openLocalState emptyStore
    importData db (BS.pack cred)

importData :: AcidState CommitStore -> BS.ByteString -> IO ()
importData db cred= do
    mXs <- fetchCommits "https://gerrit.ericsson.se" "bbi/bbi" cred return
    case mXs of
        Just xs -> update db (AddCommits xs) >> print "Yay!"
        Nothing -> print "Failed!"    
    

