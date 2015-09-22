{-# LANGUAGE OverloadedStrings #-}
module Gerrit.Source.Fetcher
    ( fetchCommits
    ) where

import Control.Monad (forM)
import Data.Aeson (FromJSON, decode)
import Data.Aeson.Types (Object, Value (..), (.:), parseMaybe)
import Data.ByteString (ByteString)
import Gerrit.Source.Types ( GerritCommitInfo (..)
                           , GerritFileInfo (..)
                           , GerritCommitFilter
                           , GerritCommitEntry
                           )
import Network.HTTP.Conduit ( Manager
                            , Request (..)
                            , httpLbs
                            , newManager
                            , parseUrl
                            , responseBody
                            , tlsManagerSettings
                            )
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Credentials = ByteString
type Server      = String

data Url = MergedChanges !String
         | FileList !String
    deriving Show

-- | Fetch commits from the specified server and using the given
-- credentials and commit filter.
fetchCommits :: Server -> Credentials -> GerritCommitFilter 
             -> IO (Maybe [GerritCommitEntry]) 
fetchCommits = undefined

getJSON :: FromJSON a => Manager -> Server -> Url -> Credentials 
        -> IO (Maybe a)
getJSON = undefined

{-
-- | Fetch commit info from the specified server.
fetchCommitInfo :: Manager -> Server -> IO (Maybe [GerritCommitInfo])
fetchCommitInfo mgr server = 
    toCommitInfo <$> (getJSON mgr server $ MergedChanges "bbi/bbi")
      where 
        toCommitInfo :: LBS.ByteString -> Maybe [GerritCommitInfo]
        toCommitInfo lbs = mapM commitInfo =<< toObjects lbs

-- | Fetch file info from the server given the commit info.
fetchFileInfo :: Manager -> Server -> GerritCommitInfo 
              -> IO (Maybe [GerritFileInfo])
fetchFileInfo mgr server ci =
    toFileInfo <$> (getJSON mgr server $ FileList (T.unpack $ changeId ci))
      where
        toFileInfo :: LBS.ByteString -> Maybe [GerritFileInfo]
        toFileInfo lbs = fileInfo =<< decode lbs

-- | Fetch stuff from the Gerrit server. Always authorize using HTTP
-- authorization. Also drop five bytes fron the body, which are added
-- by the Gerrit server.
getJSON :: Manager -> Server -> Url -> IO LBS.ByteString
getJSON mgr server url = do
    let url' = server `mappend` expand url
    req  <- parseUrl url'
    let req' = req { requestHeaders = [ auth ] }
    resp <- httpLbs req' mgr
    return (LBS.drop 5 $ responseBody resp)
      where
        auth = ("Authorization", "Basic dWFicGFzYTpTbXVyckUxMiE=")

-- | Expand "typesafe" URL:s to strings.
expand :: Url -> String
expand (MergedChanges project) = 
    "/a/changes/?q=project:" `mappend` project 
                             `mappend` "+branch:master+status:merged&n=1"

expand (FileList cid) =
    "/a/changes/" `mappend` cid `mappend` "/revisions/current/files/"

toObjects :: LBS.ByteString -> Maybe [Object]
toObjects = decode

-- | Tailor made JSON parsing of an Object to a CommitInto structure.
commitInfo :: Object -> Maybe GerritCommitInfo
commitInfo obj = flip parseMaybe obj $ 
    \o -> GerritCommitInfo <$> o .: "id"
                     <*> o .: "subject"
                     <*> (read <$> o .: "created")
                     <*> (read <$> o .: "updated")
                     <*> o .: "insertions"
                     <*> o .: "deletions"

-- | Tailor made JSON parsing of an Object to a FileInfo structure.
fileInfo :: Object -> Maybe [GerritFileInfo]
fileInfo obj =
    let xs = filter (\(k, _) -> k /= "/COMMIT_MSG") $ HM.toList obj
    in forM xs $ \(k, Object o) -> do
        (ins, del) <- fileChanges o
        Just $ GerritFileInfo { filePath      = k
                              , linesDeleted  = del
                              , linesInserted = ins }

fileChanges :: Object -> Maybe (Int, Int)
fileChanges obj = flip parseMaybe obj $ 
    \o -> (,) <$> o .: "lines_inserted" <*> o .: "lines_deleted"

-}
