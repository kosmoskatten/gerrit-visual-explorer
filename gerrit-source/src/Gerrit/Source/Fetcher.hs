{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Gerrit.Source.Fetcher
    ( fetchCommits
    ) where

import Data.Aeson (FromJSON, decode')
import Data.ByteString (ByteString)
import Gerrit.Source.Types ( GerritCommitInfo (..)
                           , GerritFileInfo (..)
                           , GerritChangeMap (..)
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
import qualified Data.Text as T

type Credentials = ByteString
type Project     = String
type Server      = String

data Url = MergedChanges !String
         | FileList !String
    deriving Show

-- | Fetch commits from the specified server and using the given
-- credentials and commit filter.
fetchCommits :: Server -> Project -> Credentials -> GerritCommitFilter 
             -> IO (Maybe [GerritCommitEntry]) 
fetchCommits server proj cred filt = do
    mgr <- newManager tlsManagerSettings
    maybeInfos <- getJSON mgr server (MergedChanges proj) cred
    case maybeInfos of
        Just infos -> do
            entries <- mapM (mkCommitEntry mgr server cred) =<< filt infos
            return $ mapM id entries
        Nothing    -> return Nothing

mkCommitEntry :: Manager -> Server -> Credentials -> GerritCommitInfo
              -> IO (Maybe GerritCommitEntry)
mkCommitEntry mgr server cred info = do
    maybeFiles <- fetchFiles mgr server cred info
    case maybeFiles of
        Just files -> return (Just (info, files))
        Nothing    -> return Nothing

fetchFiles :: Manager -> Server -> Credentials -> GerritCommitInfo 
           -> IO (Maybe [GerritFileInfo])
fetchFiles mgr server cred GerritCommitInfo {..} = do
    let url = FileList (T.unpack changeId)
    maybeChMap <- getJSON mgr server url cred
    return $ maybe Nothing (Just . removeCommitMsg . allChanges) maybeChMap 

getJSON :: FromJSON a => Manager -> Server -> Url -> Credentials 
        -> IO (Maybe a)
getJSON mgr server url cred = do
    let url' = server `mappend` expand url
    req <- parseUrl url'
    let req' = req { requestHeaders = [auth] }
    decodeIt <$> httpLbs req' mgr
      where
        auth = ("Authorization", "Basic " `mappend` cred)
        decodeIt = decode' . LBS.drop 5 . responseBody

-- | Expand "typesafe" URL:s to strings.
expand :: Url -> String
expand (MergedChanges project) = 
    "/a/changes/?q=project:" `mappend` project 
                             `mappend` "+branch:master+status:merged&n=20"

expand (FileList cid) =
    "/a/changes/" `mappend` cid `mappend` "/revisions/current/files/"

removeCommitMsg :: [GerritFileInfo] -> [GerritFileInfo]
removeCommitMsg = filter (\f -> filePath f /= "/COMMIT_MSG")
