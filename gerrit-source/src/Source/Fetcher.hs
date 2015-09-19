{-# LANGUAGE OverloadedStrings #-}
module Source.Fetcher
    ( fetchCommitInfo
    , fetchFileInfo
    ) where

import Control.Monad (forM)
import Data.Aeson (decode)
import Data.Aeson.Types (Object, Value (..), (.:), parseMaybe)
import Source.Types (CommitInfo (..), FileInfo (..))
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

type Server = String

data Url = MergedChanges !String
         | FileList !String
    deriving Show

-- | Fetch commit info from the specified server.
fetchCommitInfo :: Manager -> Server -> IO (Maybe [CommitInfo])
fetchCommitInfo mgr server = 
    toCommitInfo <$> (getJSON mgr server $ MergedChanges "bbi/bbi")
      where 
        toCommitInfo :: LBS.ByteString -> Maybe [CommitInfo]
        toCommitInfo lbs = mapM commitInfo =<< toObjects lbs

-- | Fetch file info from the server given the commit info.
fetchFileInfo :: Manager -> Server -> CommitInfo -> IO (Maybe [FileInfo])
fetchFileInfo mgr server ci =
    toFileInfo <$> (getJSON mgr server $ FileList (T.unpack $ change_id ci))
      where
        toFileInfo :: LBS.ByteString -> Maybe [FileInfo]
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
commitInfo :: Object -> Maybe CommitInfo
commitInfo obj = flip parseMaybe obj $ 
    \o -> CommitInfo <$> o .: "id"
                     <*> o .: "subject"
                     <*> (read <$> o .: "created")
                     <*> (read <$> o .: "updated")
                     <*> o .: "insertions"
                     <*> o .: "deletions"

-- | Tailor made JSON parsing of an Object to a FileInfo structure.
fileInfo :: Object -> Maybe [FileInfo]
fileInfo obj =
    let xs = filter (\(k, _) -> k /= "/COMMIT_MSG") $ HM.toList obj
    in forM xs $ \(k, Object o) -> do
        (ins, del) <- fileChanges o
        Just $ FileInfo { filePath      = k
                        , linesDeleted  = del
                        , linesInserted = ins }

fileChanges :: Object -> Maybe (Int, Int)
fileChanges obj = flip parseMaybe obj $ 
    \o -> (,) <$> o .: "lines_inserted" <*> o .: "lines_deleted"
