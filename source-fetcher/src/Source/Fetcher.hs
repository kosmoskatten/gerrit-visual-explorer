{-# LANGUAGE OverloadedStrings #-}
module Source.Fetcher
    ( CommitInfo (..) 
    , Url (..)
    , fetchCommitInfo
    , fetchFileInfo
    ) where

import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Time (UTCTime)
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Server = String

data Url = MergedChanges !String
         | FileList !String
    deriving Show

data CommitInfo = 
    CommitInfo { change_id  :: !Text
               , created    :: !UTCTime
               , updated    :: !UTCTime
               , insertions :: !Int
               , deletions  :: !Int }
    deriving Show

data FileInfo =
    FileInfo { filePath      :: !Text
             , linesDeleted  :: !Int
             , linesInserted :: !Int }
    deriving Show

fetchCommitInfo :: Manager -> Server -> IO (Maybe [CommitInfo])
fetchCommitInfo mgr server = 
    toCommitInfo <$> (getJSON mgr server $ MergedChanges "bbi/bbi")
      where 
        toCommitInfo :: LBS.ByteString -> Maybe [CommitInfo]
        toCommitInfo lbs = mapM commitInfo =<< toObjects lbs

fetchFileInfo :: Manager -> Server -> CommitInfo -> IO (Maybe [FileInfo])
fetchFileInfo mgr server ci =
    toFileInfo <$> (getJSON mgr server $ FileList (T.unpack $ change_id ci))
      where
        toFileInfo :: LBS.ByteString -> Maybe [FileInfo]
        toFileInfo lbs = fileInfo =<< decode lbs

getJSON :: Manager -> Server -> Url -> IO LBS.ByteString
getJSON mgr server url = do
    let url' = server `mappend` expand url
    req  <- parseUrl url'
    let req' = req { requestHeaders = [ auth ] }
    resp <- httpLbs req' mgr
    return (LBS.drop 5 $ responseBody resp)
      where
        auth = ("Authorization", "Basic dWFicGFzYTpTbXVyckUxMiE=")

expand :: Url -> String
expand (MergedChanges project) = 
    "/a/changes/?q=project:" `mappend` project 
                             `mappend` "+branch:master+status:merged&n=1"

expand (FileList cid) =
    "/a/changes/" `mappend` cid `mappend` "/revisions/current/files/"

toObjects :: LBS.ByteString -> Maybe [Object]
toObjects = decode

commitInfo :: Object -> Maybe CommitInfo
commitInfo obj = flip parseMaybe obj $ 
    \o -> CommitInfo <$> o .: "id"
                     <*> (read <$> o .: "created")
                     <*> (read <$> o .: "updated")
                     <*> o .: "insertions"
                     <*> o .: "deletions"

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
