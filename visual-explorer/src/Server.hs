{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Data.ByteString (ByteString)
import Gerrit.Store.Acid (openLocalState)
import Gerrit.Store.Types (emptyStore)
import Logger (createLogger, logMsg)
import Network.HTTP.Types
import Network.Wai.Conduit
import Network.Wai.Handler.Warp (run)
import State (State (..))
import System.Environment (getArgs, lookupEnv)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [port]    <- getArgs
    logger'   <- createLogger     
    maybeCred <- lookupEnv "GERRIT_CREDENTIALS"
    case maybeCred of
        Just cred -> do
            db <- openLocalState emptyStore
            let state = State { logger      = logger'
                              , database    = db
                              , credentials = BS.pack cred
                              } 
            runServer (read port) state
        Nothing   -> logMsg logger' "Error: $GERRIT_CREDENTIALS not set"

runServer :: Int -> State -> IO ()
runServer port state = do
    logMsg (logger state) $ printf "Starting server on port: %d" port
    run port $ application state

application :: State -> Application
application state request respond = respond =<< route state request

route :: State -> Request -> IO Response
route state request = do
    case rawPathInfo request of
        "/commits-per-active-day"
           | requestMethod request == "GET" -> 
               handleCommitsPerActiveDay state
           | otherwise                      -> 
               handleNotAllowed ["GET"]
        _                                  -> 
            return handleNotFound

handleCommitsPerActiveDay :: State -> IO Response
handleCommitsPerActiveDay = undefined

handleNotAllowed :: [ByteString] -> IO Response
handleNotAllowed allow =
    return $ responseLBS status405 
                         [("Allow", "," `BS.intercalate` allow)] ""

handleNotFound :: Response
handleNotFound = responseLBS status404
                             [("Content-Type", "text/plain")]
                             "Resource not found"
    
