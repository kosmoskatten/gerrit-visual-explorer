module Main
    ( main
    ) where

import Data.ByteString (ByteString)
import Gerrit.Store.Acid (openLocalState)
import Gerrit.Store.Types (emptyStore)
import Logger (createLogger, logMsg)
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

    
    
