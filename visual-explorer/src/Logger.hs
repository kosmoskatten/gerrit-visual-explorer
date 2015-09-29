module Logger 
    ( createLogger
    , logMsg
    ) where

import Data.Time (getCurrentTime)
import System.Log.FastLogger ( LoggerSet
                             , ToLogStr (..)
                             , newStdoutLoggerSet
                             , defaultBufSize
                             , pushLogStrLn
                             )

createLogger :: IO LoggerSet
createLogger = newStdoutLoggerSet defaultBufSize

logMsg :: LoggerSet -> String -> IO ()
logMsg logger str = do
    now <- getCurrentTime
    let logStr = toLogStr (show now) `mappend` toLogStr (" : " :: String) 
                                     `mappend` toLogStr str
    pushLogStrLn logger logStr

