{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString, intDec)
import Data.Conduit
import Data.Monoid ((<>))
import Data.Time
import Data.Vector (Vector)
import Gerrit.Store.Acid (GetCommits (..), openLocalState, query)
import Gerrit.Store.Analysis (Summary (..), summaryPerActiveDay)
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
        "/changed-files-per-active-day"
           | requestMethod request == "GET" ->
               handleChangedFilesPerActiveDay state
           | otherwise                      ->
               handleNotAllowed ["GET"]
        _                                   -> 
            return handleNotFound

handleCommitsPerActiveDay :: State -> IO Response
handleCommitsPerActiveDay state = do
    store <- query (database state) GetCommits
    let summary = summaryPerActiveDay store
        builder = renderCommitActivityCalender 
                    "Commits per active day" summary sumCommits
    return $ 
        responseSource status200 [("Content-Type", "text/html")]
                       (yield $ Chunk builder)

handleChangedFilesPerActiveDay :: State -> IO Response
handleChangedFilesPerActiveDay state = do
    store <- query (database state) GetCommits
    let summary = summaryPerActiveDay store
        builder = renderCommitActivityCalender 
                    "Changed files per active day" summary sumChangedFiles
    return $ 
        responseSource status200 [("Content-Type", "text/html")]
                       (yield $ Chunk builder)


handleNotAllowed :: [ByteString] -> IO Response
handleNotAllowed allow =
    return $ responseLBS status405 
                         [("Allow", "," `BS.intercalate` allow)] ""

handleNotFound :: Response
handleNotFound = responseLBS status404
                             [("Content-Type", "text/plain")]
                             "Resource not found"
   
renderCommitActivityCalender :: ByteString -> Vector Summary 
                             -> (Summary -> Int) -> Builder
renderCommitActivityCalender title vec get =
    renderCalenderPageHead title 
        <> renderCalenderDrawChart title vec get
        <> renderCalenderPageFoot

renderCalenderPageHead :: ByteString -> Builder
renderCalenderPageHead title =
    byteString "<!DOCTYPE html>\n" <>
    byteString "<html>\n" <>
    byteString " <head>\n" <>
    byteString "  <title>" <> byteString title <> byteString "</title>\n" <>
    byteString "  <script type=\"text/javascript\" \
                      \src=\"https://www.google.com/jsapi\"></script>\n" <>
    byteString "  <script type=\"text/javascript\">\n" <>
    byteString "    google.load(\"visualization\", \"1.1\",\
                              \{packages:[\"calendar\"]});\n" <>
    byteString "    google.setOnLoadCallback(drawChart);\n"

renderCalenderDrawChart :: ByteString -> Vector Summary 
                        -> (Summary -> Int) -> Builder
renderCalenderDrawChart title vec get =
    byteString "function drawChart() {\n" <>
    byteString "  var dataTable=new google.visualization.DataTable();\n" <>
    byteString "  dataTable.addColumn({type:'date', id:'Date'});\n" <>
    byteString "  dataTable.addColumn({type:'number', id:'#'});\n" <>
    byteString "  dataTable.addRows([\n" <>
    renderCalenderDataRows vec get <>
    byteString "  ]);\n" <>
    byteString "  var chart=new google.visualization.Calendar(\
                     \document.getElementById('calender'));\n" <>
    byteString "  var options={\n" <>
    byteString "    title: \"" <> byteString title <> byteString "\",\n" <>
    byteString "    height:350,\n" <>
    byteString "    width:1000\n" <>
    byteString "  }\n" <>
    byteString "  chart.draw(dataTable, options);\n" <>
    byteString "}\n"

renderCalenderDataRows :: Vector Summary -> (Summary -> Int) -> Builder
renderCalenderDataRows vec get =
    foldMap renderDataRow vec
      where
        renderDataRow :: Summary -> Builder
        renderDataRow summary =
            let (y, m, d) = parseDay (sumDay summary) 
            in byteString "[ new Date(" <> intDec y <> byteString ", "
                                        <> intDec m <> byteString ", "
                                        <> intDec d <> byteString "),"
                                        <> intDec (get summary) 
                                        <> byteString "],\n"

renderCalenderPageFoot :: Builder
renderCalenderPageFoot =
    byteString "    </script>\n" <>
    byteString "  </head>\n" <>
    byteString "  <body>\n"  <>
    byteString "    <div id=\"calender\"></div>\n" <>
    byteString "  </body>\n" <>
    byteString "</html>"

-- | Parse a day to components. The month is zero indexed to be 
-- javascript compatible.
parseDay :: Day -> (Int, Int, Int)
parseDay d =
    let [(year, '-':monthS)] = reads (show d)
        [(month, '-':dayS)]  = reads monthS
        [(day, "")]          = reads dayS
    in (year, month - 1, day)
