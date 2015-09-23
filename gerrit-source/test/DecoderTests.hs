{-# LANGUAGE OverloadedStrings #-}
module DecoderTests
    ( decodeGerritCommitInfo
    , decodeGerritChangeMap
    ) where

import Data.Aeson (decode)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Gerrit.Source.Types ( GerritCommitInfo (..)
                           , GerritChangeMap (..)
                           , GerritFileInfo (..)
                           )
import Test.HUnit

-- | Test the decoding of a proper GerritCommitInfo record from
-- JSON data.
decodeGerritCommitInfo :: Assertion
decodeGerritCommitInfo = do
    let json = "{\"id\" : \"myproject-123456\",\
               \ \"subject\" : \"Some task\",\
               \ \"updated\" : \"2015-09-22 11:22:23.368 UTC\" }"
        info = fromJust (decode json)
    "myproject-123456"            @=? changeId info
    "Some task"                   @=? commitSubject info
    "2015-09-22 11:22:23.368 UTC" @=? show (updated info)

decodeGerritChangeMap :: Assertion
decodeGerritChangeMap = do
    -- The decoding of the change map has a number of quirks. The
    -- absense of a deleted or inserted value in the JSON data will
    -- be replaced with a zero value by the decoder.
    let json = "{\"A.hs\": {\"status\": \"D\", \"lines_deleted\": 44},\
               \ \"B.hs\": {\"lines_inserted\": 1, \"lines_deleted\": 2},\
               \ \"C.hs\": {}}"
        [a, b, c] = (sortBy byFileInfo . allChanges . fromJust) (decode json)
    "A.hs" @=? filePath a
    0      @=? linesInserted a
    44     @=? linesDeleted a

    "B.hs" @=? filePath b
    1      @=? linesInserted b
    2      @=? linesDeleted b

    "C.hs" @=? filePath c
    0      @=? linesInserted c
    0      @=? linesDeleted c

byFileInfo :: GerritFileInfo -> GerritFileInfo -> Ordering
byFileInfo f1 f2 = filePath f1 `compare` filePath f2
