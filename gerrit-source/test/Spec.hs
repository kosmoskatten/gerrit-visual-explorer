module Main
    ( main
    ) where

import DecoderTests (decodeGerritCommitInfo, decodeGerritChangeMap)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "JSON decode tests"
      [ testCase "Decode GerritCommitInfo" decodeGerritCommitInfo
      , testCase "Decode GerritChangeMap" decodeGerritChangeMap
      ]
    ]   

