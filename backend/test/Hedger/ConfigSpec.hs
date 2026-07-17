module Hedger.ConfigSpec (configSpec) where

import Hedger.Config (Config (..), parseConfig)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

configSpec :: TestTree
configSpec =
    testGroup
        "configuration"
        [ testCase "requires a journal runtime path" $
            assertEqual "" (Left "A non-empty journal path is required.") (parseConfig [] [])
        , testCase "validates bounded port" $
            assertEqual
                ""
                (Left "Port must be between 1 and 65535.")
                (parseConfig [("HEDGER_JOURNAL_PATH", "fixture.journal")] ["--port", "0"])
        , testCase "validates fiscal month" $
            assertEqual
                ""
                (Left "Fiscal-year start must be between 1 and 12.")
                (parseConfig [("HEDGER_JOURNAL_PATH", "fixture.journal")] ["--fiscal-year-start", "13"])
        , testCase "allows runtime journal paths with spaces and lets CLI values win" $
            assertEqual
                ""
                (Right "/runtime/new journal")
                ( fmap
                    (.journalPath)
                    ( parseConfig
                        [ ("HEDGER_JOURNAL_PATH", "/runtime/old journal")
                        ]
                        ["--journal", "/runtime/new journal"]
                    )
                )
        ]
