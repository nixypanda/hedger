module Hedger.ConfigSpec (configSpec) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Hedger.Config (Config (..), parseConfig)
import Hedger.Domain.Investment qualified as Investment

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
        , testCase "temporarily maps the India-US portfolio fixture" $
            assertEqual
                ""
                [ "assets:debt"
                , "assets:debt:epf:india:nimbusworks"
                , "assets:debt:eps:india:nimbusworks"
                , "assets:debt:ppf:india:varuna"
                , "assets:hybrid"
                , "assets:hybrid:india:nps:monsoon"
                , "assets:equity"
                , "assets:liquid"
                , "assets:gold"
                , "assets:property"
                ]
                ( Investment.mappedAssetAccounts
                    (configFor "../examples/india-us-portfolio-2023-2025.journal").investmentMappings
                )
        , testCase "keeps other journals unmapped" $
            assertEqual
                ""
                []
                (Investment.mappedAssetAccounts (configFor "fixture.journal").investmentMappings)
        ]

configFor :: Text -> Config
configFor journalPath = case parseConfig [("HEDGER_JOURNAL_PATH", journalPath)] [] of
    Right config -> config
    Left message -> error (show message)
