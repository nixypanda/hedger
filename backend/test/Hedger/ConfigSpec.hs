module Hedger.ConfigSpec (configSpec) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Config (Config (..), parseConfig)
import Hedger.Domain.Investment (InvestmentMapping (..), PerformanceAccount (..))
import Hedger.Domain.Investment qualified as Investment

configSpec :: TestTree
configSpec =
    testGroup
        "configuration"
        [ testCase "requires a journal runtime path" requiresJournalPath
        , testCase "loads supported settings from YAML" loadsYaml
        , testCase "lets CLI scalar settings override YAML" cliOverridesYaml
        , testCase "loads gain mappings only from YAML" loadsGainMappings
        , testCase "rejects unsupported configuration versions" rejectsUnsupportedVersion
        , testCase "rejects malformed YAML sections" rejectsMalformedSections
        , testCase "rejects empty gain-account lists" rejectsEmptyGainAccounts
        , testCase "validates bounded port" validatesPort
        , testCase "validates fiscal month" validatesFiscalMonth
        ]

requiresJournalPath :: IO ()
requiresJournalPath =
    assertEqual "" (Left "A non-empty journal path is required.") (parseConfig Nothing [])

loadsYaml :: IO ()
loadsYaml = do
    config <- requireConfig (parseConfig (Just exampleConfig) [])
    assertEqual "journal" "/var/lib/hedger/main.journal" config.journalPath
    assertEqual "listen address" "0.0.0.0" config.listenAddress
    assertEqual "port" 9000 config.listenPort
    assertEqual "commodity" "USD" config.defaultDisplayCommodity
    assertEqual "fiscal month" 1 config.fiscalYearStartMonth

cliOverridesYaml :: IO ()
cliOverridesYaml = do
    config <- requireConfig (parseConfig (Just exampleConfig) overrides)
    assertEqual "journal" "/runtime/override.journal" config.journalPath
    assertEqual "listen address" "127.0.0.1" config.listenAddress
    assertEqual "port" 8081 config.listenPort
    assertEqual "commodity" "INR" config.defaultDisplayCommodity
    assertEqual "fiscal month" 4 config.fiscalYearStartMonth
  where
    overrides =
        [ "--journal"
        , "/runtime/override.journal"
        , "--listen-address"
        , "127.0.0.1"
        , "--port"
        , "8081"
        , "--display-commodity"
        , "INR"
        , "--fiscal-year-start"
        , "4"
        ]

loadsGainMappings :: IO ()
loadsGainMappings = do
    config <- requireConfig (parseConfig (Just exampleConfig) [])
    mapping <- case Investment.mappingForAccount "assets:equity:india" config.investmentMappings of
        Nothing -> assertFailure "expected an inherited equity gain mapping" >> fail "missing mapping"
        Just value -> pure value
    assertEqual
        "gain accounts"
        ["income:dividend:india", "income:dividend:us"]
        ((.account) <$> mapping.performanceAccounts)

rejectsUnsupportedVersion :: IO ()
rejectsUnsupportedVersion =
    assertEqual
        ""
        (Left "Configuration version must be 1.")
        (parseConfig (Just "version: 2\n") ["--journal", "fixture.journal"])

rejectsMalformedSections :: IO ()
rejectsMalformedSections =
    assertBool
        "malformed sections must fail decoding"
        (isLeft (parseConfig (Just malformedSectionConfig) ["--journal", "fixture.journal"]))

rejectsEmptyGainAccounts :: IO ()
rejectsEmptyGainAccounts =
    assertEqual
        ""
        (Left "Each investment gain mapping requires at least one gain account.")
        (parseConfig (Just emptyGainAccountsConfig) ["--journal", "fixture.journal"])

validatesPort :: IO ()
validatesPort =
    assertEqual
        ""
        (Left "Port must be between 1 and 65535.")
        (parseConfig Nothing ["--journal", "fixture.journal", "--port", "0"])

validatesFiscalMonth :: IO ()
validatesFiscalMonth =
    assertEqual
        ""
        (Left "Fiscal-year start must be between 1 and 12.")
        (parseConfig Nothing ["--journal", "fixture.journal", "--fiscal-year-start", "13"])

requireConfig :: Either Text Config -> IO Config
requireConfig result = case result of
    Left message -> assertFailure (show message) >> fail "invalid configuration"
    Right config -> pure config

exampleConfig :: ByteString
exampleConfig =
    ByteString.unlines
        [ "version: 1"
        , ""
        , "journal:"
        , "  path: \"/var/lib/hedger/main.journal\""
        , ""
        , "server:"
        , "  listenAddress: \"0.0.0.0\""
        , "  port: 9000"
        , ""
        , "display:"
        , "  commodity: \"USD\""
        , "  fiscalYearStartMonth: 1"
        , ""
        , "investments:"
        , "  gainMappings:"
        , "    - assetAccount: \"assets:equity\""
        , "      gainAccounts:"
        , "        - \"income:dividend:india\""
        , "        - \"income:dividend:us\""
        ]

malformedSectionConfig :: ByteString
malformedSectionConfig =
    ByteString.unlines
        [ "version: 1"
        , "server: \"not an object\""
        ]

emptyGainAccountsConfig :: ByteString
emptyGainAccountsConfig =
    ByteString.unlines
        [ "version: 1"
        , "investments:"
        , "  gainMappings:"
        , "    - assetAccount: \"assets:equity\""
        , "      gainAccounts: []"
        ]
