module Hedger.ConfigSpec (
    configSpec,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Config (Config (..), mkConfig, parseConfig, portValue)
import Hedger.Domain.Investment (AssetClass (..), InvestmentMapping (..), PerformanceAccount (..))
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.Period (monthOfYearValue)

configSpec :: TestTree
configSpec =
    testGroup
        "configuration"
        [ testCase "requires a journal runtime path" requiresJournalPath
        , testCase
            "the resolved-config builder validates the journal path"
            builderValidatesJournalPath
        , testCase "loads supported settings from YAML" loadsYaml
        , testCase "lets CLI scalar settings override YAML" cliOverridesYaml
        , testCase "loads gain mappings only from YAML" loadsGainMappings
        , testCase "loads supported asset class mappings from YAML" loadsAssetClassMappings
        , testCase "defaults asset class mappings when they are omitted" defaultsAssetClassMappings
        , testCase "rejects unsupported configuration versions" rejectsUnsupportedVersion
        , testCase "rejects malformed YAML sections" rejectsMalformedSections
        , testCase "allows empty gain-account lists" allowsEmptyGainAccounts
        , testCase "rejects unsupported asset class kinds" rejectsUnsupportedAssetClassKind
        , testCase "rejects asset class mappings without prefixes" rejectsEmptyAssetClassPrefixes
        , testCase "rejects duplicate asset class prefixes" rejectsDuplicateAssetClassPrefix
        , testCase "rejects overlapping asset class prefixes" rejectsOverlappingAssetClassPrefix
        , testCase
            "rejects asset class targets that do not total 100"
            rejectsInvalidAssetClassTargetTotal
        , testCase "rejects out-of-range port and fiscal month" rejectsOutOfRangePortAndFiscalMonth
        ]

requiresJournalPath :: IO ()
requiresJournalPath =
    assertEqual "" (Left "A non-empty journal path is required.") (parseConfig Nothing [])

builderValidatesJournalPath :: IO ()
builderValidatesJournalPath = do
    config <- requireConfig (parseConfig Nothing ["--journal", "fixture.journal"])
    assertEqual
        ""
        (Left "A non-empty journal path is required.")
        ( mkConfig
            "  "
            config.listenAddress
            config.listenPort
            config.defaultDisplayCommodity
            config.fiscalYearStartMonth
            config.investmentMappings
            config.assetClassMappings
        )

loadsYaml :: IO ()
loadsYaml = do
    config <- requireConfig (parseConfig (Just exampleConfig) [])
    assertEqual "journal" "/var/lib/hedger/main.journal" config.journalPath
    assertEqual "listen address" "0.0.0.0" config.listenAddress
    assertEqual "port" 9000 (portValue config.listenPort)
    assertEqual "commodity" "USD" config.defaultDisplayCommodity
    assertEqual "fiscal month" 1 (monthOfYearValue config.fiscalYearStartMonth)

cliOverridesYaml :: IO ()
cliOverridesYaml = do
    config <- requireConfig (parseConfig (Just exampleConfig) overrides)
    assertEqual "journal" "/runtime/override.journal" config.journalPath
    assertEqual "listen address" "127.0.0.1" config.listenAddress
    assertEqual "port" 8081 (portValue config.listenPort)
    assertEqual "commodity" "INR" config.defaultDisplayCommodity
    assertEqual "fiscal month" 4 (monthOfYearValue config.fiscalYearStartMonth)
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
    mapping <-
        case Investment.mappingForAccount "assets:equity:india" config.investmentMappings of
            Nothing ->
                assertFailure "expected an inherited equity gain mapping"
                    >> fail "missing mapping"
            Just value -> pure value
    assertEqual
        "gain accounts"
        ["income:dividend:india", "income:dividend:us"]
        (Investment.accountNameText . (.account) <$> mapping.performanceAccounts)

loadsAssetClassMappings :: IO ()
loadsAssetClassMappings = do
    config <- requireConfig (parseConfig (Just assetClassConfig) [])
    assertAssetClass config "assets:equity:india" Equity
    assertAssetClass config "assets:debt:fund" Debt
    assertAssetClass config "assets:cash:brokerage" Cash
    assertAssetClass config "assets:gold:etf" Gold
    assertAssetClass config "assets:hybrid:fund" Hybrid
    assertAssetClass config "assets:property:home" Property
    assertAssetClass config "assets:other:holding" Other
    assertEqual
        "asset class targets"
        [Just 45, Just 25, Just 10, Just 5, Just 5, Just 5, Just 5]
        ( fmap Investment.percentageValue . (.targetPercentage)
            <$> Investment.assetClassMappingsList config.assetClassMappings
        )

defaultsAssetClassMappings :: IO ()
defaultsAssetClassMappings = do
    withoutInvestments <- requireConfig (parseConfig (Just configWithoutInvestments) [])
    withInvestments <- requireConfig (parseConfig (Just exampleConfig) [])
    assertAssetClass withoutInvestments "assets:equity:india" Other
    assertAssetClass withInvestments "assets:equity:india" Other

assertAssetClass :: Config -> Text -> AssetClass -> IO ()
assertAssetClass config account expected =
    assertEqual
        "asset class"
        expected
        (Investment.assetClassForAccount account config.assetClassMappings)

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

allowsEmptyGainAccounts :: IO ()
allowsEmptyGainAccounts = do
    config <-
        requireConfig (parseConfig (Just emptyGainAccountsConfig) ["--journal", "fixture.journal"])
    mapping <-
        case Investment.mappingForAccount "assets:equity:excluded" config.investmentMappings of
            Nothing ->
                assertFailure "expected an empty child gain mapping"
                    >> fail "missing mapping"
            Just value -> pure value
    assertEqual "gain accounts" [] mapping.performanceAccounts

rejectsUnsupportedAssetClassKind :: IO ()
rejectsUnsupportedAssetClassKind =
    assertBool
        "unsupported asset class kinds must fail"
        (isLeft (parseConfig (Just invalidAssetClassKindConfig) []))

rejectsEmptyAssetClassPrefixes :: IO ()
rejectsEmptyAssetClassPrefixes =
    assertBool
        "empty asset class prefixes must fail"
        (isLeft (parseConfig (Just emptyAssetClassPrefixesConfig) []))

rejectsDuplicateAssetClassPrefix :: IO ()
rejectsDuplicateAssetClassPrefix =
    assertBool
        "duplicate asset class prefixes must fail"
        (isLeft (parseConfig (Just duplicateAssetClassPrefixConfig) []))

rejectsOverlappingAssetClassPrefix :: IO ()
rejectsOverlappingAssetClassPrefix =
    assertBool
        "overlapping asset class prefixes must fail"
        (isLeft (parseConfig (Just overlappingAssetClassPrefixConfig) []))

rejectsInvalidAssetClassTargetTotal :: IO ()
rejectsInvalidAssetClassTargetTotal =
    assertBool
        "asset class targets must total 100"
        (isLeft (parseConfig (Just invalidAssetClassTargetTotalConfig) []))

rejectsOutOfRangePortAndFiscalMonth :: IO ()
rejectsOutOfRangePortAndFiscalMonth = do
    assertBool
        "out-of-range port must fail"
        (isLeft (parseConfig Nothing ["--journal", "fixture.journal", "--port", "0"]))
    assertBool
        "out-of-range fiscal month must fail"
        (isLeft (parseConfig Nothing ["--journal", "fixture.journal", "--fiscal-year-start", "13"]))

requireConfig :: Either Text Config -> IO Config
requireConfig result =
    case result of
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
malformedSectionConfig = ByteString.unlines ["version: 1", "server: \"not an object\""]

emptyGainAccountsConfig :: ByteString
emptyGainAccountsConfig =
    ByteString.unlines
        [ "version: 1"
        , "investments:"
        , "  gainMappings:"
        , "    - assetAccount: \"assets:equity:excluded\""
        , "      gainAccounts: []"
        ]

configWithoutInvestments :: ByteString
configWithoutInvestments = ByteString.unlines ["version: 1", "journal:", "  path: fixture.journal"]

assetClassConfig :: ByteString
assetClassConfig =
    ByteString.unlines
        [ "version: 1"
        , "journal:"
        , "  path: fixture.journal"
        , "investments:"
        , "  assetClasses:"
        , "    - kind: equity"
        , "      accountPrefixes: [assets:equity]"
        , "      targetPercent: 45"
        , "    - kind: debt"
        , "      accountPrefixes: [assets:debt]"
        , "      targetPercent: 25"
        , "    - kind: cash"
        , "      accountPrefixes: [assets:cash]"
        , "      targetPercent: 10"
        , "    - kind: gold"
        , "      accountPrefixes: [assets:gold]"
        , "      targetPercent: 5"
        , "    - kind: hybrid"
        , "      accountPrefixes: [assets:hybrid]"
        , "      targetPercent: 5"
        , "    - kind: property"
        , "      accountPrefixes: [assets:property]"
        , "      targetPercent: 5"
        , "    - kind: other"
        , "      accountPrefixes: [assets:other]"
        , "      targetPercent: 5"
        ]

invalidAssetClassKindConfig :: ByteString
invalidAssetClassKindConfig = assetClassConfigWith "kind: commodities"

emptyAssetClassPrefixesConfig :: ByteString
emptyAssetClassPrefixesConfig = assetClassConfigWith "accountPrefixes: []"

duplicateAssetClassPrefixConfig :: ByteString
duplicateAssetClassPrefixConfig =
    ByteString.unlines
        [ "version: 1"
        , "journal:"
        , "  path: fixture.journal"
        , "investments:"
        , "  assetClasses:"
        , "    - kind: equity"
        , "      accountPrefixes: [assets:equity]"
        , "    - kind: debt"
        , "      accountPrefixes: [assets:equity]"
        ]

overlappingAssetClassPrefixConfig :: ByteString
overlappingAssetClassPrefixConfig =
    ByteString.unlines
        [ "version: 1"
        , "journal:"
        , "  path: fixture.journal"
        , "investments:"
        , "  assetClasses:"
        , "    - kind: equity"
        , "      accountPrefixes: [assets:equity]"
        , "    - kind: debt"
        , "      accountPrefixes: [assets:equity:india]"
        ]

invalidAssetClassTargetTotalConfig :: ByteString
invalidAssetClassTargetTotalConfig = assetClassConfigWithTarget "targetPercent: 75"

assetClassConfigWithTarget :: ByteString -> ByteString
assetClassConfigWithTarget setting =
    ByteString.unlines
        [ "version: 1"
        , "journal:"
        , "  path: fixture.journal"
        , "investments:"
        , "  assetClasses:"
        , "    - kind: equity"
        , "      accountPrefixes: [assets:equity]"
        , "      " <> setting
        ]

assetClassConfigWith :: ByteString -> ByteString
assetClassConfigWith setting =
    ByteString.unlines
        [ "version: 1"
        , "journal:"
        , "  path: fixture.journal"
        , "investments:"
        , "  assetClasses:"
        , "    - kind: equity"
        , "      " <> setting
        ]
