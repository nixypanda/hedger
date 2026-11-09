module Hedger.ConfigSpec (
    configSpec,
) where

import Control.Exception (IOException, catch, finally)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (
    createDirectory,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
    withCurrentDirectory,
 )
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Config (
    Catalog (..),
    Config (..),
    LedgerConfig (..),
    LedgerId,
    decodeCatalogFileConfig,
    ledgerIdText,
    loadCatalogFile,
    mkConfig,
    mkLedgerId,
    parseConfig,
    portValue,
 )
import Hedger.Domain.Investment (AssetClass (..), InvestmentMapping (..), PerformanceAccount (..))
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.Period (monthOfYearValue)

configSpec :: TestTree
configSpec =
    testGroup
        "configuration"
        [ testCase "requires a journal runtime path" requiresJournalPath
        , testCase "loads a named ledger catalog" loadsCatalog
        , testCase "applies referenced ledger settings" appliesReferencedSettings
        , testCase
            "resolves relative paths from another working directory"
            resolvesRelativePathsAcrossCwd
        , testCase "rejects a missing ledger settings file" rejectsMissingSettingsFile
        , testCase "rejects malformed ledger settings" rejectsMalformedSettings
        , testCase "rejects the wrong ledger settings version" rejectsWrongSettingsVersion
        , testCase "rejects deployment keys in ledger settings" rejectsDeploymentKeysInSettings
        , testCase "rejects a blank configuration path" rejectsBlankConfigPath
        , testCase "rejects a whitespace-only journal path" rejectsWhitespaceJournalPath
        , testCase "rejects mixed ledger keys" rejectsMixedLedgerKeys
        , testCase "allows a catalog without a default ledger" allowsNoDefaultLedger
        , testCase "rejects duplicate catalog IDs" rejectsDuplicateCatalogIds
        , testCase "rejects mixed configuration layouts" rejectsMixedCatalogLayout
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

loadsCatalog :: IO ()
loadsCatalog = withTempConfigDir $ \directory -> do
    ByteString.writeFile (directory </> "personal.settings.yaml") personalSettings
    ByteString.writeFile (directory </> "business.settings.yaml") businessSettings
    ByteString.writeFile (directory </> "catalog.yaml") catalogDocument
    catalog <-
        either (assertFailure . Text.unpack) pure
            =<< loadCatalogFile (directory </> "catalog.yaml")
    assertEqual "two ledgers" 2 (Map.size catalog.ledgers)
    assertEqual
        "configuration order"
        ["personal", "business"]
        (ledgerIdText <$> catalog.ledgerOrder)
    let Just personal = Map.lookup (requireLedgerId "personal") catalog.ledgers
    assertEqual "personal commodity" "USD" personal.ledgerConfig.defaultDisplayCommodity
    assertEqual
        "personal fiscal month"
        1
        (monthOfYearValue personal.ledgerConfig.fiscalYearStartMonth)
    let Just business = Map.lookup (requireLedgerId "business") catalog.ledgers
    assertEqual "business commodity" "EUR" business.ledgerConfig.defaultDisplayCommodity
    assertEqual
        "business fiscal month"
        7
        (monthOfYearValue business.ledgerConfig.fiscalYearStartMonth)

allowsNoDefaultLedger :: IO ()
allowsNoDefaultLedger = withTempConfigDir $ \directory -> do
    ByteString.writeFile (directory </> "personal.settings.yaml") personalSettings
    ByteString.writeFile (directory </> "catalog.yaml") noDefaultCatalogDocument
    catalog <-
        either (assertFailure . Text.unpack) pure
            =<< loadCatalogFile (directory </> "catalog.yaml")
    assertEqual "no default" Nothing catalog.defaultLedger

noDefaultCatalogDocument :: ByteString
noDefaultCatalogDocument =
    ByteString.unlines
        [ "version: 3"
        , "ledgers:"
        , "  - id: personal"
        , "    label: Personal"
        , "    journalPath: personal.journal"
        , "    configPath: personal.settings.yaml"
        ]

rejectsDuplicateCatalogIds :: IO ()
rejectsDuplicateCatalogIds = withTempConfigDir $ \directory -> do
    ByteString.writeFile (directory </> "personal.settings.yaml") personalSettings
    ByteString.writeFile (directory </> "business.settings.yaml") businessSettings
    ByteString.writeFile (directory </> "other.settings.yaml") personalSettings
    ByteString.writeFile (directory </> "catalog.yaml") (catalogDocument <> duplicateLedger)
    result <- loadCatalogFile (directory </> "catalog.yaml")
    case result of
        Left problem ->
            assertBool
                "mentions duplicate ledger IDs"
                ("Ledger IDs must be unique." `Text.isInfixOf` problem)
        Right _ -> assertFailure "expected failure"

rejectsMixedCatalogLayout :: IO ()
rejectsMixedCatalogLayout =
    assertBool
        "top-level journal is invalid in a catalog"
        (isLeft (decodeCatalogFileConfig (catalogDocument <> "journal:\n  path: other.journal\n")))

duplicateLedger :: ByteString
duplicateLedger =
    ByteString.unlines
        [ "  - id: personal"
        , "    label: Again"
        , "    journalPath: other.journal"
        , "    configPath: other.settings.yaml"
        ]

catalogDocument :: ByteString
catalogDocument =
    ByteString.unlines
        [ "version: 3"
        , "defaultLedger: personal"
        , "ledgers:"
        , "  - id: personal"
        , "    label: Personal"
        , "    journalPath: personal.journal"
        , "    configPath: personal.settings.yaml"
        , "  - id: business"
        , "    label: Business"
        , "    journalPath: business.journal"
        , "    configPath: business.settings.yaml"
        ]

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

personalSettings :: ByteString
personalSettings =
    ByteString.unlines
        [ "version: 1"
        , "display:"
        , "  commodity: \"USD\""
        , "  fiscalYearStartMonth: 1"
        , "investments:"
        , "  gainMappings:"
        , "    - assetAccount: \"assets:equity\""
        , "      gainAccounts: [\"income:dividend\"]"
        , "  assetClasses:"
        , "    - kind: equity"
        , "      accountPrefixes: [assets:equity]"
        , "      targetPercent: 100"
        ]

businessSettings :: ByteString
businessSettings =
    ByteString.unlines
        [ "version: 1"
        , "display:"
        , "  commodity: \"EUR\""
        , "  fiscalYearStartMonth: 7"
        ]

withTempConfigDir :: (FilePath -> IO a) -> IO a
withTempConfigDir action = do
    parent <- getTemporaryDirectory
    (path, handle) <- openTempFile parent "hedger-config-XXXXXX"
    hClose handle
    removeFile path
    createDirectory path
    action path `finally` removeDirectoryRecursive path

requireLedgerId :: Text -> LedgerId
requireLedgerId value =
    case mkLedgerId value of
        Right ledgerId -> ledgerId
        Left problem -> error (Text.unpack problem)

appliesReferencedSettings :: IO ()
appliesReferencedSettings = withTempConfigDir $ \directory -> do
    ByteString.writeFile (directory </> "personal.settings.yaml") personalSettings
    ByteString.writeFile (directory </> "business.settings.yaml") businessSettings
    ByteString.writeFile (directory </> "catalog.yaml") catalogDocument
    catalog <-
        either (assertFailure . Text.unpack) pure
            =<< loadCatalogFile (directory </> "catalog.yaml")
    let Just personal = Map.lookup (requireLedgerId "personal") catalog.ledgers
    assertEqual "personal commodity" "USD" personal.ledgerConfig.defaultDisplayCommodity
    assertEqual
        "personal fiscal month"
        1
        (monthOfYearValue personal.ledgerConfig.fiscalYearStartMonth)
    case Investment.mappingForAccount "assets:equity" personal.ledgerConfig.investmentMappings of
        Nothing -> assertFailure "expected equity gain mapping"
        Just mapping ->
            assertEqual
                "personal gain accounts"
                ["income:dividend"]
                (Investment.accountNameText . (.account) <$> mapping.performanceAccounts)
    assertEqual
        "personal asset class"
        Investment.Equity
        ( Investment.assetClassForAccount
            "assets:equity:holding"
            personal.ledgerConfig.assetClassMappings
        )

resolvesRelativePathsAcrossCwd :: IO ()
resolvesRelativePathsAcrossCwd = withTempConfigDir $ \directory -> do
    let base = directory </> "base"
    createDirectory base
    ByteString.writeFile (base </> "personal.settings.yaml") personalSettings
    ByteString.writeFile (base </> "business.settings.yaml") businessSettings
    ByteString.writeFile (base </> "personal.journal") "2024-01-01 open assets:cash\n"
    ByteString.writeFile (base </> "catalog.yaml") catalogDocument
    result <- loadCatalogFile (base </> "catalog.yaml")
    case result of
        Left problem -> assertFailure (Text.unpack problem)
        Right catalog -> do
            let Just personal = Map.lookup (requireLedgerId "personal") catalog.ledgers
            assertEqual
                "journal path"
                (base </> "personal.journal")
                (Text.unpack personal.ledgerConfig.journalPath)

rejectsMissingSettingsFile :: IO ()
rejectsMissingSettingsFile = withTempConfigDir $ \directory -> do
    ByteString.writeFile (directory </> "catalog.yaml") catalogDocument
    result <- loadCatalogFile (directory </> "catalog.yaml")
    case result of
        Left problem -> do
            assertBool "mentions ledger id" ("personal" `Text.isInfixOf` problem)
            assertBool
                "mentions resolved path"
                ( Text.pack (directory </> "personal.settings.yaml")
                    `Text.isInfixOf` problem
                )
        Right _ -> assertFailure "expected failure"

rejectsMalformedSettings :: IO ()
rejectsMalformedSettings = withTempConfigDir $ \directory -> do
    ByteString.writeFile (directory </> "personal.settings.yaml") "display: ["
    ByteString.writeFile (directory </> "catalog.yaml") catalogDocument
    result <- loadCatalogFile (directory </> "catalog.yaml")
    case result of
        Left problem -> do
            assertBool "mentions ledger id" ("personal" `Text.isInfixOf` problem)
            assertBool
                "mentions resolved path"
                ( Text.pack (directory </> "personal.settings.yaml")
                    `Text.isInfixOf` problem
                )
        Right _ -> assertFailure "expected failure"

rejectsWrongSettingsVersion :: IO ()
rejectsWrongSettingsVersion = withTempConfigDir $ \directory -> do
    ByteString.writeFile
        (directory </> "personal.settings.yaml")
        "version: 2\ndisplay:\n  commodity: USD\n"
    ByteString.writeFile (directory </> "catalog.yaml") catalogDocument
    result <- loadCatalogFile (directory </> "catalog.yaml")
    case result of
        Left problem -> do
            assertBool "mentions ledger id" ("personal" `Text.isInfixOf` problem)
            assertBool
                "mentions resolved path"
                ( Text.pack (directory </> "personal.settings.yaml")
                    `Text.isInfixOf` problem
                )
        Right _ -> assertFailure "expected failure"

rejectsDeploymentKeysInSettings :: IO ()
rejectsDeploymentKeysInSettings = withTempConfigDir $ \directory -> do
    ByteString.writeFile
        (directory </> "personal.settings.yaml")
        ( ByteString.unlines
            [ "version: 1"
            , "journalPath: x.journal"
            , "display:"
            , "  commodity: USD"
            ]
        )
    ByteString.writeFile (directory </> "catalog.yaml") catalogDocument
    result <- loadCatalogFile (directory </> "catalog.yaml")
    assertBool "deployment keys rejected" (isLeft result)

rejectsBlankConfigPath :: IO ()
rejectsBlankConfigPath = withTempConfigDir $ \directory -> do
    let content =
            ByteString.unlines
                [ "version: 3"
                , "ledgers:"
                , "  - id: personal"
                , "    label: Personal"
                , "    journalPath: personal.journal"
                , "    configPath: \"\""
                ]
    ByteString.writeFile (directory </> "catalog.yaml") content
    result <- loadCatalogFile (directory </> "catalog.yaml")
    case result of
        Left problem ->
            assertBool
                "blank config path error"
                ("A non-empty config path is required." `Text.isInfixOf` problem)
        Right _ -> assertFailure "expected failure"

rejectsWhitespaceJournalPath :: IO ()
rejectsWhitespaceJournalPath = withTempConfigDir $ \directory -> do
    let content =
            ByteString.unlines
                [ "version: 3"
                , "ledgers:"
                , "  - id: personal"
                , "    label: Personal"
                , "    journalPath: \"   \""
                , "    configPath: personal.settings.yaml"
                ]
    ByteString.writeFile (directory </> "catalog.yaml") content
    result <- loadCatalogFile (directory </> "catalog.yaml")
    case result of
        Left problem ->
            assertBool
                "whitespace journal path error"
                ("A non-empty journal path is required." `Text.isInfixOf` problem)
        Right _ -> assertFailure "expected failure"

rejectsMixedLedgerKeys :: IO ()
rejectsMixedLedgerKeys =
    assertBool
        "mixed ledger keys rejected"
        (isLeft (decodeCatalogFileConfig mixedLedgerDocument))

mixedLedgerDocument :: ByteString
mixedLedgerDocument =
    ByteString.unlines
        [ "version: 3"
        , "ledgers:"
        , "  - id: personal"
        , "    label: Personal"
        , "    journalPath: personal.journal"
        , "    configPath: personal.settings.yaml"
        , "    display:"
        , "      commodity: USD"
        ]
