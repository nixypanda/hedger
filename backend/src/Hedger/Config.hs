module Hedger.Config (
    Config (..),
    loadConfig,
    parseConfig,
) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, displayException, try)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NonEmpty
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Options.Applicative qualified as Options

import Hedger.Domain.Investment qualified as Investment

-- * Resolved configuration
data Config = Config
    { journalPath :: Text
    , listenAddress :: Text
    , listenPort :: Int
    , defaultDisplayCommodity :: Text
    , fiscalYearStartMonth :: Int
    , investmentMappings :: Investment.InvestmentMappings
    , assetClassMappings :: Investment.AssetClassMappings
    }
    deriving stock (Eq, Show)

-- * Boundary configuration types
data FileConfig = FileConfig
    { version :: Int
    , journal :: Maybe JournalFileConfig
    , server :: Maybe ServerFileConfig
    , display :: Maybe DisplayFileConfig
    , investments :: Maybe InvestmentsFileConfig
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data JournalFileConfig = JournalFileConfig
    { path :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ServerFileConfig = ServerFileConfig
    { listenAddress :: Maybe Text
    , port :: Maybe Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data DisplayFileConfig = DisplayFileConfig
    { commodity :: Maybe Text
    , fiscalYearStartMonth :: Maybe Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data InvestmentsFileConfig = InvestmentsFileConfig
    { gainMappings :: Maybe [GainMappingFileConfig]
    , assetClasses :: Maybe [AssetClassFileConfig]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GainMappingFileConfig = GainMappingFileConfig
    { assetAccount :: Text
    , gainAccounts :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data AssetClassFileConfig = AssetClassFileConfig
    { kind :: Text
    , accountPrefixes :: [Text]
    , targetPercent :: Maybe Scientific
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data CliOptions = CliOptions
    { cliConfigPath :: Maybe Text
    , cliJournalPath :: Maybe Text
    , cliListenAddress :: Maybe Text
    , cliListenPort :: Maybe Int
    , cliDisplayCommodity :: Maybe Text
    , cliFiscalYearStartMonth :: Maybe Int
    }

-- * Public entry points
loadConfig :: IO (Either Text Config)
loadConfig = do
    cli <- Options.execParser parserInfo
    case cli.cliConfigPath of
        Nothing -> pure (resolveConfig emptyFileConfig cli)
        Just configPath -> do
            loaded <- readConfigurationFile (Text.unpack configPath)
            pure $
                case loaded of
                    Left problem ->
                        Left
                            ( "Could not read configuration file "
                                <> configPath
                                <> ": "
                                <> Text.pack (displayException problem)
                            )
                    Right contents -> decodeFileConfig contents >>= (`resolveConfig` cli)

parseConfig :: Maybe ByteString -> [Text] -> Either Text Config
parseConfig document arguments = do
    cli <- parseArguments arguments
    fileConfig <- maybe (Right emptyFileConfig) decodeFileConfig document
    resolveConfig fileConfig cli

-- * Configuration file parsing
readConfigurationFile :: FilePath -> IO (Either IOException ByteString)
readConfigurationFile = try . ByteString.readFile

decodeFileConfig :: ByteString -> Either Text FileConfig
decodeFileConfig contents =
    case Yaml.decodeEither' contents of
        Left problem -> Left (Text.pack (Yaml.prettyPrintParseException problem))
        Right fileConfig
            | fileConfig.version == 1 -> Right fileConfig
            | otherwise -> Left "Configuration version must be 1."

emptyFileConfig :: FileConfig
emptyFileConfig =
    FileConfig
        { version = 1
        , journal = Nothing
        , server = Nothing
        , display = Nothing
        , investments = Nothing
        }

-- * Configuration resolution
resolveConfig :: FileConfig -> CliOptions -> Either Text Config
resolveConfig fileConfig cli = do
    mappings <- investmentMappingsFrom fileConfig.investments
    assetClasses <- assetClassMappingsFrom fileConfig.investments
    validate
        Config
            { journalPath = (cli.cliJournalPath <|> ((.path) <$> fileConfig.journal)) `orDefault` ""
            , listenAddress =
                (cli.cliListenAddress <|> (fileConfig.server >>= (.listenAddress)))
                    `orDefault` "127.0.0.1"
            , listenPort = (cli.cliListenPort <|> (fileConfig.server >>= (.port))) `orDefault` 8080
            , defaultDisplayCommodity =
                (cli.cliDisplayCommodity <|> (fileConfig.display >>= (.commodity)))
                    `orDefault` "INR"
            , fiscalYearStartMonth =
                (cli.cliFiscalYearStartMonth <|> (fileConfig.display >>= (.fiscalYearStartMonth)))
                    `orDefault` 4
            , investmentMappings = mappings
            , assetClassMappings = assetClasses
            }

investmentMappingsFrom :: Maybe InvestmentsFileConfig -> Either Text Investment.InvestmentMappings
investmentMappingsFrom Nothing = Right Investment.emptyInvestmentMappings
investmentMappingsFrom (Just configured) = do
    mappings <- traverse investmentMappingFrom (orDefault configured.gainMappings [])
    Investment.mkInvestmentMappings mappings

assetClassMappingsFrom :: Maybe InvestmentsFileConfig -> Either Text Investment.AssetClassMappings
assetClassMappingsFrom Nothing = Right Investment.emptyAssetClassMappings
assetClassMappingsFrom (Just configured) = do
    mappings <- traverse assetClassMappingFrom (orDefault configured.assetClasses [])
    Investment.mkAssetClassMappings mappings

investmentMappingFrom :: GainMappingFileConfig -> Either Text Investment.InvestmentMapping
investmentMappingFrom configured = do
    assetAccount <- Investment.mkAssetAccount configured.assetAccount
    performanceAccounts <-
        traverse
            ( \name -> do
                account <- Investment.mkAccountName name
                pure Investment.PerformanceAccount{account, role = Investment.OtherIncome}
            )
            configured.gainAccounts
    pure Investment.InvestmentMapping{assetAccount, performanceAccounts}

assetClassMappingFrom :: AssetClassFileConfig -> Either Text Investment.AssetClassMapping
assetClassMappingFrom configured = do
    assetClass <- assetClassFrom configured.kind
    assetPrefixes <- traverse Investment.mkAssetAccount configured.accountPrefixes
    accountPrefixes <-
        maybe
            (Left "Each asset class mapping requires at least one account prefix.")
            Right
            (NonEmpty.nonEmpty assetPrefixes)
    targetPercentage <-
        traverse (Investment.mkPercentage . realToFrac) configured.targetPercent
    pure Investment.AssetClassMapping{assetClass, accountPrefixes, targetPercentage}

assetClassFrom :: Text -> Either Text Investment.AssetClass
assetClassFrom kind =
    case kind of
        "equity" -> Right Investment.Equity
        "debt" -> Right Investment.Debt
        "cash" -> Right Investment.Cash
        "gold" -> Right Investment.Gold
        "hybrid" -> Right Investment.Hybrid
        "property" -> Right Investment.Property
        "other" -> Right Investment.Other
        _ -> Left ("Unsupported asset class kind: " <> kind)

validate :: Config -> Either Text Config
validate config = do
    requireNonBlank "journal path" config.journalPath
    requireToken "listen address" config.listenAddress
    requireToken "display commodity" config.defaultDisplayCommodity
    if
        | config.listenPort < 1 || config.listenPort > 65535 ->
            Left "Port must be between 1 and 65535."
        | config.fiscalYearStartMonth < 1 || config.fiscalYearStartMonth > 12 ->
            Left "Fiscal-year start must be between 1 and 12."
        | otherwise -> pure ()
    pure config

requireNonBlank :: Text -> Text -> Either Text ()
requireNonBlank label value
    | Text.any (not . Char.isSpace) value = Right ()
    | otherwise = Left ("A non-empty " <> label <> " is required.")

requireToken :: Text -> Text -> Either Text ()
requireToken label value
    | not (Text.null value) && not (Text.any Char.isSpace value) = Right ()
    | otherwise = Left ("A non-empty " <> label <> " is required.")

orDefault :: Maybe value -> value -> value
orDefault configured fallback = maybe fallback id configured

-- * Command-line parsing
parseArguments :: [Text] -> Either Text CliOptions
parseArguments arguments =
    case Options.execParserPure Options.defaultPrefs parserInfo (Text.unpack <$> arguments) of
        Options.Success options -> Right options
        Options.Failure failure ->
            Left . Text.pack . fst $ Options.renderFailure failure "hedger-server"
        Options.CompletionInvoked _ ->
            Left "Shell completion is not available through the pure configuration parser."

parserInfo :: Options.ParserInfo CliOptions
parserInfo =
    Options.info
        (Options.helper <*> cliOptionsParser)
        ( Options.fullDesc
            <> Options.progDesc "Run the Hedger read-only ledger backend"
            <> Options.header "hedger-server"
        )

cliOptionsParser :: Options.Parser CliOptions
cliOptionsParser =
    CliOptions
        <$> optionalText "config" "PATH" "Path to the YAML configuration file"
        <*> optionalText "journal" "PATH" "Runtime path to the root journal"
        <*> optionalText "listen-address" "ADDRESS" "Listen address (default: 127.0.0.1)"
        <*> optionalAuto "port" "PORT" "Listen port (default: 8080)"
        <*> optionalText "display-commodity" "COMMODITY" "Default display commodity (default: INR)"
        <*> optionalAuto "fiscal-year-start" "MONTH" "Fiscal-year start month (default: 4)"

optionalText :: Text -> Text -> Text -> Options.Parser (Maybe Text)
optionalText name valueName description =
    Options.optional
        ( Text.pack
            <$> Options.strOption
                ( Options.long (Text.unpack name)
                    <> Options.metavar (Text.unpack valueName)
                    <> Options.help (Text.unpack description)
                )
        )

optionalAuto :: (Read value) => Text -> Text -> Text -> Options.Parser (Maybe value)
optionalAuto name valueName description =
    Options.optional
        ( Options.option
            Options.auto
            ( Options.long (Text.unpack name)
                <> Options.metavar (Text.unpack valueName)
                <> Options.help (Text.unpack description)
            )
        )
