module Hedger.Config (
    Config (..),
    LogLevel (..),
    loadConfig,
    parseConfig,
)
where

import Control.Applicative ((<|>))
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative qualified as Options
import System.Environment qualified as Environment
import Text.Read qualified as Read

import Hedger.Domain.Investment qualified as Investment

data LogLevel = Debug | Info | Warn | Error
    deriving stock (Eq, Ord, Show)

data Config = Config
    { journalPath :: Text
    , listenAddress :: Text
    , listenPort :: Int
    , defaultDisplayCommodity :: Text
    , fiscalYearStartMonth :: Int
    , logLevel :: LogLevel
    , publicBaseUrl :: Maybe Text
    , investmentMappings :: Investment.InvestmentMappings
    }
    deriving stock (Eq, Show)

data CliOptions = CliOptions
    { cliJournalPath :: Maybe Text
    , cliListenAddress :: Maybe Text
    , cliListenPort :: Maybe Int
    , cliDisplayCommodity :: Maybe Text
    , cliFiscalYearStartMonth :: Maybe Int
    , cliLogLevel :: Maybe LogLevel
    , cliPublicBaseUrl :: Maybe Text
    }

loadConfig :: IO (Either Text Config)
loadConfig = do
    environment <- fmap (fmap (bimap Text.pack Text.pack)) Environment.getEnvironment
    cli <- Options.execParser parserInfo
    pure (resolveConfig environment cli)
  where
    bimap mapFirst mapSecond (first, second) = (mapFirst first, mapSecond second)

parseConfig :: [(Text, Text)] -> [Text] -> Either Text Config
parseConfig environment arguments = do
    cli <- parseArguments arguments
    resolveConfig environment cli

resolveConfig :: [(Text, Text)] -> CliOptions -> Either Text Config
resolveConfig environment cli = do
    configuredPort <- cli.cliListenPort `orEnvironmentParsed` "HEDGER_PORT"
    configuredFiscal <- cli.cliFiscalYearStartMonth `orEnvironmentParsed` "HEDGER_FISCAL_YEAR_START"
    configuredLevel <- cli.cliLogLevel `orEnvironmentLogLevel` "HEDGER_LOG_LEVEL"
    let port = configuredPort `orDefault` 8080
        fiscal = configuredFiscal `orDefault` 4
        level = configuredLevel `orDefault` Info
        path = (cli.cliJournalPath <|> lookup "HEDGER_JOURNAL_PATH" environment) `orDefault` ""
    mappings <- temporaryInvestmentMappings path
    validate
        Config
            { journalPath = path
            , listenAddress =
                (cli.cliListenAddress <|> lookup "HEDGER_LISTEN_ADDRESS" environment) `orDefault` "127.0.0.1"
            , listenPort = port
            , defaultDisplayCommodity =
                (cli.cliDisplayCommodity <|> lookup "HEDGER_DISPLAY_COMMODITY" environment) `orDefault` "INR"
            , fiscalYearStartMonth = fiscal
            , logLevel = level
            , publicBaseUrl = cli.cliPublicBaseUrl <|> lookup "HEDGER_PUBLIC_BASE_URL" environment
            , investmentMappings = mappings
            }
  where
    cliValue `orEnvironmentParsed` name =
        case cliValue of
            Just value -> Right (Just value)
            Nothing -> traverse (parseInteger name) (lookup name environment)

    cliValue `orEnvironmentLogLevel` name =
        case cliValue of
            Just value -> Right (Just value)
            Nothing -> traverse parseLogLevel (lookup name environment)

temporaryInvestmentMappings :: Text -> Either Text Investment.InvestmentMappings
temporaryInvestmentMappings journalPath
    | "india-us-portfolio-2023-2025.journal" `Text.isSuffixOf` journalPath =
        Investment.mkInvestmentMappings
            [ mapping "assets:debt" []
            , mapping
                "assets:debt:epf:india:nimbusworks"
                [ performance "income:interest:epf" Investment.Interest
                ]
            , mapping
                "assets:debt:eps:india:nimbusworks"
                [performance "income:interest:eps" Investment.Interest]
            , mapping
                "assets:debt:ppf:india:varuna"
                [performance "income:interest:ppf" Investment.Interest]
            , mapping "assets:hybrid" []
            , mapping
                "assets:hybrid:india:nps:monsoon"
                [performance "income:interest:nps" Investment.Interest]
            , mapping
                "assets:equity"
                [ performance "income:dividend:india" Investment.Dividend
                , performance "income:dividend:us" Investment.Dividend
                , performance "expenses:financial:brokerage:india" Investment.Fee
                , performance "expenses:financial:brokerage:us" Investment.Fee
                ]
            , mapping "assets:liquid" []
            , mapping "assets:gold" []
            , mapping "assets:property" []
            ]
    | otherwise = Right Investment.emptyInvestmentMappings
  where
    mapping = Investment.InvestmentMapping
    performance = Investment.PerformanceAccount

orDefault :: Maybe value -> value -> value
orDefault configured fallback = maybe fallback id configured

parseArguments :: [Text] -> Either Text CliOptions
parseArguments arguments =
    case Options.execParserPure Options.defaultPrefs parserInfo (Text.unpack <$> arguments) of
        Options.Success options -> Right options
        Options.Failure failure -> Left . Text.pack . fst $ Options.renderFailure failure "hedger-server"
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
        <$> optionalText "journal" "PATH" "Runtime path to the root journal"
        <*> optionalText "listen-address" "ADDRESS" "Listen address (default: 127.0.0.1)"
        <*> optionalAuto "port" "PORT" "Listen port (default: 8080)"
        <*> optionalText "display-commodity" "COMMODITY" "Default display commodity (default: INR)"
        <*> optionalAuto "fiscal-year-start" "MONTH" "Fiscal-year start month (default: 4)"
        <*> Options.optional
            ( Options.option
                (Options.eitherReader (firstText . parseLogLevel . Text.pack))
                ( Options.long "log-level"
                    <> Options.metavar "LEVEL"
                    <> Options.help "debug|info|warn|error (default: info)"
                )
            )
        <*> optionalText "public-base-url" "URL" "Optional absolute public base URL"
  where
    firstText = either (Left . Text.unpack) Right

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
    validatePublicBaseUrl config.publicBaseUrl
    pure config

requireNonBlank :: Text -> Text -> Either Text ()
requireNonBlank label value
    | Text.any (not . Char.isSpace) value = Right ()
    | otherwise = Left ("A non-empty " <> label <> " is required.")

requireToken :: Text -> Text -> Either Text ()
requireToken label value
    | not (Text.null value) && not (Text.any Char.isSpace value) = Right ()
    | otherwise = Left ("A non-empty " <> label <> " is required.")

validatePublicBaseUrl :: Maybe Text -> Either Text ()
validatePublicBaseUrl Nothing = Right ()
validatePublicBaseUrl (Just url)
    | isHttpPrefix && not (Text.any Char.isSpace url) =
        Right ()
    | otherwise = Left "Public base URL must be an absolute http(s) URL."
  where
    isHttpPrefix = "http://" `Text.isPrefixOf` url || "https://" `Text.isPrefixOf` url

parseInteger :: Text -> Text -> Either Text Int
parseInteger label value =
    maybe (Left (label <> " must be an integer.")) Right (Read.readMaybe (Text.unpack value))

parseLogLevel :: Text -> Either Text LogLevel
parseLogLevel value =
    case Text.toLower value of
        "debug" -> Right Debug
        "info" -> Right Info
        "warn" -> Right Warn
        "error" -> Right Error
        _ -> Left "Log level must be one of debug, info, warn, or error."
