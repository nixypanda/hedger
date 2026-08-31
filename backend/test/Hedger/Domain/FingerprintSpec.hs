module Hedger.Domain.FingerprintSpec (fingerprintSpec) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Calendar (Day)
import Data.Time.Calendar qualified as Calendar
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Fingerprint (
    ChainDiff (..),
    GlobalFingerprint (..),
    PrefixChain (..),
    diffChains,
    globalFingerprint,
    prefixChain,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)
import Hedger.TestHelpers (configuration)

fingerprintSpec :: TestTree
fingerprintSpec =
    testGroup
        "fingerprint"
        [ testCase
            "reformatting whitespace and comments leaves the chain unchanged"
            reformatKeepsChain
        , testCase
            "appending a transaction diverges only at the last month"
            appendDivergesAtLastMonth
        , testCase
            "inserting a historical transaction diverges at its month"
            historicalTransactionDiverges
        , testCase
            "inserting a historical price directive diverges at its month"
            historicalPriceDiverges
        , testCase
            "changing a commodity's decimal places changes the global fingerprint"
            decimalPlacesChangeGlobal
        , testCase
            "two identical journals produce an identical result"
            identicalJournalsAgree
        , testCase
            "reverting an edit reproduces the original chain"
            revertReproducesChain
        ]

{- | The chain must survive a reformat: source positions, indentation, blank
lines and free-standing comments are not journal content.
-}
reformatKeepsChain :: IO ()
reformatKeepsChain =
    withJournalPath "reformat" $ \path -> do
        original <- chainFor path baseJournal
        assertEqual
            "the chain should hold one entry per populated month"
            [januaryEnd, februaryEnd, marchEnd]
            (fst <$> original.months)
        reformatted <- chainFor path reformattedJournal
        assertEqual "reformatting must not change the chain" original reformatted
        assertEqual "reformatting must diff as identical" Identical (diffChains original reformatted)

appendDivergesAtLastMonth :: IO ()
appendDivergesAtLastMonth =
    withJournalPath "append" $ \path -> do
        original <- chainFor path baseJournal
        appended <- chainFor path appendedJournal
        assertEqual
            "an append must diverge at the last month"
            (DivergesFrom marchEnd)
            (diffChains original appended)
        assertEqual
            "months before the append must be untouched"
            (take 2 original.months)
            (take 2 appended.months)

historicalTransactionDiverges :: IO ()
historicalTransactionDiverges =
    withJournalPath "historical-transaction" $ \path -> do
        original <- chainFor path baseJournal
        edited <- chainFor path historicalTransactionJournal
        assertEqual
            "a historical transaction must diverge at its own month"
            (DivergesFrom januaryEnd)
            (diffChains original edited)

{- | The failure mode with no other symptom: a price directive inserted in the
past silently restates historical valuations, so it must invalidate its month.
-}
historicalPriceDiverges :: IO ()
historicalPriceDiverges =
    withJournalPath "historical-price" $ \path -> do
        original <- chainFor path baseJournal
        edited <- chainFor path historicalPriceJournal
        assertEqual
            "a historical price directive must diverge at its own month"
            (DivergesFrom januaryEnd)
            (diffChains original edited)

decimalPlacesChangeGlobal :: IO ()
decimalPlacesChangeGlobal =
    withJournalPath "decimal-places" $ \path -> do
        original <- chainFor path baseJournal
        restyled <- chainFor path declaredCommodityJournal
        assertBool
            "declaring decimal places must change the commodity style hash"
            (original.global.commodityStyles /= restyled.global.commodityStyles)
        assertEqual
            "a display-style change must invalidate the whole chain"
            GlobalChanged
            (diffChains original restyled)
        assertEqual "the month chain itself must be unaffected" original.months restyled.months

identicalJournalsAgree :: IO ()
identicalJournalsAgree =
    withJournalPath "identical" $ \path -> do
        earlier <- chainFor path baseJournal
        later <- chainFor path baseJournal
        assertEqual "identical journals must fingerprint identically" earlier later
        assertEqual "identical journals must diff as identical" Identical (diffChains earlier later)

-- | Content addressing has to make an undo a cache hit rather than a rebuild.
revertReproducesChain :: IO ()
revertReproducesChain =
    withJournalPath "revert" $ \path -> do
        original <- chainFor path baseJournal
        edited <- chainFor path appendedJournal
        reverted <- chainFor path baseJournal
        assertEqual "a revert must reproduce the original chain" original reverted
        assertEqual "a revert must diff as identical" Identical (diffChains original reverted)
        assertEqual
            "the edit it reverts must still diverge"
            (DivergesFrom marchEnd)
            (diffChains edited reverted)

januaryEnd :: Day
januaryEnd = Calendar.fromGregorian 2023 1 31

februaryEnd :: Day
februaryEnd = Calendar.fromGregorian 2023 2 28

marchEnd :: Day
marchEnd = Calendar.fromGregorian 2023 3 31

{- | Writes the journal at @path@ and fingerprints it through the same parse
path the snapshot store uses in production.
-}
chainFor :: FilePath -> Text -> IO PrefixChain
chainFor path contents = do
    TextIO.writeFile path contents
    loaded <- loadSnapshot (Text.pack path)
    case loaded of
        Left problem -> assertFailure ("journal load failed: " <> show problem)
        Right snapshot -> pure (chainOf snapshot.journal)

chainOf :: Hledger.Journal -> PrefixChain
chainOf journal =
    prefixChain (globalFingerprint journal configuration) journal.jtxns journal.jpricedirectives

{- | One directory per case, and one path within it, so that the global
fingerprint's file-set component is held constant across a case's revisions.
-}
withJournalPath :: String -> (FilePath -> IO a) -> IO a
withJournalPath label action =
    bracket acquire Directory.removeDirectoryRecursive (\dir -> action (dir </> "main.journal"))
  where
    acquire = do
        base <- Directory.getTemporaryDirectory
        let dir = base </> ("hedger-fingerprint-spec-" <> label)
        exists <- Directory.doesDirectoryExist dir
        when exists (Directory.removeDirectoryRecursive dir)
        Directory.createDirectoryIfMissing True dir
        pure dir

baseJournal :: Text
baseJournal =
    Text.unlines
        [ "2023-01-15 salary"
        , "    assets:bank:checking   1000.00 INR"
        , "    income:salary         -1000.00 INR"
        , ""
        , "2023-02-20 groceries"
        , "    expenses:food            50.00 INR"
        , "    assets:bank:checking    -50.00 INR"
        , ""
        , "P 2023-02-28 VUSA 90.00 INR"
        , ""
        , "2023-03-10 buy fund"
        , "    assets:investments:vusa    1.00 VUSA @ 100.00 INR"
        , "    assets:bank:checking    -100.00 INR"
        ]

{- | 'baseJournal' with a leading comment, different indentation, different
amount alignment and no blank separators. Digits are untouched, because
changing them would change the inferred display style.
-}
reformattedJournal :: Text
reformattedJournal =
    Text.unlines
        [ "; a comment added at the top of the file"
        , ""
        , ""
        , "2023-01-15 salary"
        , "        assets:bank:checking  1000.00 INR"
        , "        income:salary  -1000.00 INR"
        , "2023-02-20 groceries"
        , "        expenses:food  50.00 INR"
        , "        assets:bank:checking  -50.00 INR"
        , "P 2023-02-28 VUSA 90.00 INR"
        , "2023-03-10 buy fund"
        , "        assets:investments:vusa  1.00 VUSA @ 100.00 INR"
        , "        assets:bank:checking  -100.00 INR"
        ]

appendedJournal :: Text
appendedJournal =
    baseJournal
        <> Text.unlines
            [ ""
            , "2023-03-25 dinner"
            , "    expenses:food             20.00 INR"
            , "    assets:bank:checking     -20.00 INR"
            ]

historicalTransactionJournal :: Text
historicalTransactionJournal =
    Text.unlines
        [ "2023-01-20 books"
        , "    expenses:books            30.00 INR"
        , "    assets:bank:checking     -30.00 INR"
        , ""
        ]
        <> baseJournal

historicalPriceJournal :: Text
historicalPriceJournal = "P 2023-01-25 VUSA 80.00 INR\n\n" <> baseJournal

{- | Declares three decimal places for INR without touching any transaction, so
only the global fingerprint may move.
-}
declaredCommodityJournal :: Text
declaredCommodityJournal = "commodity 1000.000 INR\n\n" <> baseJournal
