module Hedger.Domain.FingerprintSpec (fingerprintSpec) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Foldable (traverse_)
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
import Hledger.Utils.Parse qualified as HledgerParse

import Hedger.Config (Config (..))
import Hedger.Domain.Fingerprint (
    ChainDiff (..),
    FamilyChains (..),
    GlobalFingerprint (..),
    Hash,
    PrefixChain (..),
    ReportFamily (..),
    chainFor,
    diffChains,
    diffFamilyChains,
    everyFamily,
    familyChains,
    globalFingerprint,
    prefixChain,
 )
import Hedger.Domain.Internal.FamilyRelevance (familyRelevance)
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
        , scopedInvalidationSpec
        , filePrefilterSpec
        ]

{- | The chain must survive a reformat: source positions, indentation, blank
lines and free-standing comments are not journal content.
-}
reformatKeepsChain :: IO ()
reformatKeepsChain =
    withJournalPath "reformat" $ \path -> do
        original <- chainAt path baseJournal
        assertEqual
            "the chain should hold one entry per populated month"
            [januaryEnd, februaryEnd, marchEnd]
            (fst <$> original.months)
        reformatted <- chainAt path reformattedJournal
        assertEqual "reformatting must not change the chain" original reformatted
        assertEqual "reformatting must diff as identical" Identical (diffChains original reformatted)

appendDivergesAtLastMonth :: IO ()
appendDivergesAtLastMonth =
    withJournalPath "append" $ \path -> do
        original <- chainAt path baseJournal
        appended <- chainAt path appendedJournal
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
        original <- chainAt path baseJournal
        edited <- chainAt path historicalTransactionJournal
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
        original <- chainAt path baseJournal
        edited <- chainAt path historicalPriceJournal
        assertEqual
            "a historical price directive must diverge at its own month"
            (DivergesFrom januaryEnd)
            (diffChains original edited)

decimalPlacesChangeGlobal :: IO ()
decimalPlacesChangeGlobal =
    withJournalPath "decimal-places" $ \path -> do
        original <- chainAt path baseJournal
        restyled <- chainAt path declaredCommodityJournal
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
        earlier <- chainAt path baseJournal
        later <- chainAt path baseJournal
        assertEqual "identical journals must fingerprint identically" earlier later
        assertEqual "identical journals must diff as identical" Identical (diffChains earlier later)

-- | Content addressing has to make an undo a cache hit rather than a rebuild.
revertReproducesChain :: IO ()
revertReproducesChain =
    withJournalPath "revert" $ \path -> do
        original <- chainAt path baseJournal
        edited <- chainAt path appendedJournal
        reverted <- chainAt path baseJournal
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
chainAt :: FilePath -> Text -> IO PrefixChain
chainAt path contents = do
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
withJournalPath label action = withJournalDir label (\dir -> action (dir </> "main.journal"))

withJournalDir :: String -> (FilePath -> IO a) -> IO a
withJournalDir label action =
    bracket acquire Directory.removeDirectoryRecursive action
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

-- * Scoped invalidation: one chain per report family

{- | Each family's chain must diverge exactly when an edit can reach that family,
and every case is asserted in /both/ directions. The permissive direction is the
dangerous one: a family that wrongly reports 'Identical' keeps checkpoints that
have been restated, which is wrong numbers with no error, so every case below also
names the families that must diverge.

Note what double entry does to the net-worth family. An ordinary expense or
income transaction has a cash or bank leg, so it /is/ net-worth relevant and does
truncate it; only an edit whose every posting is income or expense — a
recategorisation — is spared. The two cases below cover both.
-}
scopedInvalidationSpec :: TestTree
scopedInvalidationSpec =
    testGroup
        "per-family chains"
        [ testCase
            "an expense-only edit spares net worth and investments"
            expenseOnlyEditSparesOtherFamilies
        , testCase
            "an asset-only transfer edit spares cashflow"
            assetOnlyEditSparesCashflow
        , testCase
            "an investment edit truncates investments and net worth but not cashflow"
            investmentEditTruncatesInvestments
        , testCase
            "an income edit truncates cashflow and net worth but not investments"
            incomeEditSparesInvestments
        , testCase
            "a historical price directive truncates every family"
            historicalPriceTruncatesEveryFamily
        , testCase
            "a global fingerprint change invalidates every family"
            globalChangeInvalidatesEveryFamily
        , testCase
            "every family chain has an entry for every month of the whole chain"
            familyChainsAreAlignedWithTheWhole
        ]

{- | Recategorising one expense as another: both postings are expense accounts,
so no asset, cash or liability row and no investment account is touched.
-}
expenseOnlyEditSparesOtherFamilies :: IO ()
expenseOnlyEditSparesOtherFamilies =
    assertFamilyDiffs
        "expense-only"
        (familyEdit "    expenses:dining" "    expenses:entertainment")
        [ (CashflowFamily, DivergesFrom februaryEnd)
        , (NetWorthFamily, Identical)
        , (InvestmentFamily, Identical)
        ]

{- | Moving money between two bank accounts: both postings are assets, so no
income or expense row moves.
-}
assetOnlyEditSparesCashflow :: IO ()
assetOnlyEditSparesCashflow =
    assertFamilyDiffs
        "asset-only"
        (familyEdit "    assets:bank:savings" "    assets:bank:reserve")
        [ (NetWorthFamily, DivergesFrom marchEnd)
        , (CashflowFamily, Identical)
        , (InvestmentFamily, Identical)
        ]

{- | The over-eager-scoping direction for investments: an edit inside the
portfolio transaction must reach the investment family, and net worth too because
its cash leg is an asset.
-}
investmentEditTruncatesInvestments :: IO ()
investmentEditTruncatesInvestments =
    assertFamilyDiffs
        "investment"
        (familyEdit "2023-04-10 buy fund" "2023-04-10 buy fund again")
        [ (InvestmentFamily, DivergesFrom aprilEnd)
        , (NetWorthFamily, DivergesFrom aprilEnd)
        , (CashflowFamily, Identical)
        ]

{- | The same direction for net worth. A salary transaction credits a bank
account, so net worth cannot be spared here however tempting "income edit" makes
it sound; the investment family still can be.
-}
incomeEditSparesInvestments :: IO ()
incomeEditSparesInvestments =
    assertFamilyDiffs
        "income"
        (familyEdit "    income:salary" "    income:bonus")
        [ (CashflowFamily, DivergesFrom januaryEnd)
        , (NetWorthFamily, DivergesFrom januaryEnd)
        , (InvestmentFamily, Identical)
        ]

{- | A price directive values every family's columns, so it can restate any of
them whatever accounts it names.
-}
historicalPriceTruncatesEveryFamily :: IO ()
historicalPriceTruncatesEveryFamily =
    assertFamilyDiffs
        "price"
        ("P 2023-01-25 VUSA 80.00 INR\n\n" <>)
        [(family, DivergesFrom januaryEnd) | family <- everyFamily]

{- | The month chains are seeded from a constant, so a global change leaves every
family's month hashes byte-identical. Only 'diffChains' comparing the global
fingerprint catches it, and it must catch it for every family.
-}
globalChangeInvalidatesEveryFamily :: IO ()
globalChangeInvalidatesEveryFamily =
    withJournalPath "family-global" $ \path -> do
        original <- familyChainsFor path familyJournal
        restyled <- familyChainsFor path ("commodity 1000.000 INR\n\n" <> familyJournal)
        sequence_
            [ assertEqual
                (show family <> " must see a global change")
                GlobalChanged
                (diffFamilyChains original restyled family)
            | family <- everyFamily
            ]
        assertEqual
            "the family month hashes themselves must be unaffected"
            (familyMonths original)
            (familyMonths restyled)

familyMonths :: FamilyChains -> [[(Day, Hash)]]
familyMonths chains = [(chainFor family chains).months | family <- everyFamily]

{- | Alignment is what lets one divergence date mean the same thing in every
chain, and it is what makes a reused month's lookup in the previous chain
unconditional.
-}
familyChainsAreAlignedWithTheWhole :: IO ()
familyChainsAreAlignedWithTheWhole =
    withJournalPath "family-aligned" $ \path -> do
        chains <- familyChainsFor path familyJournal
        let wholeMonths = fst <$> chains.whole.months
        assertEqual
            "the whole chain should hold one entry per populated month"
            [januaryEnd, februaryEnd, marchEnd, aprilEnd]
            wholeMonths
        sequence_
            [ assertEqual
                (show family <> " must cover the same months as the whole chain")
                wholeMonths
                (fst <$> (chainFor family chains).months)
            | family <- everyFamily
            ]

{- | Applies an edit to 'familyJournal' and asserts one 'ChainDiff' per family.

The whole-journal chain is asserted too, as the control: an edit no family
noticed would satisfy every family assertion vacuously, so the whole chain must
have seen it diverge.
-}
assertFamilyDiffs :: String -> (Text -> Text) -> [(ReportFamily, ChainDiff)] -> IO ()
assertFamilyDiffs label edit expected =
    withJournalPath ("family-" <> label) $ \path -> do
        original <- familyChainsFor path familyJournal
        edited <- familyChainsFor path (edit familyJournal)
        assertBool
            "the edit must reach the whole-journal chain, or the case proves nothing"
            (diffChains original.whole edited.whole /= Identical)
        sequence_
            [ assertEqual
                (label <> ": " <> show family)
                wanted
                (diffFamilyChains original edited family)
            | (family, wanted) <- expected
            ]

-- | Replaces a fragment, failing loudly rather than silently editing nothing.
familyEdit :: Text -> Text -> Text -> Text
familyEdit before after journal
    | before `Text.isInfixOf` journal = Text.replace before after journal
    | otherwise = error ("family journal has no fragment " <> show before)

{- | One transaction per month, each reaching a different set of families: income
and bank in January, expense only in February, asset only in March, and the
portfolio in April.
-}
familyJournal :: Text
familyJournal =
    Text.unlines
        [ "2023-01-15 salary"
        , "    assets:bank:checking   1000.00 INR"
        , "    income:salary         -1000.00 INR"
        , ""
        , "2023-02-20 reclassify lunch"
        , "    expenses:dining          10.00 INR"
        , "    expenses:food           -10.00 INR"
        , ""
        , "2023-03-05 move savings"
        , "    assets:bank:savings     200.00 INR"
        , "    assets:bank:checking   -200.00 INR"
        , ""
        , "2023-04-10 buy fund"
        , "    assets:investments:vusa    1.00 VUSA @ 100.00 INR"
        , "    assets:bank:checking    -100.00 INR"
        ]

-- * The per-file pre-filter

{- | The pre-filter must be transparent and conservative: reusing a month's
digest has to produce byte-identical chains, and anything it cannot establish
cheaply and exactly has to fall back to hashing everything.

Transparency is checked by equality against a from-scratch build. That alone
would pass if the pre-filter never reused anything, so every case also runs
against a /poisoned/ previous chain — one whose month digests are all replaced by
a value no hashing could produce — and asserts exactly which months came back
poisoned. Reuse is then directly observed rather than inferred.
-}
filePrefilterSpec :: TestTree
filePrefilterSpec =
    testGroup
        "per-file pre-filter"
        [ testCase
            "an unchanged journal reuses every month"
            unchangedFilesReuseEveryMonth
        , testCase
            "a changed file recomputes only the months it sources"
            changedFileRecomputesItsOwnMonths
        , testCase
            "an in-place edit recomputes its month although the layout is unchanged"
            inPlaceEditRecomputesItsMonth
        , testCase
            "a deletion recomputes the month that lost the entry"
            deletionRecomputesItsMonth
        , testCase
            "an include repeated twice recomputes the doubled month"
            repeatedIncludeRecomputesItsMonth
        , testCase
            "every entry's source file is one of the journal's own files"
            entrySourceFilesAreJournalFiles
        , testCase
            "adding a file to the include set falls back to hashing everything"
            newFileFallsBackToHashingEverything
        , testCase
            "removing a file from the include set falls back to hashing everything"
            removedFileFallsBackToHashingEverything
        , testCase
            "reordering the include set falls back even though no data file changed"
            reorderedIncludesFallBack
        , testCase
            "a boot reload with no previous chain reuses nothing"
            bootReusesNothing
        ]

unchangedFilesReuseEveryMonth :: IO ()
unchangedFilesReuseEveryMonth =
    withSplitJournal "unchanged" $ \write -> do
        before <- write splitFiles
        after <- write splitFiles
        assertReuse "unchanged" before after [januaryEnd, marchEnd]

{- | The case the pre-filter exists for. @later.journal@ gains a transaction, so
its month is re-encoded and @earlier.journal@'s month is not — even though the
March prefix hash still moves, because prefix hashes are always refolded from
month /content/ digests, reused or not.
-}
changedFileRecomputesItsOwnMonths :: IO ()
changedFileRecomputesItsOwnMonths =
    withSplitJournal "changed" $ \write -> do
        before <- write splitFiles
        after <- write splitFiles{later = splitFiles.later <> extraMarchTransaction}
        assertReuse "changed" before after [januaryEnd]

{- | The commonest edit of all, and the one the layout hash cannot see: changing
an amount in place leaves the month's entry count, source files and order exactly
as they were. Only the file's own content hash differs, so this is the case that
the changed-file test is solely responsible for — a mutation removing it survives
every append and deletion case and is caught here.
-}
inPlaceEditRecomputesItsMonth :: IO ()
inPlaceEditRecomputesItsMonth =
    withSplitJournal "in-place" $ \write -> do
        before <- write splitFiles
        after <- write splitFiles{later = Text.replace "400.00" "450.00" splitFiles.later}
        assertReuse "in-place" before after [januaryEnd]

{- | Removal is the case a contributor-set test alone gets wrong: the file that
lost the entry can drop out of the month's contributors entirely, so it is the
month's layout hash that has to notice.
-}
deletionRecomputesItsMonth :: IO ()
deletionRecomputesItsMonth =
    withSplitJournal "deleted" $ \write -> do
        before <- write splitFiles{later = splitFiles.later <> extraMarchTransaction}
        after <- write splitFiles
        assertReuse "deleted" before after [januaryEnd]

newFileFallsBackToHashingEverything :: IO ()
newFileFallsBackToHashingEverything =
    withSplitJournal "added-file" $ \write -> do
        before <- write splitFiles
        after <- write splitFiles{extra = Just extraFileContents}
        assertReuse "added-file" before after []

removedFileFallsBackToHashingEverything :: IO ()
removedFileFallsBackToHashingEverything =
    withSplitJournal "removed-file" $ \write -> do
        before <- write splitFiles{extra = Just extraFileContents}
        after <- write splitFiles
        assertReuse "removed-file" before after []

{- | Nothing but the parent file changed, and the parent sources no entry of its
own, so a contributor test says every month is safe. It is not: swapping the
include order reorders the two files' entries inside the month they share, and
only the layout hash sees that.
-}
reorderedIncludesFallBack :: IO ()
reorderedIncludesFallBack =
    withSplitJournal "reordered" $ \write -> do
        before <- write sharedMonthFiles
        after <- write sharedMonthFiles{reversedIncludes = True}
        assertReuse "reordered" before after []

{- | The one case the layout hash's run lengths are for, and the only one that
separates them from the rest of the pre-filter.

Including the same file twice doubles its entries without changing the file set
or any file's contents, and the doubled file is not the one that changed — only
the parent is, and the parent sources no entry. So neither the file-set check nor
the changed-file check applies, and a layout hash that recorded which files
contributed but not how many entries each contributed would see nothing.
-}
repeatedIncludeRecomputesItsMonth :: IO ()
repeatedIncludeRecomputesItsMonth =
    withSplitJournal "repeated-include" $ \write -> do
        before <- write splitFiles
        after <- write splitFiles{repeatEarlier = True}
        assertBool
            "hledger must actually include the repeated file twice, or the case is vacuous"
            (length after.snapshot.journal.jtxns > length before.snapshot.journal.jtxns)
        assertReuse "repeated-include" before after [marchEnd]

{- | The assumption every month's file attribution rests on: an entry's source
position names one of the journal's own files.

'Hedger.Domain.Fingerprint' reads @tsourcepos@ and @pdsourcepos@ to decide which
file's contents a month depends on, and it falls back to hashing a month whose
entries name a file it cannot find. That fallback is unreachable as long as
hledger spells the two the same way, which is what this pins: if a future
hledger ever reported, say, an absolute path in one and a relative path in the
other, the pre-filter would quietly stop reusing anything and this case says so
directly rather than leaving it to be noticed as a performance regression.
-}
entrySourceFilesAreJournalFiles :: IO ()
entrySourceFilesAreJournalFiles =
    withSplitJournal "source-files" $ \write -> do
        build <- write splitFiles{extra = Just extraFileContents}
        let journal = build.snapshot.journal
            known = fst <$> journal.jfiles
            sources =
                [HledgerParse.sourceName (fst entry.tsourcepos) | entry <- journal.jtxns]
                    <> [HledgerParse.sourceName entry.pdsourcepos | entry <- journal.jpricedirectives]
        assertBool "the fixture must span several files" (length known >= 4)
        assertBool "the fixture must carry a price directive" (not (null journal.jpricedirectives))
        assertEqual
            "every entry must name one of the journal's own files"
            []
            (filter (`notElem` known) sources)

bootReusesNothing :: IO ()
bootReusesNothing =
    withSplitJournal "boot" $ \write -> do
        build <- write splitFiles
        assertEqual
            "with no previous chain nothing may be reused"
            []
            (reusedMonths (rebuiltFrom Nothing build))
        assertEqual
            "and the result must equal a from-scratch build"
            build.chains
            (rebuiltFrom Nothing build)

{- | Asserts both halves at once: that reuse is transparent, and that exactly the
expected months were reused.
-}
assertReuse :: String -> SplitBuild -> SplitBuild -> [Day] -> IO ()
assertReuse label before after expectedReused = do
    assertEqual
        (label <> ": reuse must reproduce the from-scratch chains")
        after.chains
        (rebuiltFrom (Just before.chains) after)
    assertEqual
        (label <> ": exactly these months may be reused")
        expectedReused
        (reusedMonths (rebuiltFrom (Just (poisonContents before.chains)) after))

{- | The months a rebuild took from its predecessor, identified by the poison
digest surviving into the result.
-}
reusedMonths :: FamilyChains -> [Day]
reusedMonths chains =
    [day | (day, content) <- chains.whole.monthContents, content == poisonDigest]

{- | Replaces every month's content digest with a value no encoding produces, in
the whole chain and in every family chain. A month that comes back poisoned was
reused; a month that comes back with an honest digest was recomputed.
-}
poisonContents :: FamilyChains -> FamilyChains
poisonContents chains =
    chains
        { whole = poisonChain chains.whole
        , netWorth = poisonChain chains.netWorth
        , cashflow = poisonChain chains.cashflow
        , investments = poisonChain chains.investments
        }
  where
    poisonChain chain =
        chain{monthContents = [(day, poisonDigest) | (day, _) <- chain.monthContents]}

poisonDigest :: Text
poisonDigest = "poison"

-- | A parsed multi-file journal together with the chains built from it.
data SplitBuild = SplitBuild
    { snapshot :: Snapshot
    , chains :: FamilyChains
    }

{- | A journal split across files by date, which is the shape the pre-filter can
profit from: a parent holding only @include@ lines, two data files, and
optionally a third.
-}
data SplitFiles = SplitFiles
    { earlier :: Text
    , later :: Text
    , extra :: Maybe Text
    , reversedIncludes :: Bool
    , repeatEarlier :: Bool
    }

splitFiles :: SplitFiles
splitFiles =
    SplitFiles
        { earlier = januaryTransaction
        , later = marchTransaction
        , extra = Nothing
        , reversedIncludes = False
        , repeatEarlier = False
        }

{- | Both data files contributing an entry on the same day, which is the only
arrangement in which the include order changes the entry stream at all; see
'secondJanuaryTransaction'.
-}
sharedMonthFiles :: SplitFiles
sharedMonthFiles = splitFiles{later = secondJanuaryTransaction}

withSplitJournal :: String -> ((SplitFiles -> IO SplitBuild) -> IO a) -> IO a
withSplitJournal label action = withJournalDir ("split-" <> label) (action . writeSplitJournal)

writeSplitJournal :: FilePath -> SplitFiles -> IO SplitBuild
writeSplitJournal dir files = do
    TextIO.writeFile (dir </> "earlier.journal") files.earlier
    TextIO.writeFile (dir </> "later.journal") files.later
    traverse_ (TextIO.writeFile (dir </> "extra.journal")) files.extra
    TextIO.writeFile (dir </> "main.journal") (includeLines files)
    loaded <- loadSnapshot (Text.pack (dir </> "main.journal"))
    case loaded of
        Left problem -> assertFailure ("journal load failed: " <> show problem)
        Right snapshot -> pure SplitBuild{snapshot = snapshot, chains = chainsOf Nothing snapshot}

includeLines :: SplitFiles -> Text
includeLines files =
    Text.unlines
        ["include " <> name | name <- included <> maybe [] (const ["extra.journal"]) files.extra]
  where
    included = repeated <> dataFiles
    repeated = ["earlier.journal" | files.repeatEarlier]
    dataFiles
        | files.reversedIncludes = ["later.journal", "earlier.journal"]
        | otherwise = ["earlier.journal", "later.journal"]

rebuiltFrom :: Maybe FamilyChains -> SplitBuild -> FamilyChains
rebuiltFrom previous build = chainsOf previous build.snapshot

familyChainsFor :: FilePath -> Text -> IO FamilyChains
familyChainsFor path contents = do
    TextIO.writeFile path contents
    loaded <- loadSnapshot (Text.pack path)
    case loaded of
        Left problem -> assertFailure ("journal load failed: " <> show problem)
        Right snapshot -> pure (chainsOf Nothing snapshot)

chainsOf :: Maybe FamilyChains -> Snapshot -> FamilyChains
chainsOf previous snapshot =
    familyChains
        relevance
        previous
        snapshot.journal
        (globalFingerprint snapshot.journal configuration)
        snapshot.journal.jtxns
        snapshot.journal.jpricedirectives
  where
    relevance =
        familyRelevance
            snapshot.journal
            configuration.assetClassMappings
            configuration.investmentMappings

januaryTransaction :: Text
januaryTransaction =
    Text.unlines
        [ "2023-01-15 salary"
        , "    assets:bank:checking   1000.00 INR"
        , "    income:salary         -1000.00 INR"
        ]

{- | Deliberately the /same date/ as 'januaryTransaction'. The entry stream is
sorted by date with a stable sort, so two entries only keep their journal order
relative to each other when their dates are equal — which makes a same-date pair
split across two files the one case where the include order is observable at all.
-}
secondJanuaryTransaction :: Text
secondJanuaryTransaction =
    Text.unlines
        [ "2023-01-15 groceries"
        , "    expenses:food            50.00 INR"
        , "    assets:bank:checking    -50.00 INR"
        ]

marchTransaction :: Text
marchTransaction =
    Text.unlines
        [ "2023-03-10 rent"
        , "    expenses:rent           400.00 INR"
        , "    assets:bank:checking   -400.00 INR"
        ]

extraMarchTransaction :: Text
extraMarchTransaction =
    Text.unlines
        [ ""
        , "2023-03-20 dinner"
        , "    expenses:food            20.00 INR"
        , "    assets:bank:checking    -20.00 INR"
        ]

{- | Carries a price directive as well as a transaction, so that
'entrySourceFilesAreJournalFiles' covers @pdsourcepos@ and not only
@tsourcepos@.
-}
extraFileContents :: Text
extraFileContents =
    Text.unlines
        [ "P 2023-02-14 VUSA 90.00 INR"
        , ""
        , "2023-02-14 flowers"
        , "    expenses:gifts           30.00 INR"
        , "    assets:bank:checking    -30.00 INR"
        ]

aprilEnd :: Day
aprilEnd = Calendar.fromGregorian 2023 4 30
