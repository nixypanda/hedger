{- | Which analytics families a transaction can change.

'Hedger.Domain.Fingerprint.familyChains' needs one predicate per family so that
an edit only invalidates the checkpoints it can actually reach. This module
supplies it, and it does so by naming the /same/ conditions the three
calculations already select on, rather than by inventing a second notion of
relevance:

* Net worth reads exactly @Hedger.Domain.Internal.NetWorth.assetLiabilityAccountTypes@.
  Every one of its reports — the consolidated balance sheet, the asset, liability
  and gain reports, and the current-balance oracles — is a query for those
  account types, so a transaction with no posting to an asset, cash or liability
  account contributes nothing to any of them.
* Cashflow reads exactly @Hedger.Domain.Internal.Cashflow.cashflowAccountTypes@,
  for the same reason.
* The investment scope scan returns early on exactly this condition:
  'Hedger.Domain.Internal.InvestmentPerformance.stepAccumulator' ignores a
  transaction that neither touches the scope's portfolio nor matches one of its
  selected performance mappings. The predicate here is that condition taken at
  its widest — the root scope and every configured mapping — so that it holds for
  every scope a request can ask for, each of whose relevant sets is a subset.

Two properties are load-bearing and both point the same way.

Membership is decided with hledger's own 'Hledger.isAccountSubtypeOf' rather than
list membership, because that is what @Hledger.Query.matchesPostingExtra@ uses to
answer a @type:@ query. A cash account satisfies a query for assets and a gain
account one for revenues, and a predicate here that disagreed with the report's
own selection would be a wrong answer in the unsafe direction.

For the two lists as they stand today the distinction happens not to bite:
@[Asset, Cash, Liability]@ and @[Revenue, Gain, Expense]@ each name their own
subtypes explicitly, so plain membership would agree on every input and no test
can separate the two. That is exactly why the subtyping predicate is the one to
use — the equivalence is a property of the current lists, not of the code, and
dropping @Cash@ or @Gain@ from a list would silently start excluding accounts the
report still includes.

Account-type classification is a function of the account's name, its parents'
names and the journal's @type:@ declarations. The names live inside the month
chain's transaction hashes and the declarations in @GlobalFingerprint@'s
@accountTypes@ component, so a change to either is already caught: nothing can
silently reclassify an account without invalidating.
-}
module Hedger.Domain.Internal.FamilyRelevance (
    familyRelevance,
)
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Journal qualified as HledgerJournal
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Fingerprint (ReportFamily (..), everyFamily)
import Hedger.Domain.Internal.Cashflow (cashflowAccountTypes)
import Hedger.Domain.Internal.InvestmentPerformance (isInvestmentAccount)
import Hedger.Domain.Internal.NetWorth (assetLiabilityAccountTypes)
import Hedger.Domain.Investment (
    InvestmentMapping (..),
    InvestmentMappings,
    PerformanceAccount (..),
 )
import Hedger.Domain.Investment qualified as Investment

{- | The relevance predicate for one journal and configuration.

A transaction is relevant to a family when any of its postings is, so the whole
predicate reduces to a function of an account name — which is why it is
memoized over the journal's used accounts. Without that, an account's type is
re-inferred once per posting, and inference splits the name into its parent
components every time; a 50,000-transaction journal has some 150,000 postings and
a few hundred distinct accounts. An account the map does not hold is classified
directly, so the memo is an optimization and never a source of a different
answer.
-}
familyRelevance ::
    Hledger.Journal ->
    Investment.AssetClassMappings ->
    InvestmentMappings ->
    (Hledger.Transaction -> [ReportFamily])
familyRelevance journal assetClasses mappings = transactionFamilies
  where
    accountType = HledgerAccountName.accountNameType journal.jaccounttypes

    -- The root scope: 'isInvestmentAccount' with no scope is the widest set of
    -- investment accounts, and every narrower scope's set is contained in it.
    isPortfolioAccount = isInvestmentAccount assetClasses Nothing

    -- @mappingsForScope ""@ is every configured mapping, which likewise contains
    -- the @selectedMappings@ of every scope plan.
    performanceAccounts =
        [ performance.account
        | mapping <- Investment.mappingsForScope "" mappings
        , performance <- mapping.performanceAccounts
        ]

    memoized =
        Map.fromList
            [(account, classifyAccount account) | account <- HledgerJournal.journalAccountNamesUsed journal]

    accountFamilies account = Map.findWithDefault (classifyAccount account) account memoized

    transactionFamilies transaction =
        [ family
        | family <- everyFamily
        , any (elem family . accountFamilies . (.paccount)) transaction.tpostings
        ]

    classifyAccount :: Text -> [ReportFamily]
    classifyAccount account =
        concat
            [ [NetWorthFamily | isOneOf assetLiabilityAccountTypes]
            , [CashflowFamily | isOneOf cashflowAccountTypes]
            , [InvestmentFamily | isInvestmentRelevant account]
            ]
      where
        isOneOf wanted =
            maybe False (\actual -> any (actual `Hledger.isAccountSubtypeOf`) wanted) (accountType account)

    isInvestmentRelevant account =
        isPortfolioAccount account
            || any (`Investment.accountWithin` account) performanceAccounts
