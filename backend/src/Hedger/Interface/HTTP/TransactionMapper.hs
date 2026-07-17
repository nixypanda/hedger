module Hedger.Interface.HTTP.TransactionMapper (toTransactionDto, toTransactions) where

import Data.Text qualified as Text
import Hedger.Domain.Transaction qualified as Domain
import Hedger.Interface.HTTP.Types (
    AmountDto (..),
    CostDto (..),
    PostingDto (..),
    TagDto (..),
    TransactionDto (..),
    TransactionPresentationDto (..),
 )
import Hledger.Data.Amount qualified as Hledger
import Hledger.Data.Types qualified as Hledger

toTransactions :: [Hledger.Transaction] -> [TransactionDto]
toTransactions = fmap toTransactionDto . Domain.newestFirst

toTransactionDto :: Hledger.Transaction -> TransactionDto
toTransactionDto transaction =
    TransactionDto
        { primaryDate = transaction.tdate
        , secondaryDate = transaction.tdate2
        , status = statusText transaction.tstatus
        , code = nonEmpty transaction.tcode
        , description = transaction.tdescription
        , comment = nonEmpty transaction.tcomment
        , tags = projectTags transaction.ttags
        , postings = projectPosting <$> transaction.tpostings
        , presentation = presentationDto (Domain.presentationFor transaction)
        }

projectPosting :: Hledger.Posting -> PostingDto
projectPosting posting =
    PostingDto
        { account = posting.paccount
        , status = statusText posting.pstatus
        , comment = nonEmpty posting.pcomment
        , tags = projectTags posting.ptags
        , amounts = projectAmount <$> Hledger.amountsRaw posting.pamount
        }

projectAmount :: Hledger.Amount -> AmountDto
projectAmount amount =
    AmountDto
        { commodity = amount.acommodity
        , quantity = Text.pack (show amount.aquantity)
        , cost = projectCost <$> amount.acost
        }

projectCost :: Hledger.AmountCost -> CostDto
projectCost amountCost =
    case amountCost of
        Hledger.UnitCost amount -> costDto "unit" amount
        Hledger.TotalCost amount -> costDto "total" amount

costDto :: Text.Text -> Hledger.Amount -> CostDto
costDto costType amount =
    CostDto
        { costType = costType
        , commodity = amount.acommodity
        , quantity = Text.pack (show amount.aquantity)
        }

projectTags :: [Hledger.Tag] -> [TagDto]
projectTags = fmap (\(name, value) -> TagDto{name = name, value = value})

statusText :: Hledger.Status -> Text.Text
statusText status =
    case status of
        Hledger.Unmarked -> "unmarked"
        Hledger.Pending -> "pending"
        Hledger.Cleared -> "cleared"

nonEmpty :: Text.Text -> Maybe Text.Text
nonEmpty value
    | Text.null value = Nothing
    | otherwise = Just value

presentationDto :: Domain.TransactionPresentation -> TransactionPresentationDto
presentationDto presentation =
    TransactionPresentationDto
        { classification = classificationText presentation.classification
        }

classificationText :: Domain.TransactionClassification -> Text.Text
classificationText classification =
    case classification of
        Domain.TransactionIncome -> "income"
        Domain.TransactionExpense -> "expense"
        Domain.TransactionTransfer -> "transfer"
        Domain.OpeningBalance -> "opening_balance"
        Domain.Borrowing -> "borrowing"
        Domain.Repayment -> "repayment"
        Domain.AssetAcquisition -> "asset_acquisition"
        Domain.AssetDisposal -> "asset_disposal"
        Domain.AssetReallocation -> "asset_reallocation"
        Domain.Unclassified -> "unclassified"
