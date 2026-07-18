module Hedger.Interface.HTTP.RegisterMapper (toRegisterEntries) where

import Data.Text (Text)
import Data.Text qualified as Text
import Hedger.Application.AccountRegisterQuery (RegisterEntry (..))
import Hedger.Interface.HTTP.Types (CommodityQuantity (..), RegisterEntryDto (..), RegisterValueDto (..))
import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger

toRegisterEntries :: Text -> [RegisterEntry] -> [RegisterEntryDto]
toRegisterEntries displayCommodity = fmap (toRegisterEntry displayCommodity)

toRegisterEntry :: Text -> RegisterEntry -> RegisterEntryDto
toRegisterEntry displayCommodity RegisterEntry{rawItem = raw, displayItem = valued, matchedAccounts} =
    RegisterEntryDto
        { date = valuedTransaction.tdate
        , description = valuedTransaction.tdescription
        , account = Text.intercalate ", " matchedAccounts
        , amount = valueDto displayCommodity rawAmount valuedAmount
        , balance = valueDto displayCommodity rawBalance valuedBalance
        }
  where
    (_, valuedTransaction, _, _, rawAmount, rawBalance) = raw
    (_, _, _, _, valuedAmount, valuedBalance) = valued

valueDto :: Text -> Hledger.MixedAmount -> Hledger.MixedAmount -> RegisterValueDto
valueDto displayCommodity rawAmount valuedAmount =
    RegisterValueDto
        { raw = amounts rawAmount
        , display = amounts valuedAmount
        , completeness = completenessFor displayCommodity valuedAmount
        }

amounts :: Hledger.MixedAmount -> [CommodityQuantity]
amounts = fmap toCommodityQuantity . HledgerAmount.amountsRaw

toCommodityQuantity :: Hledger.Amount -> CommodityQuantity
toCommodityQuantity amount =
    CommodityQuantity
        { commodity = amount.acommodity
        , quantity = Text.pack (show amount.aquantity)
        }

completenessFor :: Text -> Hledger.MixedAmount -> Text
completenessFor displayCommodity valuedAmount
    | all ((== displayCommodity) . (.commodity)) projected = "complete"
    | any ((== displayCommodity) . (.commodity)) projected = "partial"
    | otherwise = "unavailable"
  where
    projected = amounts valuedAmount
