module Main (main) where

import Control.Exception (evaluate)
import Hedger.Domain.Snapshot (Snapshot (transactions), loadSnapshot)
import Hledger.Data.Types qualified as Hledger
import Test.Tasty.Bench (bench, defaultMain, nfIO)

main :: IO ()
main =
    defaultMain
        [ bench "parse shared journal" $ nfIO loadAndForceFixture
        ]

loadAndForceFixture :: IO Int
loadAndForceFixture = do
    result <- loadSnapshot "test/fixtures/shared.journal"
    case result of
        Left loadError -> fail (show loadError)
        Right snapshot -> evaluate (sum (length . Hledger.tpostings <$> snapshot.transactions))
