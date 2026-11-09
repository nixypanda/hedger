module LedgersTest exposing (suite)

import Api.Ledgers as Ledgers
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "ledger catalog"
        [ test "accepts an absent default ledger" <|
            \_ ->
                Decode.decodeString Ledgers.decoder
                    ("{\"defaultLedger\":null,\"ledgers\":["
                        ++ "{\"id\":\"personal\",\"label\":\"Personal\",\"status\":\"fresh\"}]}"
                    )
                    |> Result.map .defaultLedger
                    |> Expect.equal (Ok Nothing)
        , test "keeps a configured default ledger" <|
            \_ ->
                Decode.decodeString Ledgers.decoder
                    """{"defaultLedger":"personal","ledgers":[]}"""
                    |> Result.map .defaultLedger
                    |> Expect.equal (Ok (Just "personal"))
        ]
