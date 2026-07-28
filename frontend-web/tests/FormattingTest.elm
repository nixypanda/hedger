module FormattingTest exposing (suite)

import Data.Decimal as Decimal
import Expect
import Test exposing (Test, describe, test)
import View.Formatting as Formatting


suite : Test
suite =
    describe "amount formatting"
        [ test "compact amounts always include the configured precision" <|
            \_ ->
                [ Formatting.formatAmountDisplay "$" 12
                , Formatting.formatAmountDisplay "$" 12.5
                , Formatting.formatAmountDisplay "$" 1234
                , Formatting.formatAmountDisplay "₹" 100000
                ]
                    |> Expect.equal [ "$12.00", "$12.50", "$1.2K", "₹1.0L" ]
        , test "full amounts are rounded to exactly two decimal places" <|
            \_ ->
                [ fullAmount "12"
                , fullAmount "12.3"
                , fullAmount "12.345"
                , fullAmount "-9.999"
                , fullAmount "99999999999999999999.999"
                ]
                    |> Expect.equal
                        [ "12.00 USD"
                        , "12.30 USD"
                        , "12.35 USD"
                        , "-10.00 USD"
                        , "100000000000000000000.00 USD"
                        ]
        ]


fullAmount : String -> String
fullAmount raw =
    raw
        |> Decimal.fromString
        |> Result.map (Formatting.formatFullAmount "USD")
        |> Result.withDefault "invalid"
