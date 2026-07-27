module View.AccountPresentation exposing (iconForAccount)

import Data.AccountType as AccountType exposing (AccountType)
import Ui.Icon as Icon exposing (Icon)


{-| Pick an icon for an account. The top-level icon is driven by hledger's
`AccountType` classification (which regex-matches common top-level accounts and
honours journal `type:` declarations), while the more specific category icons
still key off the account-name segments, since those distinctions (bank vs.
gold, car vs. food) are not expressible as account types.
-}
iconForAccount : Maybe AccountType -> String -> Icon
iconForAccount accountType accountName =
    let
        segments =
            String.split ":" (String.toLower accountName)

        categoryIcon segment =
            categoryIconFor accountType segment
    in
    segments
        |> List.drop 1
        |> List.reverse
        |> List.filterMap categoryIcon
        |> List.head
        |> Maybe.withDefault (rootIcon accountType)


rootIcon : Maybe AccountType -> Icon
rootIcon accountType =
    case accountType of
        Just AccountType.Asset ->
            Icon.Wallet

        Just AccountType.Cash ->
            Icon.Wallet

        Just AccountType.Liability ->
            Icon.Receipt

        Just AccountType.Revenue ->
            Icon.TrendingUp

        Just AccountType.Gain ->
            Icon.TrendingUp

        Just AccountType.Expense ->
            Icon.ChartPie

        Just AccountType.Equity ->
            Icon.Wallet

        Just AccountType.Conversion ->
            Icon.Info

        Nothing ->
            Icon.Info


categoryIconFor : Maybe AccountType -> String -> Maybe Icon
categoryIconFor accountType segment =
    case accountType of
        Just AccountType.Asset ->
            lookupIcon assetIcons segment

        Just AccountType.Cash ->
            lookupIcon assetIcons segment

        Just AccountType.Liability ->
            lookupIcon [ ( "loan", Icon.Loan ), ( "creditcard", Icon.CreditCard ) ] segment

        Just AccountType.Expense ->
            lookupIcon
                [ ( "car", Icon.Car )
                , ( "food", Icon.Food )
                , ( "fun", Icon.Fun )
                , ( "gifts", Icon.Gift )
                , ( "health", Icon.Health )
                , ( "house", Icon.House )
                , ( "bills", Icon.Bills )
                , ( "furniture", Icon.Furniture )
                , ( "groceries", Icon.Groceries )
                , ( "subscriptions", Icon.Subscriptions )
                , ( "tax", Icon.Tax )
                , ( "travel", Icon.Travel )
                ]
                segment

        Just AccountType.Revenue ->
            lookupIcon incomeIcons segment

        Just AccountType.Gain ->
            lookupIcon incomeIcons segment

        _ ->
            Nothing


assetIcons : List ( String, Icon )
assetIcons =
    [ ( "cash", Icon.Cash )
    , ( "bank", Icon.Bank )
    , ( "wallet", Icon.Wallet )
    , ( "debt", Icon.Debt )
    , ( "hybrid", Icon.Hybrid )
    , ( "liquid", Icon.Liquid )
    , ( "gold", Icon.Gold )
    , ( "equity", Icon.Equity )
    , ( "property", Icon.Property )
    , ( "loan", Icon.Loan )
    ]


incomeIcons : List ( String, Icon )
incomeIcons =
    [ ( "salary", Icon.Salary )
    , ( "dividend", Icon.Dividend )
    , ( "gift", Icon.Gift )
    , ( "interest", Icon.Interest )
    ]


lookupIcon : List ( String, Icon ) -> String -> Maybe Icon
lookupIcon choices segment =
    choices
        |> List.filter (Tuple.first >> (==) segment)
        |> List.head
        |> Maybe.map Tuple.second
