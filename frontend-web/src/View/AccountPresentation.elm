module View.AccountPresentation exposing (iconForAccountName)

import Ui.Icon as Icon exposing (Icon)


iconForAccountName : String -> Icon
iconForAccountName accountName =
    let
        segments =
            String.split ":" (String.toLower accountName)

        accountRoot =
            List.head segments |> Maybe.withDefault ""

        categoryIcon segment =
            categoryIconFor accountRoot segment
    in
    segments
        |> List.drop 1
        |> List.reverse
        |> List.filterMap categoryIcon
        |> List.head
        |> Maybe.withDefault (rootIcon accountRoot)


rootIcon : String -> Icon
rootIcon root =
    case root of
        "assets" ->
            Icon.Wallet

        "liabilities" ->
            Icon.Receipt

        "income" ->
            Icon.TrendingUp

        "expenses" ->
            Icon.ChartPie

        "equity" ->
            Icon.Wallet

        _ ->
            Icon.Info


categoryIconFor : String -> String -> Maybe Icon
categoryIconFor root segment =
    case root of
        "assets" ->
            lookupIcon
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
                segment

        "liabilities" ->
            lookupIcon [ ( "loan", Icon.Loan ), ( "creditcard", Icon.CreditCard ) ] segment

        "expenses" ->
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

        "income" ->
            lookupIcon
                [ ( "salary", Icon.Salary )
                , ( "dividend", Icon.Dividend )
                , ( "gift", Icon.Gift )
                , ( "interest", Icon.Interest )
                ]
                segment

        _ ->
            Nothing


lookupIcon : List ( String, Icon ) -> String -> Maybe Icon
lookupIcon choices segment =
    choices
        |> List.filter (Tuple.first >> (==) segment)
        |> List.head
        |> Maybe.map Tuple.second
