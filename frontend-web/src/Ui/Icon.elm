module Ui.Icon exposing (Icon(..), view)

import Element exposing (Element)
import Html.Attributes
import Svg exposing (Svg, circle, line, path, polyline, svg)
import Svg.Attributes as SvgA
import Ui.Css


type Icon
    = ChevronRight
    | ChevronUp
    | ChevronDown
    | LoaderCircle
    | Close
    | Refresh
    | Settings
    | Wallet
    | Cash
    | Bank
    | Debt
    | Hybrid
    | Liquid
    | Gold
    | Equity
    | Property
    | Loan
    | CreditCard
    | Car
    | Food
    | Fun
    | Gift
    | Health
    | House
    | Bills
    | Furniture
    | Groceries
    | Subscriptions
    | Tax
    | Travel
    | Salary
    | Dividend
    | Interest
    | Receipt
    | TrendingUp
    | ChartBar
    | ChartPie
    | Download
    | AlertTriangle
    | Info


view :
    { icon : Icon
    , size : Int
    , color : Element.Color
    }
    -> Element msg
view config =
    Element.el
        [ Element.width (Element.px config.size)
        , Element.height (Element.px config.size)
        , Element.centerX
        , Element.centerY
        ]
        (Element.html
            (svg
                [ SvgA.viewBox "0 0 24 24"
                , SvgA.width (String.fromInt config.size)
                , SvgA.height (String.fromInt config.size)
                , SvgA.fill "none"
                , SvgA.stroke (Ui.Css.toCssColor config.color)
                , SvgA.strokeWidth "2"
                , SvgA.strokeLinecap "round"
                , SvgA.strokeLinejoin "round"
                , Html.Attributes.style "display" "block"
                , Html.Attributes.attribute "aria-hidden" "true"
                , Html.Attributes.attribute "focusable" "false"
                ]
                (shapes config.icon)
            )
        )


shapes : Icon -> List (Svg msg)
shapes icon =
    case icon of
        ChevronRight ->
            [ polyline [ SvgA.points "9 6 15 12 9 18" ] [] ]

        ChevronUp ->
            [ polyline [ SvgA.points "6 15 12 9 18 15" ] [] ]

        ChevronDown ->
            [ polyline [ SvgA.points "6 9 12 15 18 9" ] [] ]

        LoaderCircle ->
            [ path [ SvgA.d "M21 12a9 9 0 1 1-6.2-8.6" ] [] ]

        Close ->
            [ line [ SvgA.x1 "6", SvgA.y1 "6", SvgA.x2 "18", SvgA.y2 "18" ] []
            , line [ SvgA.x1 "18", SvgA.y1 "6", SvgA.x2 "6", SvgA.y2 "18" ] []
            ]

        Refresh ->
            [ path [ SvgA.d "M21 12a9 9 0 0 1-15.2 6.5" ] []
            , polyline [ SvgA.points "3 18 6 18 6 15" ] []
            , path [ SvgA.d "M3 12a9 9 0 0 1 15.2-6.5" ] []
            , polyline [ SvgA.points "21 6 18 6 18 9" ] []
            ]

        Settings ->
            [ circle [ SvgA.cx "12", SvgA.cy "12", SvgA.r "3" ] []
            , path
                [ SvgA.d
                    (String.concat
                        [ "M19.4 15a1.7 1.7 0 0 0 .3 1.9l.1.1-2.8 2.8-.1-.1"
                        , "a1.7 1.7 0 0 0-1.9-.3 1.7 1.7 0 0 0-1 1.6V21h-4"
                        , "v-.1a1.7 1.7 0 0 0-1-1.6 1.7 1.7 0 0 0-1.9.3l-.1.1"
                        , "L4.2 17l.1-.1a1.7 1.7 0 0 0 .3-1.9 1.7 1.7 0 0 0-1.6-1"
                        , "H3v-4h.1a1.7 1.7 0 0 0 1.6-1 1.7 1.7 0 0 0-.3-1.9"
                        , "L4.2 7 7 4.2l.1.1a1.7 1.7 0 0 0 1.9.3 1.7 1.7 0 0 0 1-1.6"
                        , "V3h4v.1a1.7 1.7 0 0 0 1 1.6 1.7 1.7 0 0 0 1.9-.3l.1-.1"
                        , "L19.8 7l-.1.1a1.7 1.7 0 0 0-.3 1.9 1.7 1.7 0 0 0 1.6 1"
                        , "h.1v4H21a1.7 1.7 0 0 0-1.6 1Z"
                        ]
                    )
                ]
                []
            ]

        Wallet ->
            [ path [ SvgA.d "M4 7h14a2 2 0 0 1 2 2v9H4a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h13" ] []
            , path [ SvgA.d "M16 12h5v4h-5a2 2 0 0 1 0-4Z" ] []
            , line [ SvgA.x1 "18", SvgA.y1 "14", SvgA.x2 "18", SvgA.y2 "14" ] []
            ]

        Cash ->
            [ path [ SvgA.d "M3 6h18v12H3Z" ] []
            , circle [ SvgA.cx "12", SvgA.cy "12", SvgA.r "3" ] []
            , path [ SvgA.d "M7 9H6v1M17 15h1v-1" ] []
            ]

        Bank ->
            [ path [ SvgA.d "m3 9 9-5 9 5" ] []
            , path [ SvgA.d "M5 10v7M9 10v7M15 10v7M19 10v7M3 20h18M4 17h16" ] []
            ]

        Debt ->
            [ path [ SvgA.d "M5 3h14v18l-3-2-4 2-4-2-3 2Z" ] []
            , circle [ SvgA.cx "9", SvgA.cy "9", SvgA.r "1" ] []
            , circle [ SvgA.cx "15", SvgA.cy "15", SvgA.r "1" ] []
            , line [ SvgA.x1 "15", SvgA.y1 "9", SvgA.x2 "9", SvgA.y2 "15" ] []
            ]

        Hybrid ->
            [ circle [ SvgA.cx "9", SvgA.cy "12", SvgA.r "6" ] []
            , circle [ SvgA.cx "15", SvgA.cy "12", SvgA.r "6" ] []
            , path [ SvgA.d "M12 7a6 6 0 0 0 0 10M12 7a6 6 0 0 1 0 10" ] []
            ]

        Liquid ->
            [ path [ SvgA.d "M12 3S6 10 6 15a6 6 0 0 0 12 0c0-5-6-12-6-12Z" ] []
            , path [ SvgA.d "M9 16a3 3 0 0 0 3 3" ] []
            ]

        Gold ->
            [ path [ SvgA.d "m6 7-3 10h18L18 7Z" ] []
            , path [ SvgA.d "M8 7h8l-2-4h-4Z" ] []
            , line [ SvgA.x1 "7", SvgA.y1 "13", SvgA.x2 "17", SvgA.y2 "13" ] []
            ]

        Equity ->
            [ path [ SvgA.d "M4 20V10M10 20V5M16 20v-8M22 20V8" ] []
            , path [ SvgA.d "M2 16 8 10l5 3 8-9" ] []
            , polyline [ SvgA.points "16 4 21 4 21 9" ] []
            ]

        Property ->
            [ path [ SvgA.d "M4 21V5h11v16M15 9h5v12M2 21h20" ] []
            , path [ SvgA.d "M8 9h3M8 13h3M8 17h3M18 13h.01M18 17h.01" ] []
            ]

        Loan ->
            [ circle [ SvgA.cx "17", SvgA.cy "7", SvgA.r "3" ] []
            , path [ SvgA.d "M3 14h5l3 3h4a2 2 0 0 0 2-2h-5" ] []
            , path [ SvgA.d "m3 12-2 2 5 6 2-2M17 13h2a3 3 0 0 1 3 3l-7 5H7" ] []
            ]

        CreditCard ->
            [ path [ SvgA.d "M3 6h18v12H3Z" ] []
            , line [ SvgA.x1 "3", SvgA.y1 "10", SvgA.x2 "21", SvgA.y2 "10" ] []
            , line [ SvgA.x1 "7", SvgA.y1 "15", SvgA.x2 "10", SvgA.y2 "15" ] []
            ]

        Car ->
            [ path [ SvgA.d "m5 11 2-5h10l2 5" ] []
            , path [ SvgA.d "M4 11h16a2 2 0 0 1 2 2v5H2v-5a2 2 0 0 1 2-2Z" ] []
            , circle [ SvgA.cx "6", SvgA.cy "15", SvgA.r "1" ] []
            , circle [ SvgA.cx "18", SvgA.cy "15", SvgA.r "1" ] []
            , path [ SvgA.d "M4 18v2M20 18v2" ] []
            ]

        Food ->
            [ path [ SvgA.d "M6 3v7a3 3 0 0 0 3 3V3M6 7h3M7.5 13v8" ] []
            , path [ SvgA.d "M15 3v8h4V7a4 4 0 0 0-4-4ZM19 11v10" ] []
            ]

        Fun ->
            [ path
                [ SvgA.d
                    (String.concat
                        [ "M7 9h10a4 4 0 0 1 3.8 5.2l-1.2 3.5a2 2 0 0 1-3.2.9"
                        , "L14 17h-4l-2.4 1.6a2 2 0 0 1-3.2-.9l-1.2-3.5A4 4 0 0 1 7 9Z"
                        ]
                    )
                ]
                []
            , path [ SvgA.d "M8 12v4M6 14h4M16 13h.01M18 15h.01" ] []
            ]

        Gift ->
            [ path [ SvgA.d "M3 10h18v11H3ZM2 6h20v4H2ZM12 6v15" ] []
            , path [ SvgA.d "M12 6H8.5A2.5 2.5 0 1 1 11 3.5ZM12 6h3.5A2.5 2.5 0 1 0 13 3.5Z" ] []
            ]

        Health ->
            [ path
                [ SvgA.d
                    (String.concat
                        [ "M20.8 5.7a5.5 5.5 0 0 0-7.8 0L12 6.8l-1-1.1"
                        , "a5.5 5.5 0 0 0-7.8 7.8L12 22l8.8-8.5a5.5 5.5 0 0 0 0-7.8Z"
                        ]
                    )
                ]
                []
            , polyline [ SvgA.points "5 13 9 13 11 9 14 16 16 13 20 13" ] []
            ]

        House ->
            [ path [ SvgA.d "m3 11 9-8 9 8v10h-6v-6H9v6H3Z" ] []
            ]

        Bills ->
            [ path [ SvgA.d "M6 3h12v18l-3-2-3 2-3-2-3 2Z" ] []
            , path
                [ SvgA.d
                    (String.concat
                        [ "M14.5 8.5a2.5 2.5 0 1 0-2.5 2.5"
                        , " 2.5 2.5 0 1 1-2.5 2.5M12 6v2M12 16v2"
                        ]
                    )
                ]
                []
            ]

        Furniture ->
            [ path [ SvgA.d "M6 12V7a3 3 0 0 1 3-3h6a3 3 0 0 1 3 3v5" ] []
            , path
                [ SvgA.d
                    (String.concat
                        [ "M4 10a2 2 0 0 0-2 2v6h20v-6a2 2 0 0 0-4 0v2"
                        , "H6v-2a2 2 0 0 0-2-2ZM5 18v3M19 18v3"
                        ]
                    )
                ]
                []
            ]

        Groceries ->
            [ path [ SvgA.d "M4 10h16l-2 10H6ZM8 10l4-7 4 7M9 14v2M15 14v2" ] []
            ]

        Subscriptions ->
            [ path [ SvgA.d "M20 7h-5V2" ] []
            , path [ SvgA.d "M20 7a9 9 0 0 0-15.5-2M4 17h5v5" ] []
            , path [ SvgA.d "M4 17a9 9 0 0 0 15.5 2" ] []
            ]

        Tax ->
            [ path [ SvgA.d "M5 3h14v18H5Z" ] []
            , circle [ SvgA.cx "9", SvgA.cy "9", SvgA.r "1" ] []
            , circle [ SvgA.cx "15", SvgA.cy "15", SvgA.r "1" ] []
            , line [ SvgA.x1 "15", SvgA.y1 "9", SvgA.x2 "9", SvgA.y2 "15" ] []
            ]

        Travel ->
            [ path [ SvgA.d "m22 2-8 20-3-9-9-3Z" ] []
            , line [ SvgA.x1 "11", SvgA.y1 "13", SvgA.x2 "22", SvgA.y2 "2" ] []
            ]

        Salary ->
            [ path [ SvgA.d "M3 8h18v12H3Z" ] []
            , path [ SvgA.d "M8 8V5h8v3M3 12h18M10 12v2h4v-2" ] []
            ]

        Dividend ->
            [ circle [ SvgA.cx "8", SvgA.cy "16", SvgA.r "5" ] []
            , path [ SvgA.d "M8 13v6M6 15h3a1.5 1.5 0 0 1 0 3H6" ] []
            , path [ SvgA.d "M12 11c1-5 4-7 9-7 0 5-3 8-8 8M15 7c-2-2-4-3-7-3 0 3 1 5 4 6" ] []
            ]

        Interest ->
            [ circle [ SvgA.cx "12", SvgA.cy "12", SvgA.r "9" ] []
            , circle [ SvgA.cx "9", SvgA.cy "9", SvgA.r "1" ] []
            , circle [ SvgA.cx "15", SvgA.cy "15", SvgA.r "1" ] []
            , line [ SvgA.x1 "15", SvgA.y1 "9", SvgA.x2 "9", SvgA.y2 "15" ] []
            ]

        Receipt ->
            [ path [ SvgA.d "M6 3h12v18l-3-2-3 2-3-2-3 2Z" ] []
            , line [ SvgA.x1 "9", SvgA.y1 "8", SvgA.x2 "15", SvgA.y2 "8" ] []
            , line [ SvgA.x1 "9", SvgA.y1 "12", SvgA.x2 "15", SvgA.y2 "12" ] []
            , line [ SvgA.x1 "9", SvgA.y1 "16", SvgA.x2 "13", SvgA.y2 "16" ] []
            ]

        TrendingUp ->
            [ polyline [ SvgA.points "3 17 9 11 13 15 21 7" ] []
            , polyline [ SvgA.points "15 7 21 7 21 13" ] []
            ]

        ChartBar ->
            [ path [ SvgA.d "M4 20V10" ] []
            , path [ SvgA.d "M10 20V4" ] []
            , path [ SvgA.d "M16 20v-7" ] []
            , path [ SvgA.d "M22 20V7" ] []
            , path [ SvgA.d "M2 20h22" ] []
            ]

        ChartPie ->
            [ path [ SvgA.d "M12 3v9h9" ] []
            , path [ SvgA.d "M21 12a9 9 0 1 1-9-9" ] []
            ]

        Download ->
            [ path [ SvgA.d "M12 3v12" ] []
            , polyline [ SvgA.points "7 10 12 15 17 10" ] []
            , path [ SvgA.d "M5 21h14" ] []
            ]

        AlertTriangle ->
            [ path
                [ SvgA.d
                    (String.concat
                        [ "M10.3 4.7 2.8 18a2 2 0 0 0 1.7 3h15"
                        , "a2 2 0 0 0 1.7-3L13.7 4.7a2 2 0 0 0-3.4 0Z"
                        ]
                    )
                ]
                []
            , line [ SvgA.x1 "12", SvgA.y1 "9", SvgA.x2 "12", SvgA.y2 "13" ] []
            , line [ SvgA.x1 "12", SvgA.y1 "17", SvgA.x2 "12", SvgA.y2 "17" ] []
            ]

        Info ->
            [ circle [ SvgA.cx "12", SvgA.cy "12", SvgA.r "9" ] []
            , line [ SvgA.x1 "12", SvgA.y1 "11", SvgA.x2 "12", SvgA.y2 "16" ] []
            , line [ SvgA.x1 "12", SvgA.y1 "8", SvgA.x2 "12", SvgA.y2 "8" ] []
            ]
