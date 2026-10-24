module View.Analytics.Category exposing
    ( categoryCard
    , categoryLabel
    , depthControl
    )

import Data.AccountType exposing (AccountType)
import Data.Analytics exposing (CategoryBreakdown)
import Data.Decimal as Decimal exposing (Decimal)
import Data.Formatting as Formatting
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Lib.Number as Number
import Ui.Chart.Donut as Donut
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon
import Ui.Input
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport
import View.AccountPresentation as AccountPresentation
import View.Analytics.Palette exposing (otherColor, paletteColor)
import View.Analytics.Types exposing (Palette)


categoryCard :
    Theme
    -> Viewport.ContentLayout
    -> Int
    ->
        { title : String
        , commodity : String
        , breakdown : CategoryBreakdown
        , palette : Palette
        , otherExpanded : Bool
        , onOtherToggle : msg
        , highlightedCategory : Maybe Int
        , onSliceClick : Int -> msg
        }
    -> Element msg
categoryCard theme layout cardHeight config =
    Surface.cardWith
        [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        , Element.alignTop
        , Element.height (Element.px cardHeight)
        ]
        theme
        [ Typography.cardTitle theme config.title
        , categoryContent theme layout cardHeight config
        ]


categoryContent :
    Theme
    -> Viewport.ContentLayout
    -> Int
    ->
        { config
            | title : String
            , commodity : String
            , breakdown : CategoryBreakdown
            , palette : Palette
            , otherExpanded : Bool
            , onOtherToggle : msg
            , highlightedCategory : Maybe Int
            , onSliceClick : Int -> msg
        }
    -> Element msg
categoryContent theme layout cardHeight config =
    let
        contentHeight =
            cardHeight
                - 2
                * Tokens.space20
                - 2
                * Tokens.borderHairline
                - Tokens.chartTitleHeight
                - Tokens.space12

        rowsHeight =
            if Viewport.isCompact layout then
                contentHeight - Tokens.analyticsDonutSize - Tokens.space16

            else
                contentHeight
    in
    if List.isEmpty config.breakdown.items then
        Element.el
            [ Element.width Element.fill
            , Element.height (Element.px contentHeight)
            , Element.padding Tokens.space12
            ]
            (Typography.mutedParagraph theme "No categories for this period")

    else
        let
            donut =
                categoryBreakdownDonut theme
                    config.palette
                    config.highlightedCategory
                    config.onSliceClick
                    config.breakdown.items

            rows =
                categoryRowsViewport theme rowsHeight config

            container =
                if Viewport.isCompact layout then
                    Element.column
                        [ Element.width Element.fill
                        , Element.height (Element.px contentHeight)
                        , Element.spacing Tokens.space16
                        , Element.alignTop
                        , Common.minHeightZero
                        ]

                else
                    Element.row
                        [ Element.width Element.fill
                        , Element.height (Element.px contentHeight)
                        , Element.spacing Tokens.space24
                        , Element.alignTop
                        , Common.minHeightZero
                        ]
        in
        container [ donut, rows ]


categoryRowsViewport :
    Theme
    -> Int
    ->
        { config
            | title : String
            , commodity : String
            , breakdown : CategoryBreakdown
            , palette : Palette
            , otherExpanded : Bool
            , onOtherToggle : msg
            , highlightedCategory : Maybe Int
        }
    -> Element msg
categoryRowsViewport theme height config =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px height)
        , Element.scrollbarY
        , Common.minWidthZero
        , Common.minHeightZero
        , Element.htmlAttribute (Attr.tabindex 0)
        , Element.htmlAttribute (Attr.attribute "role" "region")
        , Element.htmlAttribute
            (Attr.attribute "aria-label" (config.title ++ " category rows"))
        , Element.focused
            [ Border.shadow
                { color = theme.primary
                , offset = ( 0, 0 )
                , blur = 3
                , size = 0
                }
            ]
        ]
        (Element.column
            [ Element.width Element.fill
            , Element.spacing Tokens.space12
            , Element.alignTop
            ]
            (categoryRows theme config)
        )


depthControl : Theme -> Int -> (Int -> msg) -> Element msg
depthControl theme selected onDepth =
    Ui.Input.segmentedControl theme
        (List.map
            (\depth ->
                Ui.Input.segmentedOption theme
                    { label = String.fromInt depth
                    , selected = selected == depth
                    , onPress = onDepth depth
                    }
            )
            [ 1, 2, 3, 4 ]
        )


categoryBreakdownDonut :
    Theme
    -> Palette
    -> Maybe Int
    -> (Int -> msg)
    -> List { item | account : String, quantity : Decimal }
    -> Element msg
categoryBreakdownDonut theme palette highlightedCategory onSliceClick items =
    let
        sliceColor index item =
            if item.account == "Other" then
                otherColor theme

            else
                paletteColor theme palette index
    in
    Element.el [ Element.alignTop ]
        (Donut.view
            { size = Tokens.analyticsDonutSize
            , outerRadius = 108
            , innerRadius = 64
            , padAngle = 0.035
            , cornerRadius = 3
            , selectedOffset = 18
            , label = "Category breakdown"
            , items = items
            , value = .quantity >> Decimal.toFloat
            , color = sliceColor
            , itemLabel = .account >> categoryLabel >> (\label -> label ++ " slice")
            , selected = highlightedCategory
            , onSelect = onSliceClick
            , backgroundColor = theme.cardBg
            }
        )


categoryRow :
    Theme
    -> String
    -> Palette
    -> Decimal
    -> Int
    -> Bool
    -> { item | account : String, accountType : Maybe AccountType, quantity : Decimal }
    -> Element msg
categoryRow theme commodity palette total index isHighlighted item =
    let
        totalValue =
            Decimal.toFloat total

        share =
            if totalValue <= 0 then
                0

            else
                Decimal.toFloat item.quantity / totalValue * 100

        itemColor =
            if item.account == "Other" then
                otherColor theme

            else
                paletteColor theme palette index
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space8
        , Element.paddingXY Tokens.space4 Tokens.space2
        , Border.rounded Tokens.radiusHairline
        , Background.color theme.transparent
        , Border.color
            (if isHighlighted then
                itemColor

             else
                theme.transparent
            )
        , Border.width Tokens.borderHairline
        ]
        [ Icon.view
            { icon = AccountPresentation.iconForAccount item.accountType item.account
            , size = Tokens.iconSizeSmallGlyph
            , color = itemColor
            }
        , Element.el
            [ Element.width Element.fill
            , Common.minWidthZero
            ]
            (Element.text (categoryLabel item.account))
        , Element.el
            (Typography.numeric ++ [ Font.color theme.textMuted ])
            (Element.text
                (Formatting.formatDecimalDisplay commodity item.quantity
                    ++ " ("
                    ++ Number.roundedString share 0
                    ++ "%)"
                )
            )
        ]


categoryRows :
    Theme
    ->
        { config
            | commodity : String
            , breakdown : CategoryBreakdown
            , palette : Palette
            , otherExpanded : Bool
            , onOtherToggle : msg
            , highlightedCategory : Maybe Int
        }
    -> List (Element msg)
categoryRows theme config =
    let
        topLevel =
            config.breakdown.items
                |> List.indexedMap
                    (\index item ->
                        let
                            isHighlighted =
                                config.highlightedCategory
                                    == Just index
                        in
                        if
                            item.account
                                == "Other"
                                && not (List.isEmpty config.breakdown.otherItems)
                        then
                            Input.button
                                [ Element.width Element.fill
                                , Common.touchTargetHeight
                                , Element.htmlAttribute
                                    (Attr.attribute "aria-expanded"
                                        (if config.otherExpanded then
                                            "true"

                                         else
                                            "false"
                                        )
                                    )
                                , Element.htmlAttribute
                                    (Attr.title
                                        (if config.otherExpanded then
                                            "Collapse aggregated categories"

                                         else
                                            "Expand aggregated categories"
                                        )
                                    )
                                ]
                                { onPress = Just config.onOtherToggle
                                , label =
                                    Element.row
                                        [ Element.width Element.fill
                                        , Element.spacing Tokens.space8
                                        ]
                                        [ Element.el
                                            [ Element.width Element.fill ]
                                            (categoryRow theme
                                                config.commodity
                                                config.palette
                                                config.breakdown.total
                                                index
                                                isHighlighted
                                                item
                                            )
                                        , Icon.view
                                            { icon =
                                                if config.otherExpanded then
                                                    Icon.ChevronUp

                                                else
                                                    Icon.ChevronDown
                                            , size = Tokens.iconSizeSmallGlyph
                                            , color = theme.textMuted
                                            }
                                        ]
                                }

                        else
                            categoryRow theme
                                config.commodity
                                config.palette
                                config.breakdown.total
                                index
                                isHighlighted
                                item
                    )

        nested =
            if config.otherExpanded then
                config.breakdown.otherItems
                    |> List.indexedMap
                        (\index item ->
                            let
                                nestedIndex =
                                    List.length config.breakdown.items
                                        + index

                                isHighlighted =
                                    config.highlightedCategory
                                        == Just nestedIndex
                            in
                            Element.el
                                [ Element.width Element.fill
                                , Element.paddingEach
                                    { top = Tokens.space0
                                    , right = Tokens.space0
                                    , bottom = Tokens.space0
                                    , left = Tokens.space24
                                    }
                                ]
                                (categoryRow theme
                                    config.commodity
                                    config.palette
                                    config.breakdown.total
                                    nestedIndex
                                    isHighlighted
                                    item
                                )
                        )

            else
                []
    in
    topLevel ++ nested


categoryLabel : String -> String
categoryLabel account =
    if account == "Other" then
        "Others (agg.)"

    else
        case String.split ":" account of
            _ :: nested ->
                if List.isEmpty nested then
                    account

                else
                    String.join ":" nested

            [] ->
                account
