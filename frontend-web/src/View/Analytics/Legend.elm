module View.Analytics.Legend exposing
    ( activityLegend
    , chartLegend
    )

import Data.Analytics exposing (BreakdownItem)
import Element exposing (Element)
import Element.Font as Font
import List
import Ui.Chip as Chip
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.Analytics.Category exposing (categoryLabel)
import View.Analytics.Palette
    exposing
        ( activityCategoryColor
        , directionSpecsFor
        , flowMovementColors
        , otherColor
        , scaleColor
        )
import View.Analytics.Types
    exposing
        ( ActivityTone(..)
        , CalendarDisplay(..)
        , DirectionSpec
        , LegendState(..)
        )


chartLegend : Theme -> List ( Element.Color, String ) -> Element msg
chartLegend theme items =
    Element.wrappedRow [ Element.spacing Tokens.space14 ]
        (List.map
            (\( color, label ) ->
                Element.row [ Element.spacing Tokens.space5 ]
                    [ Surface.accentDot color
                    , Element.el [ Font.color theme.textMuted ] (Element.text label)
                    ]
            )
            items
        )


type alias ActivityCategoryLegendConfig msg =
    { theme : Theme
    , tone : ActivityTone
    , categories : List BreakdownItem
    , maxVisible : Int
    , legendState : LegendState
    , onToggle : msg
    , highlightedCategory : Maybe Int
    , onCategoryHighlight : Int -> msg
    }


activityCategoryLegend : ActivityCategoryLegendConfig msg -> Element msg
activityCategoryLegend config =
    if List.isEmpty config.categories then
        Element.none

    else
        let
            theme =
                config.theme

            tone =
                config.tone

            isExpanded =
                config.legendState == LegendExpanded

            indexedCategories =
                List.indexedMap Tuple.pair config.categories

            hiddenCount =
                max 0 (List.length config.categories - config.maxVisible)

            chip ( index, category ) =
                let
                    dotColor =
                        if category.account == "Other" then
                            otherColor theme

                        else
                            activityCategoryColor theme tone index
                in
                Chip.legendButton theme
                    { accent = dotColor
                    , selected = config.highlightedCategory == Just index
                    , onPress = config.onCategoryHighlight index
                    , label =
                        Element.row [ Element.spacing Tokens.space5 ]
                            [ Surface.accentDot dotColor
                            , Typography.mutedCaption theme (categoryLabel category.account)
                            ]
                    }

            toggle text =
                Chip.legendToggleButton theme
                    { onPress = config.onToggle
                    , label = Typography.mutedCaption theme text
                    }
        in
        if isExpanded then
            Element.wrappedRow
                [ Element.width Element.fill, Element.spacing Tokens.space10 ]
                (List.map chip indexedCategories
                    ++ (if hiddenCount > 0 then
                            [ toggle "show less" ]

                        else
                            []
                       )
                )

        else
            Element.row
                [ Element.width Element.fill, Element.spacing Tokens.space10 ]
                [ Element.el
                    [ Element.width Element.fill
                    , Element.clip
                    , Common.minWidthZero
                    ]
                    (Element.row
                        [ Element.spacing Tokens.space10 ]
                        (List.map chip (List.take config.maxVisible indexedCategories))
                    )
                , if hiddenCount > 0 then
                    toggle ("+" ++ String.fromInt hiddenCount ++ " more")

                  else
                    Element.none
                ]


activityLegend :
    Theme
    -> CalendarDisplay
    -> Int
    -> ActivityTone
    -> List BreakdownItem
    -> LegendState
    -> msg
    -> Maybe Int
    -> (Int -> msg)
    -> Element msg
activityLegend theme display limit tone categories state onToggle highlighted onHighlight =
    case directionSpecsFor theme tone of
        Just specs ->
            activityDirectionLegend theme specs highlighted onHighlight

        Nothing ->
            if display == ByCategory && (tone == IncomeTone || tone == ExpenseTone) then
                activityCategoryLegend
                    { theme = theme
                    , tone = tone
                    , categories = categories
                    , maxVisible = limit
                    , legendState = state
                    , onToggle = onToggle
                    , highlightedCategory = highlighted
                    , onCategoryHighlight = onHighlight
                    }

            else
                activityMeaningLegend theme tone


activityMeaningLegend : Theme -> ActivityTone -> Element msg
activityMeaningLegend theme tone =
    let
        meanings =
            case tone of
                LiabilityTone ->
                    [ ( "Borrowed", theme.borrowingFg )
                    , ( "Repaid", theme.repaymentFg )
                    , ( "Forgiven", theme.warningFg )
                    , ( "Borrowing reversed", theme.textMuted )
                    ]

                _ ->
                    let
                        ( postedColor, reversedColor ) =
                            flowMovementColors theme tone
                    in
                    [ ( "Posted", postedColor ), ( "Reversed", reversedColor ) ]
    in
    Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space10 ]
        (List.map
            (\( label, color ) ->
                Element.row [ Element.spacing Tokens.space5 ]
                    [ Surface.accentDot color, Typography.mutedCaption theme label ]
            )
            meanings
        )


activityDirectionLegend : Theme -> List DirectionSpec -> Maybe Int -> (Int -> msg) -> Element msg
activityDirectionLegend theme specs highlighted onHighlight =
    Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space10 ]
        (List.indexedMap
            (\index spec ->
                Chip.legendButton theme
                    { accent = scaleColor spec.scale 0
                    , selected = highlighted == Just index
                    , onPress = onHighlight index
                    , label =
                        Element.row [ Element.spacing Tokens.space5 ]
                            [ Surface.accentDot (scaleColor spec.scale 0)
                            , Typography.mutedCaption theme spec.label
                            ]
                    }
            )
            specs
        )
