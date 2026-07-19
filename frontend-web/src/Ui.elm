module Ui exposing
    ( FinancialTone(..)
    , StatusTone(..)
    , accentDot
    , badge
    , card
    , cardWith
    , chip
    , chips
    , coloredPill
    , completionDropdown
    , completionRow
    , completionRowWithLeading
    , dangerButton
    , dangerIconButton
    , dateInput
    , detailRow
    , disabledIconButton
    , disabledPrimaryButton
    , dotSeparated
    , drawerPanel
    , emptyState
    , errorBanner
    , fieldLabel
    , financialBadge
    , formSection
    , ghostButton
    , ghostButtonWithLabel
    , hairline
    , iconButton
    , input
    , inputWithSuffix
    , isSmall
    , layout
    , listCard
    , listRow
    , loading
    , maybeErrorBanner
    , modalOverlay
    , optionRow
    , optionRowWithLeading
    , pageContainer
    , pageHeader
    , pageHeaderBand
    , periodNavigation
    , primaryButton
    , removableChip
    , removableChipWithLeading
    , rowSurfaceAttrs
    , scrimOverlay
    , searchInput
    , searchInputInline
    , secondaryButton
    , section
    , segmentedControl
    , segmentedOption
    , selectDropdown
    , skeletonBar
    , skeletonLoading
    , statusBadge
    , suggestionList
    , truncatedText
    , wideSegmentedControl
    , wrappingStatusBadge
    )

import Element exposing (Attribute, Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes
import Lib.Text
import Ui.DesignTokens
    exposing
        ( radiusCard
        , radiusControl
        , radiusHairline
        , radiusPill
        , radiusSmall
        , space12
        , space16
        , space2
        , space20
        , space4
        , space8
        )
import Ui.Icon as Icon exposing (Icon)
import Ui.Responsive as Responsive
import Ui.Theme exposing (Theme)
import Ui.Typography
    exposing
        ( body
        , caption
        , minWidthZero
        , mutedParagraph
        , sectionTitle
        , subsectionTitle
        , uiClass
        )


layout : Theme -> Element msg -> Element msg
layout theme content =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Font.family [ Font.typeface "Inter", Font.sansSerif ]
        , Font.color theme.textPrimary
        , body
        , uiClass "ui-theme-root"
        ]
        [ content ]


pageContainer : Device -> List (Element msg) -> Element msg
pageContainer device children =
    let
        padding =
            Responsive.contentPadding device
    in
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 1280)
        , Element.spacing space16
        , Element.padding padding
        ]
        children


pageHeaderBand : Theme -> Device -> Element msg -> Element msg
pageHeaderBand theme device content =
    let
        padding =
            Responsive.contentPadding device
    in
    Element.el
        [ Element.width Element.fill
        , Background.color theme.pageBg
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        , Border.color theme.pageBg
        ]
        (Element.el
            [ Element.centerX
            , Element.width (Element.fill |> Element.maximum 1280)
            , Element.padding padding
            ]
            content
        )


isSmall : Device -> Bool
isSmall device =
    device.class == Element.Phone || device.class == Element.Tablet


truncatedText : Device -> String -> Element msg
truncatedText _ value =
    Element.el
        [ Element.width Element.fill
        , Element.clip
        , minWidthZero
        , uiClass "ui-text-ellipsis"
        ]
        (Element.text value)


card : Theme -> List (Element msg) -> Element msg
card theme children =
    cardWith [] theme children


cardWith : List (Attribute msg) -> Theme -> List (Element msg) -> Element msg
cardWith attributes theme children =
    Element.column
        (attributes
            ++ [ Element.width Element.fill
               , Background.color theme.cardBg
               , Border.rounded radiusCard
               , Element.padding space20
               , Element.spacing space12
               , Border.width 1
               , Border.color theme.divider
               , uiClass "ui-card-shadow ui-surface"
               ]
        )
        children


section :
    Theme
    ->
        { header : Maybe (Element msg)
        , body : List (Element msg)
        , footer : Maybe (Element msg)
        }
    -> Element msg
section theme config =
    card theme
        (optionalWithDivider theme config.header
            ++ config.body
            ++ optionalFooter theme config.footer
        )


formSection : Theme -> String -> List (Element msg) -> Element msg
formSection theme title children =
    section theme
        { header = Just (subsectionTitle theme title)
        , body = children
        , footer = Nothing
        }


listCard : Theme -> { rows : List (Element msg), footer : Maybe (Element msg) } -> Element msg
listCard theme config =
    section theme
        { header = Nothing
        , body =
            case config.rows of
                [] ->
                    [ Element.el [ Element.width Element.fill, Element.padding space16 ]
                        (mutedParagraph theme "No items yet.")
                    ]

                rows ->
                    List.intersperse (hairline theme) rows
        , footer = config.footer
        }


listRow :
    { onSelect : msg
    , leading : List (Element msg)
    , title : Element msg
    , subtitle : List (Element msg)
    , trailing : List (Element msg)
    }
    -> Element msg
listRow config =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.padding space16
        , Element.spacing space12
        , minWidthZero
        ]
        [ Input.button
            [ Element.width Element.fill
            , minWidthZero
            , uiClass "ui-motion"
            ]
            { onPress = Just config.onSelect
            , label =
                Element.row
                    [ Element.width Element.fill
                    , Element.spacing space12
                    , minWidthZero
                    ]
                    [ leadingSlot config.leading
                    , Element.column
                        [ Element.width Element.fill
                        , Element.spacing space4
                        , Element.alignTop
                        , minWidthZero
                        ]
                        (config.title :: config.subtitle)
                    ]
            }
        , trailingSlot config.trailing
        ]


hairline : Theme -> Element msg
hairline theme =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 1)
        , Background.color theme.divider
        ]
        Element.none


pageHeader :
    Theme
    ->
        { leading : Maybe (Element msg)
        , title : Element msg
        , subtitle : Maybe String
        , actions : List (Element msg)
        }
    -> Element msg
pageHeader theme config =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spacing space12
        , minWidthZero
        ]
        [ Maybe.withDefault Element.none config.leading
        , Element.column
            [ Element.width Element.fill
            , Element.spacing space8
            , minWidthZero
            ]
            (config.title
                :: (case config.subtitle of
                        Just subtitle ->
                            [ mutedParagraph theme subtitle ]

                        Nothing ->
                            []
                   )
            )
        , if List.isEmpty config.actions then
            Element.none

          else
            Element.row [ Element.spacing space8 ] config.actions
        ]


periodNavigation :
    Theme
    ->
        { label : Element msg
        , onPrevious : msg
        , onLatest : Maybe msg
        , onNext : msg
        }
    -> Element msg
periodNavigation theme config =
    Element.row [ Element.spacing space4, Element.alignRight ]
        [ compactPeriodButton theme "Previous period" "‹" config.onPrevious
        , config.label
        , compactPeriodButton theme "Next period" "›" config.onNext
        , config.onLatest
            |> Maybe.map
                (\onLatest ->
                    ghostButton theme { label = "Latest", onPress = onLatest }
                )
            |> Maybe.withDefault Element.none
        ]


compactPeriodButton : Theme -> String -> String -> msg -> Element msg
compactPeriodButton theme title label onPress =
    Input.button
        [ Element.width (Element.px 30)
        , Element.height (Element.px 30)
        , Background.color theme.cardBg
        , Border.color theme.divider
        , Border.width 1
        , Border.rounded radiusControl
        , Font.color theme.textPrimary
        , Font.center
        , Element.centerY
        , Html.Attributes.title title |> Element.htmlAttribute
        ]
        { onPress = Just onPress
        , label = Element.el [ Element.centerX, Element.centerY, Font.size 19 ] (Element.text label)
        }


loading : Theme -> String -> Element msg
loading theme message =
    Element.row
        [ Element.spacing space8
        , Font.color theme.textMuted
        ]
        [ Icon.view { icon = Icon.LoaderCircle, size = 18, color = theme.textMuted }
        , Element.text message
        ]


emptyState : Theme -> String -> Element msg
emptyState theme message =
    card theme
        [ Element.paragraph
            [ Font.color theme.textMuted
            , Font.center
            , Element.padding space8
            ]
            [ Element.text message ]
        ]


maybeErrorBanner : Theme -> Maybe String -> Element msg
maybeErrorBanner theme maybeError =
    Maybe.withDefault Element.none (Maybe.map (errorBanner theme) maybeError)


errorBanner : Theme -> String -> Element msg
errorBanner theme message =
    Element.row
        [ Element.width Element.fill
        , Element.spacing space8
        , Element.padding space12
        , Background.color theme.dangerBg
        , Font.color theme.dangerFg
        , Border.rounded radiusControl
        ]
        [ Icon.view { icon = Icon.AlertTriangle, size = 18, color = theme.dangerFg }
        , Element.paragraph [ Element.width Element.fill ] [ Element.text message ]
        ]


skeletonLoading : Theme -> Element msg
skeletonLoading theme =
    Element.column [ Element.width Element.fill, Element.spacing space12 ]
        [ skeletonBar theme
        , skeletonBar theme
        , skeletonBar theme
        ]


skeletonBar : Theme -> Element msg
skeletonBar theme =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 16)
        , Background.color theme.inputFill
        , Border.rounded radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none


primaryButton : Theme -> { label : String, onPress : msg } -> Element msg
primaryButton theme config =
    buttonBase theme
        { label = Element.text config.label
        , onPress = Just config.onPress
        , background = theme.primary
        , foreground = Element.rgb255 255 255 255
        , border = theme.primary
        , hoverBackground = theme.primaryPressed
        }


disabledPrimaryButton : Theme -> { label : String } -> Element msg
disabledPrimaryButton theme config =
    buttonBase theme
        { label = Element.text config.label
        , onPress = Nothing
        , background = theme.primaryDisabled
        , foreground = Element.rgb255 255 255 255
        , border = theme.primaryDisabled
        , hoverBackground = theme.primaryDisabled
        }


secondaryButton : Theme -> { label : String, onPress : msg } -> Element msg
secondaryButton theme config =
    buttonBase theme
        { label = Element.text config.label
        , onPress = Just config.onPress
        , background = theme.primarySoft
        , foreground = theme.primary
        , border = theme.primarySoft
        , hoverBackground = theme.selectedBg
        }


dangerButton : Theme -> { label : String, onPress : msg } -> Element msg
dangerButton theme config =
    buttonBase theme
        { label = Element.text config.label
        , onPress = Just config.onPress
        , background = theme.dangerSolid
        , foreground = Element.rgb255 255 255 255
        , border = theme.dangerSolid
        , hoverBackground = theme.dangerFg
        }


ghostButton : Theme -> { label : String, onPress : msg } -> Element msg
ghostButton theme config =
    ghostButtonWithLabel theme
        { label = Element.text config.label
        , onPress = config.onPress
        }


ghostButtonWithLabel : Theme -> { label : Element msg, onPress : msg } -> Element msg
ghostButtonWithLabel theme config =
    buttonBase theme
        { label = config.label
        , onPress = Just config.onPress
        , background = theme.cardBg
        , foreground = theme.textPrimary
        , border = theme.divider
        , hoverBackground = theme.hoverBg
        }


iconButton : Device -> Theme -> { icon : Icon, title : String, onPress : msg } -> Element msg
iconButton device theme config =
    iconButtonBase device
        theme
        { icon = config.icon
        , title = config.title
        , onPress = Just config.onPress
        , background = theme.cardBg
        , foreground = theme.textPrimary
        , border = theme.divider
        , hoverBackground = theme.primarySoft
        , hoverBorder = theme.primary
        }


dangerIconButton : Device -> Theme -> { icon : Icon, title : String, onPress : msg } -> Element msg
dangerIconButton device theme config =
    iconButtonBase device
        theme
        { icon = config.icon
        , title = config.title
        , onPress = Just config.onPress
        , background = theme.cardBg
        , foreground = theme.dangerFg
        , border = theme.dangerFg
        , hoverBackground = theme.dangerBg
        , hoverBorder = theme.dangerSolid
        }


disabledIconButton : Theme -> { icon : Icon, title : String } -> Element msg
disabledIconButton theme config =
    iconButtonBase { class = Element.Phone, orientation = Element.Portrait }
        theme
        { icon = config.icon
        , title = config.title
        , onPress = Nothing
        , background = theme.inputFill
        , foreground = theme.textMuted
        , border = theme.divider
        , hoverBackground = theme.inputFill
        , hoverBorder = theme.divider
        }


fieldLabel : Theme -> String -> Element msg
fieldLabel theme label =
    Element.el
        [ caption
        , Font.medium
        , Font.color theme.textPrimary
        ]
        (Element.text label)


input :
    Theme
    ->
        { label : String
        , value : String
        , onChange : String -> msg
        }
    -> Element msg
input theme config =
    Input.text (inputAttrs theme)
        { onChange = config.onChange
        , text = config.value
        , placeholder = Nothing
        , label = Input.labelAbove [] (fieldLabel theme config.label)
        }


dateInput :
    Theme
    ->
        { label : String
        , value : String
        , onChange : String -> msg
        }
    -> Element msg
dateInput theme config =
    Input.text
        (Element.htmlAttribute (Html.Attributes.type_ "date") :: inputAttrs theme)
        { onChange = config.onChange
        , text = config.value
        , placeholder = Nothing
        , label = Input.labelAbove [] (fieldLabel theme config.label)
        }


searchInput :
    Theme
    ->
        { label : String
        , text : String
        , onChange : String -> msg
        , extraAttrs : List (Attribute msg)
        }
    -> Element msg
searchInput theme config =
    Input.text (inputAttrs theme ++ config.extraAttrs)
        { onChange = config.onChange
        , text = config.text
        , placeholder = Nothing
        , label = Input.labelAbove [] (fieldLabel theme config.label)
        }


searchInputInline :
    Theme
    ->
        { label : String
        , text : String
        , onChange : String -> msg
        , extraAttrs : List (Attribute msg)
        }
    -> Element msg
searchInputInline theme config =
    Input.text (inputAttrs theme ++ config.extraAttrs)
        { onChange = config.onChange
        , text = config.text
        , placeholder = Nothing
        , label = Input.labelHidden config.label
        }


inputWithSuffix :
    Theme
    ->
        { label : String
        , text : String
        , suffix : String
        , onChange : String -> msg
        }
    -> Element msg
inputWithSuffix theme config =
    Element.column
        [ Element.width Element.fill
        , Element.spacing space4
        ]
        [ fieldLabel theme config.label
        , Element.el
            [ Element.width Element.fill
            , Element.inFront
                (Element.el
                    [ Element.alignRight
                    , Element.centerY
                    , Element.paddingEach { top = 0, right = 12, bottom = 0, left = 0 }
                    , Font.color theme.textMuted
                    , body
                    ]
                    (Element.text config.suffix)
                )
            ]
            (Input.text
                (Element.paddingEach
                    { top = 10
                    , right = 56
                    , bottom = 10
                    , left = 10
                    }
                    :: inputBaseAttrs theme
                )
                { onChange = config.onChange
                , text = config.text
                , placeholder = Nothing
                , label = Input.labelHidden config.label
                }
            )
        ]


chip :
    Theme
    ->
        { label : String
        , selected : Bool
        , onClick : Maybe msg
        }
    -> Element msg
chip theme config =
    let
        attrs =
            chipAttrs theme config.selected
    in
    case config.onClick of
        Just msg ->
            Input.button attrs
                { onPress = Just msg
                , label = Element.text config.label
                }

        Nothing ->
            Element.el attrs (Element.text config.label)


chips : Theme -> List String -> Element msg
chips theme labels =
    if List.isEmpty labels then
        Element.none

    else
        Element.wrappedRow
            [ Element.spacing space8
            , Element.width Element.fill
            ]
            (List.map
                (\label ->
                    chip theme
                        { label = label
                        , selected = False
                        , onClick = Nothing
                        }
                )
                labels
            )


segmentedControl : Theme -> List (Element msg) -> Element msg
segmentedControl theme segments =
    Element.row
        [ Element.spacing space2
        , Element.padding 3
        , Border.rounded radiusSmall
        , Background.color theme.chipBg
        , Border.width 1
        , Border.color theme.divider
        ]
        segments


wideSegmentedControl :
    Theme
    -> List { label : String, selected : Bool, onPress : msg }
    -> Element msg
wideSegmentedControl theme options =
    Element.row
        [ Element.width Element.fill
        , Element.spacing space2
        , Element.padding 3
        , Border.rounded radiusSmall
        , Background.color theme.chipBg
        , Border.width 1
        , Border.color theme.divider
        ]
        (List.map
            (\config ->
                Input.button
                    [ Element.width (Element.fillPortion 1)
                    , Element.paddingEach { top = 8, right = 12, bottom = 8, left = 12 }
                    , Border.rounded radiusControl
                    , Background.color
                        (if config.selected then
                            theme.cardBg

                         else
                            theme.chipBg
                        )
                    , Font.color
                        (if config.selected then
                            theme.primary

                         else
                            theme.textMedium
                        )
                    , Font.medium
                    , Font.center
                    , caption
                    , uiClass "ui-motion"
                    , Element.htmlAttribute
                        (Html.Attributes.attribute
                            "aria-pressed"
                            (if config.selected then
                                "true"

                             else
                                "false"
                            )
                        )
                    ]
                    { onPress = Just config.onPress, label = Element.text config.label }
            )
            options
        )


segmentedOption :
    Theme
    -> { label : String, selected : Bool, onPress : msg }
    -> Element msg
segmentedOption theme config =
    Input.button
        [ Element.paddingEach { top = 6, right = 12, bottom = 6, left = 12 }
        , Border.rounded radiusControl
        , Background.color
            (if config.selected then
                theme.cardBg

             else
                theme.chipBg
            )
        , Font.color
            (if config.selected then
                theme.primary

             else
                theme.textMedium
            )
        , Font.medium
        , caption
        , uiClass "ui-motion"
        , Element.htmlAttribute
            (Html.Attributes.attribute
                "aria-pressed"
                (if config.selected then
                    "true"

                 else
                    "false"
                )
            )
        ]
        { onPress = Just config.onPress
        , label = Element.text config.label
        }


removableChip : Device -> Theme -> { label : String, onRemove : msg } -> Element msg
removableChip device theme config =
    removableChipWithLeading device
        theme
        { leading = Nothing
        , label = config.label
        , onRemove = config.onRemove
        }


removableChipWithLeading :
    Device
    -> Theme
    -> { leading : Maybe (Element msg), label : String, onRemove : msg }
    -> Element msg
removableChipWithLeading device theme config =
    let
        removeSize =
            if isSmall device then
                28

            else
                20
    in
    Element.row
        [ Element.width Element.shrink
        , Element.htmlAttribute (Html.Attributes.style "flex" "0 1 auto")
        , Element.htmlAttribute (Html.Attributes.style "max-width" "100%")
        , Background.color theme.chipBg
        , Font.color theme.chipFg
        , Border.rounded radiusPill
        , Border.width 1
        , Border.color theme.chipBorder
        , Element.paddingEach { top = 4, right = 4, bottom = 4, left = 10 }
        , Element.spacing space8
        , caption
        , Font.medium
        ]
        [ Element.row
            [ Element.width Element.shrink
            , Element.htmlAttribute (Html.Attributes.style "flex" "0 1 auto")
            , Element.htmlAttribute (Html.Attributes.style "max-width" "100%")
            , Element.spacing space4
            , minWidthZero
            ]
            (Maybe.map List.singleton config.leading
                |> Maybe.withDefault []
                |> (\leading ->
                        leading
                            ++ [ Element.el
                                    [ minWidthZero
                                    , Element.htmlAttribute (Html.Attributes.style "flex" "0 1 auto")
                                    , uiClass "ui-text-ellipsis"
                                    ]
                                    (Element.text config.label)
                               ]
                   )
            )
        , Input.button
            [ Element.width (Element.px removeSize)
            , Element.height (Element.px removeSize)
            , Border.rounded radiusPill
            , Background.color theme.cardBg
            , Border.width 1
            , Border.color theme.divider
            , Element.htmlAttribute (Html.Attributes.title "Remove")
            ]
            { onPress = Just config.onRemove
            , label =
                Icon.view
                    { icon = Icon.Close
                    , size = 14
                    , color = theme.dangerFg
                    }
            }
        ]


coloredPill :
    { label : String
    , bg : Element.Color
    , fg : Element.Color
    }
    -> Element msg
coloredPill config =
    Element.el
        [ Background.color config.bg
        , Font.color config.fg
        , Border.rounded radiusPill
        , Element.paddingEach { top = 4, right = 10, bottom = 4, left = 10 }
        , caption
        , Font.medium
        ]
        (Element.text config.label)


badge : Theme -> String -> Element msg
badge theme label =
    coloredPill
        { label = label
        , bg = theme.inputFill
        , fg = theme.textPrimary
        }


type StatusTone
    = StatusSuccess
    | StatusPrimary
    | StatusWarning
    | StatusDanger
    | StatusNeutral


type FinancialTone
    = FinancialIncome
    | FinancialExpense
    | FinancialNeutral
    | FinancialEquity
    | FinancialBorrowing
    | FinancialRepayment
    | FinancialAsset


financialBadge : Theme -> FinancialTone -> String -> Element msg
financialBadge theme tone label =
    let
        colors =
            case tone of
                FinancialIncome ->
                    { bg = theme.successBg, fg = theme.successFg }

                FinancialExpense ->
                    { bg = theme.warningBg, fg = theme.warningFg }

                FinancialNeutral ->
                    { bg = theme.chipBg, fg = theme.neutral }

                FinancialEquity ->
                    { bg = theme.primarySoft, fg = theme.equity }

                FinancialBorrowing ->
                    { bg = theme.borrowingBg, fg = theme.borrowingFg }

                FinancialRepayment ->
                    { bg = theme.repaymentBg, fg = theme.repaymentFg }

                FinancialAsset ->
                    { bg = theme.assetBg, fg = theme.assetFg }
    in
    Element.el
        [ Background.color colors.bg
        , Font.color colors.fg
        , Border.rounded radiusPill
        , Element.paddingEach { top = 2, right = 8, bottom = 2, left = 8 }
        , caption
        , Font.medium
        ]
        (Element.text label)


statusBadge : Theme -> StatusTone -> String -> Element msg
statusBadge theme tone status =
    let
        colors =
            statusToneColors theme tone
    in
    coloredPill { label = status, bg = colors.bg, fg = colors.fg }


wrappingStatusBadge : Theme -> StatusTone -> String -> Element msg
wrappingStatusBadge theme tone status =
    let
        colors =
            statusToneColors theme tone
    in
    Element.paragraph
        [ Element.width Element.fill
        , Background.color colors.bg
        , Font.color colors.fg
        , Border.rounded radiusSmall
        , Element.paddingEach { top = 4, right = 10, bottom = 4, left = 10 }
        , caption
        , Font.medium
        ]
        [ Element.text status ]


statusToneColors : Theme -> StatusTone -> { bg : Element.Color, fg : Element.Color }
statusToneColors theme tone =
    case tone of
        StatusSuccess ->
            { bg = theme.successBg, fg = theme.successFg }

        StatusPrimary ->
            { bg = theme.primarySoft, fg = theme.primary }

        StatusWarning ->
            { bg = theme.warningBg, fg = theme.warningFg }

        StatusDanger ->
            { bg = theme.dangerBg, fg = theme.dangerFg }

        StatusNeutral ->
            { bg = theme.chipBg, fg = theme.chipFg }


modalOverlay :
    Theme
    ->
        { title : String
        , body : List (Element msg)
        , actions : Element msg
        , onBackgroundPress : Maybe msg
        }
    -> Element msg
modalOverlay theme config =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.inFront
            (Element.el
                [ Element.centerX
                , Element.centerY
                , Element.width (Element.fill |> Element.maximum 480)
                , Element.padding space16
                ]
                (card theme (sectionTitle theme config.title :: config.body ++ [ config.actions ]))
            )
        ]
        (scrimOverlay config.onBackgroundPress)


scrimOverlay : Maybe msg -> Element msg
scrimOverlay onPress =
    Input.button
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color (Element.rgba255 0 0 0 0.32)
        ]
        { onPress = onPress
        , label = Element.none
        }


drawerPanel : Theme -> List (Element msg) -> Element msg
drawerPanel theme children =
    Element.column
        [ Element.width (Element.fill |> Element.maximum 320)
        , Element.height Element.fill
        , Element.paddingXY space16 10
        , Element.spacing space12
        , Background.color theme.cardBg
        , uiClass "ui-card-shadow ui-surface"
        , Region.navigation
        ]
        children


completionDropdown :
    Theme
    -> { height : Int, rows : List (Element msg) }
    -> Element msg
completionDropdown theme config =
    optionList theme
        { maxHeight = Just config.height
        , rows = config.rows
        }


completionRow :
    Device
    -> Theme
    ->
        { label : String
        , trailing : Maybe (Element msg)
        , onPress : msg
        }
    -> Element msg
completionRow device theme config =
    completionRowWithLeading device
        theme
        { label = config.label
        , leading = Nothing
        , trailing = config.trailing
        , onPress = config.onPress
        }


completionRowWithLeading :
    Device
    -> Theme
    ->
        { leading : Maybe (Element msg)
        , label : String
        , trailing : Maybe (Element msg)
        , onPress : msg
        }
    -> Element msg
completionRowWithLeading device theme config =
    optionRowWithLeading device
        theme
        { label = config.label
        , leading = config.leading
        , trailing = config.trailing
        , selected = False
        , onPress = config.onPress
        }


optionRow :
    Device
    -> Theme
    ->
        { label : String
        , trailing : Maybe (Element msg)
        , selected : Bool
        , onPress : msg
        }
    -> Element msg
optionRow device theme config =
    optionRowWithLeading device
        theme
        { leading = Nothing
        , label = config.label
        , trailing = config.trailing
        , selected = config.selected
        , onPress = config.onPress
        }


optionRowWithLeading :
    Device
    -> Theme
    ->
        { leading : Maybe (Element msg)
        , label : String
        , trailing : Maybe (Element msg)
        , selected : Bool
        , onPress : msg
        }
    -> Element msg
optionRowWithLeading device theme config =
    let
        foreground =
            if config.selected then
                theme.chipSelectedFg

            else
                theme.textPrimary

        background =
            if config.selected then
                theme.chipSelectedBg

            else
                theme.cardBg
    in
    Input.button
        [ Element.width Element.fill
        , Element.paddingXY space12 (smallAware device 12 10)
        , Border.rounded radiusControl
        , Background.color background
        , Font.color foreground
        , uiClass "ui-motion"
        , hoverEffect device [ Background.color theme.hoverBg ]
        ]
        { onPress = Just config.onPress
        , label =
            Element.row
                [ Element.width Element.fill
                , Element.spacing space8
                , minWidthZero
                ]
                (Maybe.map List.singleton config.leading
                    |> Maybe.withDefault []
                    |> (\leading ->
                            leading
                                ++ [ Element.el
                                        [ Element.width Element.fill
                                        , Font.medium
                                        , minWidthZero
                                        , uiClass "ui-text-ellipsis"
                                        ]
                                        (Element.text config.label)
                                   , Maybe.withDefault Element.none config.trailing
                                   ]
                       )
                )
        }


suggestionList : Theme -> List (Element msg) -> Element msg
suggestionList theme rows =
    optionList theme
        { maxHeight = Nothing
        , rows = rows
        }


selectDropdown :
    Device
    -> Theme
    ->
        { label : String
        , selected : a
        , options : List ( a, String )
        , onChange : a -> msg
        , isOpen : Bool
        , onToggle : msg
        }
    -> Element msg
selectDropdown device theme config =
    let
        selectedLabel =
            config.options
                |> List.filter (\( value, _ ) -> value == config.selected)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault ""

        dropdown =
            Element.el [ Element.moveDown 4 ]
                (optionList theme
                    { maxHeight = Nothing
                    , rows =
                        List.map
                            (\( value, label ) ->
                                optionRow device
                                    theme
                                    { label = label
                                    , trailing = Nothing
                                    , selected = value == config.selected
                                    , onPress = config.onChange value
                                    }
                            )
                            config.options
                    }
                )

        anchorAttrs =
            if config.isOpen then
                [ Element.width Element.fill, Element.below dropdown ]

            else
                [ Element.width Element.fill ]
    in
    Element.column [ Element.width Element.fill, Element.spacing space4 ]
        [ fieldLabel theme config.label
        , Element.el anchorAttrs
            (Input.button
                [ Element.width Element.fill
                , Border.rounded radiusControl
                , Background.color theme.inputFill
                , Border.width 1
                , Border.color theme.divider
                , Element.padding space12
                , uiClass "ui-motion"
                , hoverEffect device [ Border.color theme.primary ]
                ]
                { onPress = Just config.onToggle
                , label =
                    Element.row
                        [ Element.width Element.fill
                        , Element.spacing space8
                        , minWidthZero
                        ]
                        [ Element.el
                            [ Element.width Element.fill
                            , Font.color
                                (if selectedLabel == "" then
                                    theme.textMuted

                                 else
                                    theme.textPrimary
                                )
                            , minWidthZero
                            ]
                            (Element.text
                                (if selectedLabel == "" then
                                    "Select..."

                                 else
                                    selectedLabel
                                )
                            )
                        , Icon.view
                            { icon =
                                if config.isOpen then
                                    Icon.ChevronUp

                                else
                                    Icon.ChevronDown
                            , size = 18
                            , color = theme.textMuted
                            }
                        ]
                }
            )
        ]


detailRow : Theme -> String -> Element msg -> Element msg
detailRow theme label value =
    Element.row
        [ Element.spacing space4
        , Element.width Element.fill
        ]
        [ Element.el [ Font.color theme.textMedium, caption ] (Element.text (label ++ ":"))
        , Element.el [ Element.width Element.fill, minWidthZero ] value
        ]


rowSurfaceAttrs : Theme -> Bool -> List (Attribute msg)
rowSurfaceAttrs theme selected =
    [ Element.width Element.fill
    , Element.padding 10
    , Background.color
        (if selected then
            theme.selectedBg

         else
            theme.cardBg
        )
    , Border.rounded radiusCard
    , Border.width 1
    , Border.color
        (if selected then
            theme.primarySoft

         else
            theme.divider
        )
    , hoverEffect
        { class = Element.Desktop
        , orientation = Element.Landscape
        }
        [ Background.color theme.hoverBg ]
    , uiClass "ui-motion"
    ]


accentDot : Element.Color -> Element msg
accentDot color =
    Element.el
        [ Element.width (Element.px 4)
        , Element.height (Element.px 24)
        , Background.color color
        , Border.rounded radiusHairline
        ]
        Element.none


dotSeparated : List (Maybe String) -> String
dotSeparated =
    Lib.Text.dotSeparated


optionalWithDivider : Theme -> Maybe (Element msg) -> List (Element msg)
optionalWithDivider theme maybeElement =
    case maybeElement of
        Just element ->
            [ element, hairline theme ]

        Nothing ->
            []


optionalFooter : Theme -> Maybe (Element msg) -> List (Element msg)
optionalFooter theme maybeElement =
    case maybeElement of
        Just element ->
            [ hairline theme, element ]

        Nothing ->
            []


leadingSlot : List (Element msg) -> Element msg
leadingSlot leading =
    Element.el
        [ Element.width (Element.px 12)
        , Element.paddingEach { top = space2, right = 0, bottom = 0, left = 0 }
        , Element.alignTop
        ]
        (case leading of
            first :: _ ->
                first

            [] ->
                Element.none
        )


trailingSlot : List (Element msg) -> Element msg
trailingSlot trailing =
    if List.isEmpty trailing then
        Element.none

    else
        Element.row
            [ Element.spacing space12
            , Element.alignTop
            , Element.paddingEach { top = space8, right = 0, bottom = 0, left = 0 }
            ]
            trailing


buttonBase :
    Theme
    ->
        { label : Element msg
        , onPress : Maybe msg
        , background : Element.Color
        , foreground : Element.Color
        , border : Element.Color
        , hoverBackground : Element.Color
        }
    -> Element msg
buttonBase _ config =
    Input.button
        [ Element.paddingEach { top = 11, right = 16, bottom = 11, left = 16 }
        , Border.rounded radiusControl
        , Border.width 1
        , Border.color config.border
        , Background.color config.background
        , Font.color config.foreground
        , Font.medium
        , Font.center
        , uiClass "ui-motion"
        , hoverEffect
            { class = Element.Desktop
            , orientation = Element.Landscape
            }
            [ Background.color config.hoverBackground ]
        , Element.htmlAttribute (Html.Attributes.style "min-height" "40px")
        ]
        { onPress = config.onPress
        , label = Element.el [ Element.centerX ] config.label
        }


iconButtonBase :
    Device
    -> Theme
    ->
        { icon : Icon
        , title : String
        , onPress : Maybe msg
        , background : Element.Color
        , foreground : Element.Color
        , border : Element.Color
        , hoverBackground : Element.Color
        , hoverBorder : Element.Color
        }
    -> Element msg
iconButtonBase device _ config =
    Input.button
        [ Element.width (Element.px 40)
        , Element.height (Element.px 40)
        , Background.color config.background
        , Font.color config.foreground
        , Font.center
        , Border.rounded radiusPill
        , Border.width 1
        , Border.color config.border
        , uiClass "ui-motion"
        , hoverEffect device
            [ Background.color config.hoverBackground
            , Border.color config.hoverBorder
            ]
        , Element.htmlAttribute (Html.Attributes.title config.title)
        ]
        { onPress = config.onPress
        , label =
            Icon.view
                { icon = config.icon
                , size = 20
                , color = config.foreground
                }
        }


inputAttrs : Theme -> List (Attribute msg)
inputAttrs theme =
    Element.padding 10 :: inputBaseAttrs theme


inputBaseAttrs : Theme -> List (Attribute msg)
inputBaseAttrs theme =
    [ Element.width Element.fill
    , Border.rounded radiusControl
    , Border.width 1
    , Border.color theme.divider
    , Background.color theme.inputFill
    , Font.color theme.textPrimary
    , uiClass "ui-motion"
    ]


chipAttrs : Theme -> Bool -> List (Attribute msg)
chipAttrs theme selected =
    [ Background.color
        (if selected then
            theme.chipSelectedBg

         else
            theme.chipBg
        )
    , Font.color
        (if selected then
            theme.chipSelectedFg

         else
            theme.chipFg
        )
    , Border.rounded radiusPill
    , Border.width 1
    , Border.color
        (if selected then
            theme.chipSelectedBg

         else
            theme.chipBorder
        )
    , Element.paddingEach { top = 5, right = 10, bottom = 5, left = 10 }
    , caption
    , Font.medium
    , uiClass "ui-motion"
    ]


optionList :
    Theme
    -> { maxHeight : Maybe Int, rows : List (Element msg) }
    -> Element msg
optionList theme config =
    if List.isEmpty config.rows then
        Element.none

    else
        Element.column
            ([ Element.width Element.fill
             , Element.spacing space2
             , Element.padding space4
             , Border.rounded radiusControl
             , Background.color theme.cardBg
             , Border.width 1
             , Border.color theme.divider
             , uiClass "ui-card-shadow ui-surface"
             ]
                ++ (case config.maxHeight of
                        Just height ->
                            [ Element.height (Element.px height)
                            , Element.scrollbarY
                            ]

                        Nothing ->
                            []
                   )
            )
            config.rows


hoverEffect : Device -> List Element.Decoration -> Attribute msg
hoverEffect device attrs =
    if isSmall device then
        Element.mouseOver []

    else
        Element.mouseOver attrs


smallAware : Device -> Int -> Int -> Int
smallAware device phoneValue desktopValue =
    if isSmall device then
        phoneValue

    else
        desktopValue
