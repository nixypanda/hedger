module Ui exposing
    ( FinancialTone(..)
    , PeriodLabelKind(..)
    , StatusTone(..)
    , accentDot
    , backButton
    , badge
    , card
    , cardWith
    , chip
    , dangerButton
    , dateInput
    , emptyState
    , errorBanner
    , fieldLabel
    , financialBadge
    , formSection
    , ghostButton
    , hairline
    , hoverBackground
    , iconButton
    , layout
    , legendButton
    , legendToggleButton
    , loading
    , pageContainer
    , pageHeader
    , pageHeaderBand
    , periodNavigation
    , primaryButton
    , rowSurfaceAttrs
    , searchInputInline
    , secondaryButton
    , section
    , segmentedControl
    , segmentedOption
    , statusBadge
    , touchTargetHeight
    , truncatedText
    , wideSegmentedControl
    , wrappingStatusBadge
    )

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes
import Ui.Css as Css
import Ui.DesignTokens
    exposing
        ( compactControlVisualHeight
        , minimumTouchTarget
        , periodFiscalYearLabelWidth
        , periodMonthLabelWidth
        , radiusCard
        , radiusControl
        , radiusHairline
        , radiusPill
        , radiusSmall
        , space12
        , space16
        , space2
        , space20
        , space3
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


pageContainer : List (Element msg) -> Element msg
pageContainer children =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 1280)
        , Element.spacing space16
        , Element.padding Responsive.contentPadding
        ]
        children


pageHeaderBand : Theme -> Element msg -> Element msg
pageHeaderBand theme content =
    Element.el
        [ Element.width Element.fill
        , Background.color theme.pageBg
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        , Border.color theme.pageBg
        ]
        (Element.el
            [ Element.centerX
            , Element.width (Element.fill |> Element.maximum 1280)
            , Element.padding Responsive.contentPadding
            ]
            content
        )


truncatedText : String -> Element msg
truncatedText value =
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
        { label : String
        , labelKind : PeriodLabelKind
        , onPrevious : msg
        , onNext : msg
        }
    -> Element msg
periodNavigation theme config =
    Element.row [ Element.spacing space4, Element.alignRight ]
        [ iconButton theme
            { icon = Icon.ChevronLeft
            , title = "Previous period"
            , onPress = config.onPrevious
            }
        , Element.el
            [ Element.width (Element.px (periodLabelWidth config.labelKind))
            , Element.height (Element.px minimumTouchTarget)
            , Element.centerY
            ]
            (Element.el
                [ Element.centerX, Element.centerY, body, Font.medium ]
                (Element.text config.label)
            )
        , iconButton theme
            { icon = Icon.ChevronRight
            , title = "Next period"
            , onPress = config.onNext
            }
        ]


type PeriodLabelKind
    = MonthPeriodLabel
    | FiscalYearPeriodLabel


periodLabelWidth : PeriodLabelKind -> Int
periodLabelWidth labelKind =
    case labelKind of
        MonthPeriodLabel ->
            periodMonthLabelWidth

        FiscalYearPeriodLabel ->
            periodFiscalYearLabelWidth


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


iconButton : Theme -> { icon : Icon, title : String, onPress : msg } -> Element msg
iconButton theme config =
    iconButtonBase theme
        { icon = config.icon
        , title = config.title
        , onPress = Just config.onPress
        , background = theme.cardBg
        , foreground = theme.textPrimary
        , border = theme.divider
        , hoverBackground = theme.primarySoft
        , hoverBorder = theme.primary
        }


backButton : Theme -> { onPress : msg } -> Element msg
backButton theme config =
    iconButton theme
        { icon = Icon.ChevronLeft
        , title = "Back"
        , onPress = config.onPress
        }


fieldLabel : Theme -> String -> Element msg
fieldLabel theme label =
    Element.el
        [ caption
        , Font.medium
        , Font.color theme.textPrimary
        ]
        (Element.text label)


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
        surface =
            Element.el
                (Element.width (Element.shrink |> Element.minimum minimumTouchTarget)
                    :: chipAttrs theme config.selected
                )
                (Element.el [ Element.centerX ] (Element.text config.label))

        attrs =
            chipAttrs theme config.selected
    in
    case config.onClick of
        Just msg ->
            Input.button
                [ Element.height (Element.px minimumTouchTarget)
                , Element.width Element.shrink
                ]
                { onPress = Just msg
                , label = surface
                }

        Nothing ->
            Element.el
                (Element.width (Element.shrink |> Element.minimum minimumTouchTarget) :: attrs)
                (Element.el [ Element.centerX ] (Element.text config.label))


segmentedControl : Theme -> List (Element msg) -> Element msg
segmentedControl theme segments =
    Element.row
        [ Element.spacing space2
        , Element.padding space3
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
        , Element.padding space3
        , Border.rounded radiusSmall
        , Background.color theme.chipBg
        , Border.width 1
        , Border.color theme.divider
        ]
        (List.map
            (\config ->
                Input.button
                    [ Element.width (Element.fillPortion 1)
                    , Font.color
                        (if config.selected then
                            theme.primary

                         else
                            theme.textMedium
                        )
                    , Font.medium
                    , Font.center
                    , body
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
                    , label = segmentedSurface theme config.selected config.label
                    }
            )
            options
        )


segmentedOption :
    Theme
    -> { label : String, selected : Bool, onPress : msg }
    -> Element msg
segmentedOption theme config =
    Input.button
        [ Font.color
            (if config.selected then
                theme.primary

             else
                theme.textMedium
            )
        , Font.medium
        , Font.center
        , body
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
        , label = segmentedSurface theme config.selected config.label
        }


segmentedSurface : Theme -> Bool -> String -> Element msg
segmentedSurface theme selected label =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px compactControlVisualHeight)
        , Element.paddingXY space12 0
        , Border.rounded radiusControl
        , Background.color
            (if selected then
                theme.cardBg

             else
                theme.chipBg
            )
        ]
        (Element.el [ Element.centerX, Element.centerY ] (Element.text label))


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
    ]
        ++ hoverBackground theme.hoverBg
        ++ [ uiClass "ui-motion" ]


accentDot : Element.Color -> Element msg
accentDot color =
    Element.el
        [ Element.width (Element.px 4)
        , Element.height (Element.px 24)
        , Background.color color
        , Border.rounded radiusHairline
        ]
        Element.none


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
        ([ Element.paddingEach { top = 11, right = 16, bottom = 11, left = 16 }
         , Border.rounded radiusControl
         , Border.width 1
         , Border.color config.border
         , Background.color config.background
         , Font.color config.foreground
         , Font.medium
         , Font.center
         , uiClass "ui-motion"
         ]
            ++ hoverBackground config.hoverBackground
            ++ [ Element.height (Element.shrink |> Element.minimum minimumTouchTarget) ]
        )
        { onPress = config.onPress
        , label = Element.el [ Element.centerX ] config.label
        }


iconButtonBase :
    Theme
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
iconButtonBase _ config =
    Input.button
        ([ Element.width (Element.px minimumTouchTarget)
         , Element.height (Element.px minimumTouchTarget)
         , Background.color config.background
         , Font.color config.foreground
         , Font.center
         , Border.rounded radiusPill
         , Border.width 1
         , Border.color config.border
         , uiClass "ui-motion"
         ]
            ++ hoverBackground config.hoverBackground
            ++ hoverBorder config.hoverBorder
            ++ [ Element.htmlAttribute (Html.Attributes.title config.title)
               , Region.description config.title
               ]
        )
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


legendButton :
    Theme
    ->
        { label : Element msg
        , accent : Element.Color
        , selected : Bool
        , onPress : msg
        }
    -> Element msg
legendButton _ config =
    Input.button
        [ Element.height (Element.shrink |> Element.minimum minimumTouchTarget) ]
        { onPress = Just config.onPress
        , label = legendSurface config
        }


legendToggleButton : Theme -> { label : Element msg, onPress : msg } -> Element msg
legendToggleButton theme config =
    Input.button
        [ Element.height (Element.shrink |> Element.minimum minimumTouchTarget)
        , Font.color theme.textMuted
        ]
        { onPress = Just config.onPress
        , label = legendToggleSurface config.label
        }


legendSurface :
    { config | label : Element msg, accent : Element.Color, selected : Bool }
    -> Element msg
legendSurface config =
    Element.el
        [ Element.height (Element.px compactControlVisualHeight)
        , Element.paddingXY space8 0
        , Border.rounded radiusControl
        , Border.color
            (if config.selected then
                config.accent

             else
                Element.rgba 0 0 0 0
            )
        , Border.width 1
        ]
        (Element.el [ Element.centerY ] config.label)


legendToggleSurface : Element msg -> Element msg
legendToggleSurface label =
    Element.el
        [ Element.height (Element.px compactControlVisualHeight)
        , Element.paddingXY space8 0
        ]
        (Element.el [ Element.centerY ] label)


touchTargetHeight : Attribute msg
touchTargetHeight =
    Element.height (Element.shrink |> Element.minimum minimumTouchTarget)


hoverBackground : Element.Color -> List (Attribute msg)
hoverBackground color =
    [ Element.htmlAttribute (Html.Attributes.class "ui-hover-background")
    , Element.htmlAttribute
        (Html.Attributes.style "--ui-hover-background" (Css.toCssColor color))
    ]


hoverBorder : Element.Color -> List (Attribute msg)
hoverBorder color =
    [ Element.htmlAttribute (Html.Attributes.class "ui-hover-border")
    , Element.htmlAttribute
        (Html.Attributes.style "--ui-hover-border" (Css.toCssColor color))
    ]
