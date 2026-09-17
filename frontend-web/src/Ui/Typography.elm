module Ui.Typography exposing
    ( body
    , brandLabel
    , caption
    , delimitedPageTitle
    , delimitedSectionTitle
    , detailLabel
    , detailValue
    , financeNumber
    , htmlCaption
    , kpiValue
    , listItemTitle
    , minWidthZero
    , mutedBody
    , mutedCaption
    , mutedParagraph
    , naturalWrapText
    , navLabel
    , pageTitle
    , rowAmount
    , rowDate
    , rowMetadata
    , rowTitle
    , sectionTitle
    , subsectionTitle
    , tabLabel
    , truncatedListItemTitle
    , uiClass
    , wrappedListItemTitle
    )

import Element exposing (Element)
import Element.Font as Font
import Element.Region as Region
import Html exposing (Attribute)
import Html.Attributes
import Ui.Theme exposing (Theme)


body : Element.Attribute msg
body =
    Font.size 14


caption : Element.Attribute msg
caption =
    Font.size 13


rowDate : Element.Attribute msg
rowDate =
    Font.size 13


rowTitle : Element.Attribute msg
rowTitle =
    Font.size 14


rowAmount : Element.Attribute msg
rowAmount =
    Font.size 13


rowMetadata : Element.Attribute msg
rowMetadata =
    Font.size 13


detailLabel : Element.Attribute msg
detailLabel =
    Font.size 13


detailValue : Element.Attribute msg
detailValue =
    Font.size 14


kpiValue : Element.Attribute msg
kpiValue =
    Font.size 28


navLabel : Element.Attribute msg
navLabel =
    Font.size 16


tabLabel : Element.Attribute msg
tabLabel =
    Font.size 13


htmlCaption : Attribute msg
htmlCaption =
    Html.Attributes.style "font-size" "13px"


brandLabel : Element.Attribute msg
brandLabel =
    Font.size 20


financeNumber : Element.Attribute msg
financeNumber =
    Font.variant Font.tabularNumbers


pageTitleSize : Element.Attribute msg
pageTitleSize =
    Font.size 24


sectionTitleSize : Element.Attribute msg
sectionTitleSize =
    Font.size 18


subsectionTitleSize : Element.Attribute msg
subsectionTitleSize =
    Font.size 15


pageTitle : Theme -> String -> Element msg
pageTitle theme text =
    Element.el
        [ pageTitleSize
        , Font.semiBold
        , Font.color theme.textPrimary
        , Region.heading 1
        ]
        (Element.text text)


delimitedPageTitle : Theme -> String -> String -> Element msg
delimitedPageTitle theme delimiter text =
    naturalWrapText
        [ pageTitleSize
        , Font.semiBold
        , Font.color theme.textPrimary
        , Region.heading 1
        ]
        delimiter
        text


sectionTitle : Theme -> String -> Element msg
sectionTitle theme text =
    Element.el
        [ sectionTitleSize
        , Font.semiBold
        , Font.color theme.textPrimary
        , Region.heading 2
        ]
        (Element.text text)


delimitedSectionTitle : Theme -> String -> String -> Element msg
delimitedSectionTitle theme delimiter text =
    naturalWrapText
        [ sectionTitleSize
        , Font.semiBold
        , Font.color theme.textPrimary
        , Region.heading 2
        ]
        delimiter
        text


subsectionTitle : Theme -> String -> Element msg
subsectionTitle theme text =
    Element.el
        [ subsectionTitleSize
        , Font.semiBold
        , Font.color theme.textPrimary
        , Region.heading 3
        ]
        (Element.text text)


mutedBody : Theme -> String -> Element msg
mutedBody theme text =
    Element.el
        [ body
        , Font.color theme.textMuted
        ]
        (Element.text text)


mutedCaption : Theme -> String -> Element msg
mutedCaption theme text =
    Element.el
        [ caption
        , Font.color theme.textMuted
        ]
        (Element.text text)


mutedParagraph : Theme -> String -> Element msg
mutedParagraph theme text =
    Element.paragraph
        [ body
        , Font.color theme.textMuted
        , Element.spacing 4
        ]
        [ Element.text text ]


listItemTitle : Theme -> Element msg -> Element msg
listItemTitle theme child =
    Element.el
        [ body
        , Font.semiBold
        , Font.color theme.textPrimary
        , minWidthZero
        ]
        child


truncatedListItemTitle : Theme -> String -> Element msg
truncatedListItemTitle theme text =
    Element.el
        [ Element.width Element.fill
        , minWidthZero
        ]
        (listItemTitle theme
            (Element.el
                [ Element.width Element.fill
                , Element.clip
                , uiClass "ui-text-ellipsis"
                ]
                (Element.text text)
            )
        )


wrappedListItemTitle : Theme -> String -> Element msg
wrappedListItemTitle theme text =
    listItemTitle theme
        (Element.paragraph
            [ Element.width Element.fill
            , minWidthZero
            , uiClass "ui-text-wrap"
            ]
            [ Element.text text ]
        )


naturalWrapText : List (Element.Attribute msg) -> String -> String -> Element msg
naturalWrapText attributes delimiter value =
    if delimiter == "" then
        Element.paragraph
            ([ Element.width Element.fill
             , minWidthZero
             ]
                ++ attributes
            )
            [ Element.text value ]

    else
        Element.wrappedRow
            ([ Element.width Element.fill
             , Element.spacing 0
             , minWidthZero
             ]
                ++ attributes
            )
            (delimitedTextSegments delimiter value)


delimitedTextSegments : String -> String -> List (Element msg)
delimitedTextSegments delimiter value =
    let
        segments =
            String.split delimiter value

        lastIndex =
            List.length segments - 1
    in
    segments
        |> List.indexedMap
            (\index segment ->
                Element.el [ Element.width Element.shrink ]
                    (Element.text
                        (if index == lastIndex then
                            segment

                         else
                            segment ++ delimiter
                        )
                    )
            )


uiClass : String -> Element.Attribute msg
uiClass className =
    Element.htmlAttribute (Html.Attributes.class className)


minWidthZero : Element.Attribute msg
minWidthZero =
    Element.htmlAttribute (Html.Attributes.style "min-width" "0")
