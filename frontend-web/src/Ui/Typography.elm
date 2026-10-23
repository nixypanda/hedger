module Ui.Typography exposing
    ( body
    , brandLabel
    , caption
    , chartLabel
    , controlLabel
    , delimitedPageTitle
    , detailHeroValue
    , detailLabel
    , detailValue
    , financeNumber
    , heroStat
    , htmlCaption
    , kpiValue
    , mutedBody
    , mutedCaption
    , mutedParagraph
    , naturalWrapText
    , pageTitle
    , rowAmount
    , rowDate
    , rowMetadata
    , rowTitle
    , sectionTitle
    , subsectionTitle
    , tabLabel
    )

import Element exposing (Element)
import Element.Font as Font
import Element.Region as Region
import Html exposing (Attribute)
import Html.Attributes
import Ui.Common as Common
import Ui.Theme exposing (Theme)


body : Element.Attribute msg
body =
    Font.size 14


controlLabel : Element.Attribute msg
controlLabel =
    Font.size 12


caption : Element.Attribute msg
caption =
    Font.size 12


rowDate : Element.Attribute msg
rowDate =
    Font.size 12


rowTitle : List (Element.Attribute msg)
rowTitle =
    [ Font.size 14, Font.bold ]


rowAmount : Element.Attribute msg
rowAmount =
    Font.size 12


rowMetadata : Element.Attribute msg
rowMetadata =
    Font.size 12


detailLabel : List (Element.Attribute msg)
detailLabel =
    [ Font.size 13, Font.medium ]


detailValue : Element.Attribute msg
detailValue =
    Font.size 16


detailHeroValue : List (Element.Attribute msg)
detailHeroValue =
    [ Font.size 32, Font.bold ]


chartLabel : Element.Attribute msg
chartLabel =
    Font.size 14


kpiValue : List (Element.Attribute msg)
kpiValue =
    [ Font.size 32, Font.bold ]


tabLabel : Element.Attribute msg
tabLabel =
    Font.size 13


htmlCaption : Attribute msg
htmlCaption =
    Html.Attributes.style "font-size" "13px"


brandLabel : List (Element.Attribute msg)
brandLabel =
    [ Font.size 20, Font.bold ]


financeNumber : Element.Attribute msg
financeNumber =
    Font.variant Font.tabularNumbers


heroStat : Theme -> String -> Element msg
heroStat theme text =
    Element.el
        [ Font.size 44
        , Font.bold
        , Font.color theme.textPrimary
        ]
        (Element.text text)


pageTitleSize : Element.Attribute msg
pageTitleSize =
    Font.size 32


sectionTitleSize : Element.Attribute msg
sectionTitleSize =
    Font.size 24


subsectionTitleSize : Element.Attribute msg
subsectionTitleSize =
    Font.size 20


pageTitle : Theme -> String -> Element msg
pageTitle theme text =
    naturalWrapText
        [ pageTitleSize
        , Font.bold
        , Font.color theme.textPrimary
        , Region.heading 1
        ]
        ""
        text


delimitedPageTitle : Theme -> String -> String -> Element msg
delimitedPageTitle theme delimiter text =
    naturalWrapText
        [ pageTitleSize
        , Font.bold
        , Font.color theme.textPrimary
        , Region.heading 1
        ]
        delimiter
        text


sectionTitle : Theme -> String -> Element msg
sectionTitle theme text =
    Element.el
        [ sectionTitleSize
        , Font.bold
        , Font.color theme.textPrimary
        , Region.heading 2
        ]
        (Element.text text)


subsectionTitle : Theme -> String -> Element msg
subsectionTitle theme text =
    Element.el
        [ subsectionTitleSize
        , Font.bold
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


naturalWrapText : List (Element.Attribute msg) -> String -> String -> Element msg
naturalWrapText attributes delimiter value =
    if delimiter == "" then
        Element.paragraph
            ([ Element.width Element.fill
             , Common.minWidthZero
             ]
                ++ attributes
            )
            [ Element.text value ]

    else
        Element.wrappedRow
            ([ Element.width Element.fill
             , Element.spacing 0
             , Common.minWidthZero
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
