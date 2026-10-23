module Ui.Typography exposing
    ( Size(..)
    , Weight(..)
    , body
    , bodyLarge
    , caption
    , display
    , fontFamily
    , fontSize
    , fontWeight
    , heading
    , headline
    , htmlCaption
    , htmlFontWeight
    , label
    , labelSmall
    , mutedBody
    , mutedCaption
    , mutedParagraph
    , naturalWrapText
    , numeric
    , pageTitle
    , pageTitleWithDelimiter
    , sectionTitle
    , subheading
    , subsectionTitle
    )

import Element exposing (Element)
import Element.Font as Font
import Element.Region as Region
import Html
import Html.Attributes
import Ui.Common as Common
import Ui.DesignTokens exposing (space4)
import Ui.Theme exposing (Theme)


{-| The generic type scale. Sizes are intentionally coarse and are the only
font sizes used by the application; add a new step here rather than a literal
size in a component.

    xs 12, sm 14, md 16, lg 20, xl 24, 2xl 32, 3xl 44

-}
type Size
    = Xs
    | Sm
    | Md
    | Lg
    | Xl
    | Xxl
    | Xxxl


type Weight
    = Regular
    | Medium
    | SemiBold
    | Bold


fontSize : Size -> Element.Attribute msg
fontSize size =
    Font.size (sizeValue size)


fontWeight : Weight -> Element.Attribute msg
fontWeight weight =
    case weight of
        Regular ->
            Font.regular

        Medium ->
            Font.medium

        SemiBold ->
            Font.semiBold

        Bold ->
            Font.bold


fontFamily : Element.Attribute msg
fontFamily =
    Font.family [ Font.typeface "Inter", Font.sansSerif ]



-- Generic text roles
--
-- These are the only named text styles the application should use. Keep them
-- free of ledger, screen, or component vocabulary; compose those at the call
-- site instead.


{-| Reserved for future display/hero text; keep the 44 step in the scale.
-}
display : List (Element.Attribute msg)
display =
    [ fontSize Xxxl, fontWeight Bold ]


headline : List (Element.Attribute msg)
headline =
    [ fontSize Xxl, fontWeight Bold ]


heading : List (Element.Attribute msg)
heading =
    [ fontSize Xl, fontWeight Bold ]


subheading : List (Element.Attribute msg)
subheading =
    [ fontSize Lg, fontWeight Bold ]


body : List (Element.Attribute msg)
body =
    [ fontSize Sm ]


bodyLarge : List (Element.Attribute msg)
bodyLarge =
    [ fontSize Md ]


label : List (Element.Attribute msg)
label =
    [ fontSize Sm, fontWeight Medium ]


labelSmall : List (Element.Attribute msg)
labelSmall =
    [ fontSize Xs, fontWeight Medium ]


caption : List (Element.Attribute msg)
caption =
    [ fontSize Xs ]


numeric : List (Element.Attribute msg)
numeric =
    [ Font.variant Font.tabularNumbers ]


htmlCaption : Html.Attribute msg
htmlCaption =
    Html.Attributes.style "font-size" (String.fromInt (sizeValue Xs) ++ "px")


htmlFontWeight : Weight -> Html.Attribute msg
htmlFontWeight weight =
    Html.Attributes.style "font-weight" (weightValue weight)



-- Document text roles


pageTitle : Theme -> String -> Element msg
pageTitle theme text =
    naturalWrapText
        (headline ++ [ Font.color theme.textPrimary, Region.heading 1 ])
        ""
        text


pageTitleWithDelimiter : Theme -> String -> String -> Element msg
pageTitleWithDelimiter theme delimiter text =
    naturalWrapText
        (headline ++ [ Font.color theme.textPrimary, Region.heading 1 ])
        delimiter
        text


sectionTitle : Theme -> String -> Element msg
sectionTitle theme text =
    Element.el
        (heading ++ [ Font.color theme.textPrimary, Region.heading 2 ])
        (Element.text text)


subsectionTitle : Theme -> String -> Element msg
subsectionTitle theme text =
    Element.el
        (subheading ++ [ Font.color theme.textPrimary, Region.heading 3 ])
        (Element.text text)


mutedBody : Theme -> String -> Element msg
mutedBody theme text =
    Element.el
        (body ++ [ Font.color theme.textMuted ])
        (Element.text text)


mutedCaption : Theme -> String -> Element msg
mutedCaption theme text =
    Element.el
        (caption ++ [ Font.color theme.textMuted ])
        (Element.text text)


mutedParagraph : Theme -> String -> Element msg
mutedParagraph theme text =
    Element.paragraph
        (body
            ++ [ Font.color theme.textMuted
               , Element.spacing space4
               ]
        )
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


sizeValue : Size -> Int
sizeValue size =
    case size of
        Xs ->
            12

        Sm ->
            14

        Md ->
            16

        Lg ->
            20

        Xl ->
            24

        Xxl ->
            32

        Xxxl ->
            44


weightValue : Weight -> String
weightValue weight =
    case weight of
        Regular ->
            "400"

        Medium ->
            "500"

        SemiBold ->
            "600"

        Bold ->
            "700"
