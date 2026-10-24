module Ui.Calendar.Month exposing (view)

import Data.Calendar as Calendar
import Element exposing (Element)
import Element.Font as Font
import Html.Attributes as Attr
import Ui.Typography as Typography


view :
    { period : String
    , weekdayLabels : List String
    , weekdayColor : Element.Color
    , cellHeight : Int
    , viewDay : Int -> List (Element.Attribute msg) -> Element msg
    }
    -> Element msg
view config =
    let
        attributes =
            [ Element.width (Element.fillPortion 1)
            , Element.height Element.fill
            , equalColumn
            ]

        cell maybeDay =
            case maybeDay of
                Nothing ->
                    Element.el attributes Element.none

                Just day ->
                    config.viewDay day attributes

        rows =
            Calendar.monthWeeks config.period
                |> List.map
                    (List.map cell
                        >> Element.row
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            ]
                    )
    in
    Element.column [ Element.width Element.fill ]
        [ Element.row [ Element.width Element.fill ]
            (List.map
                (\label ->
                    Element.el
                        (Typography.caption
                            ++ [ Element.width (Element.fillPortion 1)
                               , equalColumn
                               , Font.center
                               , Font.color config.weekdayColor
                               ]
                        )
                        (Element.text label)
                )
                config.weekdayLabels
            )
        , Element.column
            [ Element.width Element.fill
            , Element.height (Element.px (6 * config.cellHeight))
            ]
            rows
        ]


equalColumn : Element.Attribute msg
equalColumn =
    Element.htmlAttribute (Attr.attribute "style" "flex: 1 1 0; min-width: 0")
