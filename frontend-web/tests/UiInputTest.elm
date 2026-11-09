module UiInputTest exposing (suite)

import Element
import Expect
import Html.Attributes
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Ui.Input
import Ui.Theme as Theme


suite : Test
suite =
    describe "segmented control"
        [ test "exactly one option is pressed" <|
            \_ ->
                segmentedView 2 twoOptions
                    |> pressedOptions
                    |> Query.count (Expect.equal 1)
        , test "the pressed option matches the selected value" <|
            \_ ->
                segmentedView 2 twoOptions
                    |> Query.find [ Selector.class "ui-segment", pressed ]
                    |> Query.has [ Selector.text "2" ]
        , test "exactly one indicator renders for the selected value" <|
            \_ ->
                segmentedView 2 twoOptions
                    |> indicators
                    |> Query.count (Expect.equal 1)
        , test "the indicator is translated to the selected index" <|
            \_ ->
                segmentedView 2 twoOptions
                    |> Query.find [ Selector.class "ui-segment-indicator" ]
                    |> Query.has
                        [ Selector.style
                            "transform"
                            "translateX(calc(1 * (100% + 2px)))"
                        ]
        , test "the indicator is hidden from assistive technology" <|
            \_ ->
                segmentedView 2 twoOptions
                    |> Query.find [ Selector.class "ui-segment-indicator" ]
                    |> Query.has [ hidden ]
        , test "a one-option control still renders an indicator" <|
            \_ ->
                segmentedView 1 oneOption
                    |> indicators
                    |> Query.count (Expect.equal 1)
        , test "an absent selected value renders no indicator" <|
            \_ ->
                segmentedView 9 twoOptions
                    |> Query.hasNot [ Selector.class "ui-segment-indicator" ]
        , test "an absent selected value marks no option pressed" <|
            \_ ->
                segmentedView 9 twoOptions
                    |> pressedOptions
                    |> Query.count (Expect.equal 0)
        ]


segmentedView : Int -> Ui.Input.Segments Int -> Query.Single ()
segmentedView selected options =
    Ui.Input.segmentedControl Theme.light
        { label = "Test options"
        , selected = selected
        , options = options
        , onSelect = always ()
        }
        |> Element.layout []
        |> Query.fromHtml


twoOptions : Ui.Input.Segments Int
twoOptions =
    { first = { label = "1", value = 1 }
    , rest = [ { label = "2", value = 2 } ]
    }


oneOption : Ui.Input.Segments Int
oneOption =
    { first = { label = "1", value = 1 }
    , rest = []
    }


pressed : Selector
pressed =
    Selector.attribute (Html.Attributes.attribute "aria-pressed" "true")


hidden : Selector
hidden =
    Selector.attribute (Html.Attributes.attribute "aria-hidden" "true")


pressedOptions : Query.Single msg -> Query.Multiple msg
pressedOptions query =
    Query.findAll [ Selector.class "ui-segment", pressed ] query


indicators : Query.Single msg -> Query.Multiple msg
indicators query =
    Query.findAll [ Selector.class "ui-segment-indicator" ] query
