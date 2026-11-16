module UiButtonTest exposing (suite)

import Element exposing (Element)
import Expect
import Html.Attributes
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Theme as Theme


suite : Test
suite =
    describe "buttons"
        [ describe "text tones"
            [ test "a primary button uses the primary tone and visible label" <|
                \_ ->
                    primaryView
                        |> buttonQuery
                        |> Query.has
                            [ Selector.class "ui-button-primary"
                            , Selector.text "Save"
                            ]
            ]
        , describe "submission state"
            [ test "an idle button reports that it is neither busy nor disabled" <|
                \_ ->
                    primaryView
                        |> buttonQuery
                        |> Query.has [ ariaBusy "false", ariaDisabled "false" ]
            ]
        , describe "icon buttons"
            [ test "an icon button exposes its title as the accessible name" <|
                \_ ->
                    Button.iconButtonWithTone Theme.light
                        { icon = Icon.ChevronLeft
                        , title = "Back"
                        , onPress = Just Pressed
                        , tone = Button.Ghost
                        }
                        |> render
                        |> buttonQuery
                        |> Query.has
                            [ ariaLabel "Back"
                            , Selector.attribute (Html.Attributes.title "Back")
                            ]
            , test "a disabled icon button is not pressable" <|
                \_ ->
                    Button.iconButtonWithTone Theme.light
                        { icon = Icon.Refresh
                        , title = "Refresh"
                        , onPress = Nothing
                        , tone = Button.Ghost
                        }
                        |> render
                        |> buttonQuery
                        |> expectUnpressable [ ariaDisabled "true" ]
            , test "an unavailable icon button stays pressable for its nudge" <|
                \_ ->
                    unavailableIconView
                        |> buttonQuery
                        |> expectPressable
                            [ ariaLabel "Next period (At the latest period)"
                            , Selector.attribute
                                (Html.Attributes.title "At the latest period")
                            , ariaDisabled "true"
                            ]
                            Blocked
            ]
        , describe "refresh"
            [ test "idle refresh is named and pressable" <|
                \_ ->
                    refreshView False
                        |> buttonQuery
                        |> expectPressable
                            [ ariaLabel "Refresh"
                            , ariaBusy "false"
                            , ariaDisabled "false"
                            ]
                            Pressed
            , test "loading refresh stays busy and not pressable" <|
                \_ ->
                    refreshView True
                        |> buttonQuery
                        |> expectUnpressable
                            [ ariaLabel "Refreshing"
                            , ariaBusy "true"
                            , ariaDisabled "true"
                            , Selector.attribute (Html.Attributes.attribute "inert" "")
                            ]
            ]
        , describe "unavailable actions"
            [ test "an unavailable button keeps the reason in its accessible name" <|
                \_ ->
                    unavailableView
                        |> buttonQuery
                        |> expectPressable
                            [ ariaLabel "Load more (Finish refreshing first.)"
                            , Selector.attribute
                                (Html.Attributes.title "Finish refreshing first.")
                            , ariaDisabled "true"
                            ]
                            Blocked
            ]
        ]


type Msg
    = Pressed
    | Blocked


primaryConfig : Button.ButtonConfig Msg
primaryConfig =
    { tone = Button.Primary
    , width = Button.ContentWidth
    , label = "Save"
    , onPress = Pressed
    , submitState = Nothing
    , submittingLabel = Nothing
    , trailing = Nothing
    }


primaryView : Query.Single Msg
primaryView =
    Button.textButton Theme.light primaryConfig
        |> render


unavailableView : Query.Single Msg
unavailableView =
    Button.unavailableButton Theme.light
        { label = "Load more"
        , reason = "Finish refreshing first."
        , nudge = Just 1
        , onPress = Blocked
        }
        |> render


unavailableIconView : Query.Single Msg
unavailableIconView =
    Button.unavailableIconButton Theme.light
        { icon = Icon.ChevronRight
        , title = "Next period"
        , reason = "At the latest period"
        , nudge = Just 2
        , onPress = Blocked
        }
        |> render


refreshView : Bool -> Query.Single Msg
refreshView loading =
    Button.refreshButton Theme.light { loading = loading, onPress = Pressed }
        |> render


render : Element msg -> Query.Single msg
render element =
    element
        |> Element.layout []
        |> Query.fromHtml


buttonQuery : Query.Single msg -> Query.Single msg
buttonQuery query =
    Query.find [ roleButton ] query


expectPressable :
    List Selector
    -> msg
    -> Query.Single msg
    -> Expect.Expectation
expectPressable selectors expected query =
    Expect.all
        [ \_ -> Query.has selectors query
        , \_ -> expectClick expected query
        ]
        ()


expectUnpressable : List Selector -> Query.Single msg -> Expect.Expectation
expectUnpressable selectors query =
    Expect.all
        [ \_ -> Query.has selectors query
        , \_ -> expectNoClick query
        ]
        ()


expectClick : msg -> Query.Single msg -> Expect.Expectation
expectClick expected query =
    query
        |> Event.simulate Event.click
        |> Event.toResult
        |> Result.toMaybe
        |> Expect.equal (Just expected)


expectNoClick : Query.Single msg -> Expect.Expectation
expectNoClick query =
    query
        |> Event.simulate Event.click
        |> Event.toResult
        |> Result.toMaybe
        |> Expect.equal Nothing


roleButton : Selector
roleButton =
    Selector.attribute (Html.Attributes.attribute "role" "button")


ariaBusy : String -> Selector
ariaBusy value =
    Selector.attribute (Html.Attributes.attribute "aria-busy" value)


ariaDisabled : String -> Selector
ariaDisabled value =
    Selector.attribute (Html.Attributes.attribute "aria-disabled" value)


ariaLabel : String -> Selector
ariaLabel value =
    Selector.attribute (Html.Attributes.attribute "aria-label" value)
