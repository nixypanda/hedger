module ViewQueryPanelTest exposing (suite)

import Element
import Expect
import Html.Attributes
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Ui.Theme as Theme
import View.QueryPanel as QueryPanel


suite : Test
suite =
    describe "query panel"
        [ test "Apply keeps the primary tone and applies the draft" <|
            \_ ->
                queryPanelView False
                    |> applyButton
                    |> Expect.all
                        [ \button -> Query.has [ Selector.class "ui-button-primary" ] button
                        , \button -> clickExpecting Applied button
                        ]
        , test "Clear keeps the danger tone and clears the draft" <|
            \_ ->
                queryPanelView False
                    |> clearButton
                    |> Expect.all
                        [ \button -> Query.has [ Selector.class "ui-button-danger" ] button
                        , \button -> clickExpecting Cleared button
                        ]
        , test "the compact layout preserves the Apply action" <|
            \_ ->
                queryPanelView True
                    |> applyButton
                    |> clickExpecting Applied
        , test "a query error still renders its notice beside the actions" <|
            \_ ->
                queryPanelViewWithError
                    |> Query.has
                        [ Selector.attribute
                            (Html.Attributes.id "test-query-error")
                        ]
        ]


type Msg
    = Applied
    | Cleared
    | DraftChanged String


config : QueryPanel.Config Msg
config =
    { theme = Theme.light
    , compact = False
    , draftQuery = "acct:expenses"
    , onDraftChanged = DraftChanged
    , onApply = Applied
    , onClear = Cleared
    , queryError = Nothing
    , helpText = "Filter transactions."
    , examples = [ "acct:assets" ]
    , baseId = "test-query"
    }


queryPanelView : Bool -> Query.Single Msg
queryPanelView compact =
    QueryPanel.view { config | compact = compact }
        |> Element.layout []
        |> Query.fromHtml


queryPanelViewWithError : Query.Single Msg
queryPanelViewWithError =
    QueryPanel.view { config | queryError = Just "The query is invalid." }
        |> Element.layout []
        |> Query.fromHtml


applyButton : Query.Single msg -> Query.Single msg
applyButton query =
    Query.find [ roleButton, Selector.containing [ Selector.text "Apply" ] ] query


clearButton : Query.Single msg -> Query.Single msg
clearButton query =
    Query.find [ roleButton, Selector.containing [ Selector.text "Clear" ] ] query


clickExpecting : msg -> Query.Single msg -> Expect.Expectation
clickExpecting expected query =
    query
        |> Event.simulate Event.click
        |> Event.toResult
        |> Expect.equal (Ok expected)


roleButton : Selector
roleButton =
    Selector.attribute (Html.Attributes.attribute "role" "button")
