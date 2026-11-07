module AnimationTest exposing (suite)

import Element
import Expect
import Test exposing (Test, describe, test)
import Time
import Ui.Animation as Animation


suite : Test
suite =
    describe "Animation transition rendering"
        [ test "a pending transition does not render stale keyframes before its first tick" <|
            \_ ->
                Animation.init False
                    |> Animation.to { reducedMotion = False } 220 True
                    |> renderAttributes
                    |> Expect.equal []
        , test "the first tick starts rendering animation attributes" <|
            \_ ->
                Animation.init False
                    |> Animation.to { reducedMotion = False } 220 True
                    |> Animation.update (Time.millisToPosix 10)
                    |> renderAttributes
                    |> Expect.notEqual []
        , test "reduced motion renders no animation attributes" <|
            \_ ->
                Animation.init False
                    |> Animation.to { reducedMotion = True } 220 True
                    |> renderAttributes
                    |> Expect.equal []
        , test "an interruption keeps the previous timeline until its first tick" <|
            \_ ->
                let
                    running =
                        Animation.init False
                            |> Animation.to { reducedMotion = False } 220 True
                            |> Animation.update (Time.millisToPosix 10)

                    interrupted =
                        Animation.to { reducedMotion = False } 220 False running
                in
                ( List.isEmpty (renderAttributes running)
                , List.isEmpty (renderAttributes interrupted)
                )
                    |> Expect.equal ( False, False )
        ]


renderAttributes : Animation.Transition Bool -> List (Element.Attribute msg)
renderAttributes transition =
    Animation.render
        transition
        (\visible ->
            Animation.Fade
                (if visible then
                    1

                 else
                    0
                )
        )
        |> .attributes
