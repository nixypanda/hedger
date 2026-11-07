module PushPopTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Time
import Ui.PushPop as PushPop


suite : Test
suite =
    describe "Push-pop transition"
        [ test "open targets detail immediately while current waits for the first tick" <|
            \_ ->
                let
                    opened =
                        open False (PushPop.init PushPop.ListPane)
                in
                ( PushPop.target opened
                , PushPop.current opened
                , PushPop.isRunning opened
                )
                    |> Expect.equal ( PushPop.DetailPane, PushPop.ListPane, True )
        , test "the first tick moves current to the open target" <|
            \_ ->
                open False (PushPop.init PushPop.ListPane)
                    |> PushPop.update (Time.millisToPosix 1000)
                    |> PushPop.current
                    |> Expect.equal PushPop.DetailPane
        , test "reduced motion opens immediately without a timeline" <|
            \_ ->
                let
                    opened =
                        open True (PushPop.init PushPop.ListPane)
                in
                ( PushPop.target opened, PushPop.current opened, PushPop.isRunning opened )
                    |> Expect.equal ( PushPop.DetailPane, PushPop.DetailPane, False )
        , test "close targets the list before the first tick" <|
            \_ ->
                let
                    closed =
                        open True (PushPop.init PushPop.ListPane)
                            |> PushPop.close { reducedMotion = False, distance = 390 }
                in
                ( PushPop.target closed, PushPop.current closed )
                    |> Expect.equal ( PushPop.ListPane, PushPop.DetailPane )
        , test "a snapped transition has no pending arrival" <|
            \_ ->
                PushPop.arrivedAt PushPop.ListPane
                    (Time.millisToPosix 1000)
                    (PushPop.init PushPop.ListPane)
                    |> Expect.equal False
        , test "the outgoing list is tracked from the first open frame" <|
            \_ ->
                PushPop.outgoing (open False (PushPop.init PushPop.ListPane))
                    |> Expect.equal (Just PushPop.ListPane)
        , test "closing mid-open snaps back to the list" <|
            \_ ->
                let
                    snapped =
                        open False (PushPop.init PushPop.ListPane)
                            |> PushPop.update (Time.millisToPosix 100)
                            |> PushPop.close { reducedMotion = False, distance = 390 }
                in
                { target = PushPop.target snapped
                , current = PushPop.current snapped
                , outgoing = PushPop.outgoing snapped
                , running = PushPop.isRunning snapped
                }
                    |> Expect.equal
                        { target = PushPop.ListPane
                        , current = PushPop.ListPane
                        , outgoing = Nothing
                        , running = False
                        }
        , test "closing before the first open tick cancels back to the list" <|
            \_ ->
                let
                    cancelled =
                        open False (PushPop.init PushPop.ListPane)
                            |> PushPop.close { reducedMotion = False, distance = 390 }
                in
                { target = PushPop.target cancelled
                , current = PushPop.current cancelled
                , outgoing = PushPop.outgoing cancelled
                , running = PushPop.isRunning cancelled
                }
                    |> Expect.equal
                        { target = PushPop.ListPane
                        , current = PushPop.ListPane
                        , outgoing = Nothing
                        , running = False
                        }
        , test "opening during a close snaps back to the detail" <|
            \_ ->
                let
                    snapped =
                        PushPop.init PushPop.DetailPane
                            |> PushPop.close { reducedMotion = False, distance = 390 }
                            |> PushPop.update (Time.millisToPosix 100)
                            |> open False
                in
                { target = PushPop.target snapped
                , current = PushPop.current snapped
                , outgoing = PushPop.outgoing snapped
                , running = PushPop.isRunning snapped
                }
                    |> Expect.equal
                        { target = PushPop.DetailPane
                        , current = PushPop.DetailPane
                        , outgoing = Nothing
                        , running = False
                        }
        , test "opening before the first close tick cancels back to the detail" <|
            \_ ->
                let
                    cancelled =
                        PushPop.init PushPop.DetailPane
                            |> PushPop.close { reducedMotion = False, distance = 390 }
                            |> open False
                in
                { target = PushPop.target cancelled
                , current = PushPop.current cancelled
                , outgoing = PushPop.outgoing cancelled
                , running = PushPop.isRunning cancelled
                }
                    |> Expect.equal
                        { target = PushPop.DetailPane
                        , current = PushPop.DetailPane
                        , outgoing = Nothing
                        , running = False
                        }
        ]


open : Bool -> PushPop.Transition -> PushPop.Transition
open reducedMotion model =
    PushPop.open { reducedMotion = reducedMotion, distance = 390 } model
