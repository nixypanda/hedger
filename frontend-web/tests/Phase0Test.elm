module Phase0Test exposing (suite)

import Expect
import Test exposing (Test, test)


suite : Test
suite =
    test "workspace is established" <|
        \_ -> Expect.pass
