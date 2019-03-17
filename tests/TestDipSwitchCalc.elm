module TestDipSwitchCalc exposing (suite)

import DipSwitchCalc exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Dip Switch Calculator"
        [ binaryLogic
        ]


binaryLogic : Test
binaryLogic =
    describe "Binary logic"
        [ test "Converts back to back" <|
            \_ ->
                Expect.equal 2 (convertBinaryToInt (convertIntToBinary 2))
        ]
