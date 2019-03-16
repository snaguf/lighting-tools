module DipSwitchCalc exposing (Binary, convertBinaryToInt, convertIntToBinary, dipSwitchView, zeroBinary)

import Array exposing (Array)
import Element exposing (Color, Element, fill, maximum, minimum, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input


type Bit
    = Bit Bool


type Binary
    = Binary (List Bit)


zeroBinary : Binary
zeroBinary =
    Binary <| List.repeat 10 (Bit False)


convertBinaryToInt : Binary -> Int
convertBinaryToInt binary =
    binary
        |> unwrapBinary
        |> List.map bitToInt
        |> List.foldl (\bit sum -> 2 * sum + bit) 0


convertIntToBinary : Int -> Binary
convertIntToBinary num =
    divideIntoBits num []
        |> List.map numToBit
        |> addDummyBits 10
        |> Binary


addDummyBits : Int -> List Bit -> List Bit
addDummyBits size bits =
    List.repeat (size - List.length bits) (Bit False) ++ bits


unwrapBit : Bit -> Bool
unwrapBit bit =
    case bit of
        Bit value ->
            value


unwrapBinary : Binary -> List Bit
unwrapBinary binary =
    case binary of
        Binary bits ->
            bits


bitToInt : Bit -> Int
bitToInt bit =
    if unwrapBit bit then
        1

    else
        0


numToBit : Int -> Bit
numToBit num =
    case num of
        1 ->
            Bit True

        _ ->
            Bit False


divideIntoBits : Int -> List Int -> List Int
divideIntoBits num bits =
    if num == 0 then
        bits

    else
        divideIntoBits (num // 2) (modBy 2 num :: bits)


dipView : (Int -> Bool -> msg) -> Int -> Bit -> (Int -> Element msg)
dipView msg ix bit =
    \value ->
        Input.checkbox []
            { onChange = msg ix
            , icon = bitIcon
            , checked = unwrapBit bit
            , label = Input.labelBelow [ Element.centerX ] <| Element.text <| String.fromInt value
            }


dipSwitchView : (Binary -> msg) -> Binary -> Element msg
dipSwitchView msg binary =
    let
        mappedMessage =
            mapBinaryMessage msg binary
    in
    binary
        |> unwrapBinary
        |> List.indexedMap (dipView mappedMessage)
        |> List.reverse
        |> List.indexedMap (\i f -> f (i + 1))
        |> Element.row [ Element.spacing 10, Element.padding 10 ]


updateBinary : Int -> Bit -> Binary -> Binary
updateBinary ix bit binary =
    binary
        |> unwrapBinary
        |> updateInIndex ix bit
        |> Binary


updateInIndex : Int -> a -> List a -> List a
updateInIndex ix a list =
    let
        head =
            List.take ix list

        tail =
            List.drop (ix + 1) list
    in
    head ++ [ a ] ++ tail


mapBinaryMessage : (Binary -> msg) -> Binary -> (Int -> Bool -> msg)
mapBinaryMessage f binary =
    \ix value -> f (updateBinary ix (Bit value) binary)


bitIcon : Bool -> Element msg
bitIcon on =
    let
        ( color, alignment ) =
            if on then
                ( green, Element.alignTop )

            else
                ( red, Element.alignBottom )

        list =
            [ Element.el [ Element.width fill, Element.height fill, Border.rounded 5, Background.color grey, Border.width 2 ] Element.none
            , Element.el [ Element.width fill, Element.height fill, Border.rounded 5 ] Element.none
            ]

        switch =
            if on then
                list

            else
                List.reverse list
    in
    Element.column
        [ Element.width (fill |> minimum 20)
        , Element.height (fill |> minimum 80)
        , Background.color color
        , Element.centerX
        , Element.padding 3
        , Border.rounded 5
        , Border.width 2
        ]
        switch


red : Color
red =
    Element.rgb255 255 0 0


green : Color
green =
    Element.rgb255 0 170 30


grey : Color
grey =
    Element.rgb255 125 125 125
