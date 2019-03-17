module DipSwitchCalc exposing (convertBinaryToInt, convertIntToBinary, dipSwitchCalc, validAddress)

import Array exposing (Array)
import Element exposing (Color, Element, fill, maximum, minimum, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes as Attributes


type Bit
    = Bit Bool


type Binary
    = Binary (List Bit)


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


dipSwitchView : (Int -> msg) -> Int -> Element msg
dipSwitchView msg address =
    let
        mappedMessage =
            mapBinaryMessage msg (convertIntToBinary address)
    in
    address
        |> convertIntToBinary
        |> unwrapBinary
        |> List.indexedMap (dipView mappedMessage)
        |> List.reverse
        |> List.indexedMap (\i f -> f (i + 1))
        |> Element.row [ Element.spacing 10, Element.padding 10 ]


dipSwitchCalc : (Int -> msg) -> Int -> Element msg
dipSwitchCalc msg address =
    Element.column [ Element.centerX ]
        [ dipSwitchView msg address
        , numberView msg address
        ]


updateBinary : Int -> Bit -> Binary -> Int
updateBinary ix bit binary =
    binary
        |> unwrapBinary
        |> updateInIndex ix bit
        |> Binary
        |> convertBinaryToInt


updateInIndex : Int -> a -> List a -> List a
updateInIndex ix a list =
    let
        head =
            List.take ix list

        tail =
            List.drop (ix + 1) list
    in
    head ++ [ a ] ++ tail


mapBinaryMessage : (Int -> msg) -> Binary -> (Int -> Bool -> msg)
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


numberView : (Int -> msg) -> Int -> Element msg
numberView msg num =
    let
        inputText =
            if num < 0 then
                ""

            else
                String.fromInt num
    in
    Input.text ([] ++ numericAttributes)
        { label = Input.labelHidden "DMX Address"
        , onChange = mapInputMessage msg
        , placeholder = Nothing
        , text = inputText
        }


mapInputMessage : (Int -> msg) -> (String -> msg)
mapInputMessage msg =
    \str -> msg <| Maybe.withDefault 0 <| String.toInt str


validAddress : Int -> Bool
validAddress value =
    not <| (value > 512 || value < 0)


{-| Attributes for input to use numeric keyboard on iOS.
-}
numericAttributes : List (Element.Attribute msg)
numericAttributes =
    [ Element.htmlAttribute <| Attributes.pattern "[0-9]*"
    , Element.htmlAttribute <| Attributes.attribute "inputmode" "numeric"
    , Element.htmlAttribute <| Attributes.type_ "number"
    ]


red : Color
red =
    Element.rgb255 255 0 0


green : Color
green =
    Element.rgb255 0 170 30


grey : Color
grey =
    Element.rgb255 125 125 125
