module LightingTools exposing (Model, Msg, init, subscriptions, update, view)

import DipSwitchCalc exposing (Binary, convertBinaryToInt, convertIntToBinary, dipSwitchView, zeroBinary)
import Element exposing (Element)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes



-- MODEL


type alias Model =
    { title : String
    , address : Int
    , binary : Binary
    }


initialModel : Model
initialModel =
    { title = "Lighting Tools"
    , address = 0
    , binary = zeroBinary
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = DipChanged Binary
    | AddressChanged (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DipChanged value ->
            ( updateAddress (convertBinaryToInt value) model, Cmd.none )

        AddressChanged value ->
            ( updateAddress (Maybe.withDefault 0 value) model
            , Cmd.none
            )


updateAddress : Int -> { a | address : Int, binary : Binary } -> { a | address : Int, binary : Binary }
updateAddress value model =
    if value > 512 || value < 0 then
        model

    else
        { model | address = value, binary = convertIntToBinary value }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ Element.centerX ]
            [ dipSwitchView DipChanged model.binary
            , numberView model.address
            ]


numberView : Int -> Element Msg
numberView num =
    let
        input =
            if num <= 0 then
                ""

            else
                String.fromInt num
    in
    Input.text ([] ++ numericAttributes)
        { label = Input.labelHidden "DMX Address"
        , onChange = \value -> AddressChanged <| String.toInt value
        , placeholder = Nothing
        , text = input
        }


{-| Attributes for input to use numeric keyboard on iOS.
-}
numericAttributes : List (Element.Attribute msg)
numericAttributes =
    [ Element.htmlAttribute <| Attributes.type_ "number"
    , Element.htmlAttribute <| Attributes.pattern "[0-9]*"
    , Element.htmlAttribute <| Attributes.attribute "inputmode" "numeric"
    ]
