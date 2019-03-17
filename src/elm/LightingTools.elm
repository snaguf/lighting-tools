module LightingTools exposing (Model, Msg, init, subscriptions, update, view)

import DipSwitchCalc exposing (dipSwitchCalc, validAddress)
import Element exposing (Element)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes



-- MODEL


type alias Model =
    { title : String
    , address : Int
    }


initialModel : Model
initialModel =
    { title = "Lighting Tools"
    , address = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = AddressChanged Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddressChanged value ->
            if validAddress value then
                ( { model | address = value }, Cmd.none )

            else
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ Element.centerX ]
            [ dipSwitchCalc AddressChanged model.address
            ]
