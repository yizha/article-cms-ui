port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Date
import Time
import MDC.Textfield as Textfield


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { username : Textfield.Model }


init : ( Model, Cmd Msg )
init =
    let
        conf =
            Textfield.defaultConfig

        ( m, c ) =
            Textfield.init
                { conf
                    | hint = "Username"
                    , helpAsValidationMsg = True
                }
    in
        ( { username = m }, Cmd.map Username c )



-- UPDATE


type Msg
    = None
    | Username Textfield.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Username tfm ->
            let
                ( m, c ) =
                    Textfield.update tfm model.username
            in
                ( { model | username = m }, Cmd.map Username c )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.map Username (Textfield.view model.username) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
