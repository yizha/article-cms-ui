module Main exposing (main)

import Html exposing (Html, div, nav, p, a, text, strong)
import Html.Attributes exposing (class, style)
import Http
import Date
import Time
import Login


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { login : Login.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( login_m, login_cmd ) =
            Login.init

        cmds =
            [ (Cmd.map Login login_cmd) ]
    in
        ( { login = login_m
          }
        , Cmd.batch cmds
        )



-- UPDATE


type Msg
    = Login Login.Msg


handleLogin : Login.Msg -> Model -> ( Model, Cmd Msg )
handleLogin login_msg model =
    let
        ( m, cmd ) =
            Login.update login_msg model.login
    in
        ( { model | login = m }, Cmd.map Login cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login login_msg ->
            handleLogin login_msg model



-- VIEW


view : Model -> Html Msg
view model =
    if Login.authorized model.login then
        div [] [ text "logged in" ]
    else
        Html.map Login (Login.view model.login)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Login (Login.subscriptions model.login) ]
