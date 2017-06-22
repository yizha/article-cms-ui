module Main exposing (..)

import Html exposing (Html, div, nav)
import Html.Attributes exposing (class)
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
    div []
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "bulma.css" ] []
        , nav [ class "level" ]
            [ div [ class "level-left" ]
                []
            , Html.map Login (Login.view model.login)
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            [ Sub.map Login (Login.subscriptions model.login) ]
    in
        Sub.batch subs
