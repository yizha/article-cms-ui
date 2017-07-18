module Main exposing (main)

import Defs exposing (Model, Msg(..))
import Html exposing (Html, div, text)
import Page.Login as LoginPage
import RemoteData
import Time


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        ( loginModel, loginCmd ) =
            LoginPage.init "/api/login" (Time.minute * 30)
    in
        { login = loginModel } ! [ Cmd.map Login loginCmd ]



-- UPDATE


handleLoginPageUpdate : LoginPage.Msg -> Model -> ( Model, Cmd Msg )
handleLoginPageUpdate msg model =
    let
        ( m, c ) =
            LoginPage.update msg model.login
    in
        { model | login = m } ! [ Cmd.map Login c ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Defs.Noop ->
            model ! []

        Defs.Login loginMsg ->
            handleLoginPageUpdate loginMsg model



-- VIEW


view : Model -> Html Msg
view model =
    case model.login.auth of
        LoginPage.AuthSuccess _ ->
            div [] [ text "logged in" ]

        _ ->
            Html.map Login (LoginPage.view model.login)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        loginSubs =
            LoginPage.subscriptions model.login
    in
        Sub.batch [ Sub.map Defs.Login loginSubs ]
