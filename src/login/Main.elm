module Main exposing (main)

import Html exposing (Html, div, nav, p, a, text, strong)
import Html.Attributes exposing (class, style)
import Http
import Date
import Time
import Global
import Login
import Users
import Common.Debug exposing (debug)


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
    , users : Users.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( login_m, login_cmd ) =
            Login.init

        ( users_m, users_cmd ) =
            Users.init

        cmds =
            [ (Cmd.map Login login_cmd)
            , (Cmd.map Users users_cmd)
            ]
    in
        ( { login = login_m
          , users = users_m
          }
        , Cmd.batch cmds
        )



-- UPDATE


type Msg
    = Login Login.Msg
    | Users Users.Msg


handleGlobalEvent : Model -> Global.Event -> ( Model, List (Cmd Msg) )
handleGlobalEvent model e =
    case e of
        Global.None ->
            ( model, [] )

        Global.Login token ->
            let
                ( usersM, usersCmd ) =
                    Users.login token model.users
            in
                ( { model | users = usersM }, [ Cmd.map Users usersCmd ] )

        Global.Logout ->
            let
                ( loginM, loginCmd ) =
                    Login.logout model.login
            in
                ( { model | login = loginM }, [ Cmd.map Login loginCmd ] )


handleLogin : Login.Msg -> Model -> ( Model, Cmd Msg )
handleLogin login_msg model =
    let
        ( m, cmd, e ) =
            Login.update login_msg model.login

        ( newModel, cmds ) =
            handleGlobalEvent { model | login = m } e
    in
        ( newModel, Cmd.batch ((Cmd.map Login cmd) :: cmds) )


handleUsers : Users.Msg -> Model -> ( Model, Cmd Msg )
handleUsers users_msg model =
    let
        ( m, cmd, e ) =
            Users.update users_msg model.users

        ( newModel, cmds ) =
            handleGlobalEvent { model | users = m } e
    in
        ( newModel, Cmd.batch ((Cmd.map Users cmd) :: cmds) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login login_msg ->
            handleLogin login_msg model

        Users users_msg ->
            handleUsers users_msg model



-- VIEW


view : Model -> Html Msg
view model =
    case Login.authToken model.login of
        Just _ ->
            Html.map Users (Users.view model.users)

        Nothing ->
            Html.map Login (Login.view model.login)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Login (Login.subscriptions model.login)
        , Sub.map Users (Users.subscriptions model.users)
        ]
