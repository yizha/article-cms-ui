module Login
    exposing
        ( Model
        , Msg
        , view
        , update
        , init
        , subscriptions
        , authToken
        , logout
        )

import Global
import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Html.Events
import Dom
import Date
import Time
import Http
import Task
import Mouse
import Keyboard
import Json.Decode as Json
import Json.Decode.Extra as JsonExtra
import Common.Debug exposing (debug)
import Common.Util exposing (httpErrorString)
import MDC.Textfield as Textfield


-- MODEL


type alias AuthData =
    { token : String
    , expire : Time.Time
    , role : Int
    }


type AuthError
    = NoSuchUser Http.Error String
    | WrongPassword Http.Error String
    | OtherError Http.Error


authError : Http.Error -> String -> String -> AuthError
authError err username password =
    case err of
        Http.BadStatus resp ->
            case resp.status.code of
                403 ->
                    case resp.body of
                        "username" ->
                            NoSuchUser err username

                        "password" ->
                            WrongPassword err password

                        _ ->
                            OtherError err

                _ ->
                    OtherError err

        _ ->
            OtherError err


type State
    = Authorized AuthData
    | Unauthorized (Maybe AuthError)
    | InProgress


type alias Model =
    { loginPath : String
    , username : Textfield.Model Msg
    , password : Textfield.Model Msg
    , state : State
    , lastActivityTime : Maybe Time.Time
    , maxIdleTime : Time.Time
    }


authToken : Model -> Maybe String
authToken model =
    case model.state of
        Authorized data ->
            Just data.token

        _ ->
            Nothing


logout : Model -> ( Model, Cmd Msg )
logout model =
    init


init : ( Model, Cmd Msg )
init =
    let
        ( usernameModel, usernameCmd ) =
            Textfield.init
                Username
                [ Textfield.ID "login_username"
                , Textfield.Hint "Username"
                , Textfield.HelpAsValidationMsg True
                ]

        ( passwordModel, passwordCmd ) =
            Textfield.init
                Password
                [ Textfield.ID "login_password"
                , Textfield.Hint "Password"
                , Textfield.HelpAsValidationMsg True
                ]

        m =
            { loginPath = "/api/login"
            , username = usernameModel
            , password = passwordModel
            , state = Unauthorized Nothing
            , lastActivityTime = Nothing
            , maxIdleTime = (Time.minute * 30)
            }
    in
        ( m
        , Cmd.batch
            [ Task.attempt ignore (Dom.focus "login_username")
            , usernameCmd
            , passwordCmd
            ]
        )



-- ACTION, UPDATE


type Msg
    = Ignore
    | Username Textfield.Msg
    | Password Textfield.Msg
    | LoginRequest
    | LoginResponse (Result Http.Error AuthData)
    | UserActivity
    | LogUserActivity Time.Time
    | CheckIdleTimeout Time.Time


ignore : a -> Msg
ignore =
    always Ignore


checkIdleTimeout : Time.Time -> Model -> ( Model, Cmd Msg )
checkIdleTimeout now model =
    case model.lastActivityTime of
        Just t ->
            if now - t > model.maxIdleTime then
                init
            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


handleLoginErr : Http.Error -> Model -> ( Model, Cmd Msg, Global.Event )
handleLoginErr err model =
    let
        authErr =
            authError err model.username.value model.password.value

        newModel =
            { model | state = Unauthorized (Just authErr) }
    in
        case authErr of
            OtherError err ->
                ( newModel, Cmd.none, Global.None )

            NoSuchUser _ _ ->
                ( newModel, Task.attempt ignore (Dom.focus "login_username"), Global.None )

            WrongPassword _ _ ->
                ( newModel, Task.attempt ignore (Dom.focus "login_password"), Global.None )


update : Msg -> Model -> ( Model, Cmd Msg, Global.Event )
update msg model =
    case msg of
        Ignore ->
            ( model, Cmd.none, Global.None )

        Username tfm ->
            let
                ( m, c ) =
                    Textfield.update tfm model.username
            in
                ( { model | username = m }, c, Global.None )

        Password tfm ->
            let
                ( m, c ) =
                    Textfield.update tfm model.password
            in
                ( { model | password = m }, c, Global.None )

        LoginRequest ->
            if model.username.value /= "" && model.password.value /= "" then
                ( debug "login-request" { model | state = InProgress }
                , getAuthData model.loginPath model.username.value model.password.value
                , Global.None
                )
            else
                ( model, Cmd.none, Global.None )

        LoginResponse (Ok authdata) ->
            ( debug "login-ok" { model | state = Authorized authdata }
            , Task.perform LogUserActivity Time.now
            , Global.Login authdata.token
            )

        LoginResponse (Err err) ->
            handleLoginErr err model

        UserActivity ->
            ( model, Task.perform LogUserActivity Time.now, Global.None )

        LogUserActivity time ->
            ( { model | lastActivityTime = Just time }, Cmd.none, Global.None )

        CheckIdleTimeout time ->
            let
                ( m, c ) =
                    checkIdleTimeout time model
            in
                ( debug "login-check" m, c, Global.None )



-- VIEW


disableLoginBtn : Model -> Bool
disableLoginBtn model =
    model.username.value == "" || model.password.value == "" || model.state == InProgress


disableLoginInput : Model -> Bool
disableLoginInput model =
    model.state == InProgress


noSuchUserErr : Model -> String
noSuchUserErr model =
    case model.state of
        Unauthorized (Just (NoSuchUser _ username)) ->
            if model.username.value == username then
                "No such user!"
            else
                ""

        _ ->
            ""


wrongPasswordErr : Model -> String
wrongPasswordErr model =
    case model.state of
        Unauthorized (Just (WrongPassword _ password)) ->
            if model.password.value == password then
                "Wrong password!"
            else
                ""

        _ ->
            ""


view : Model -> Html Msg
view model =
    let
        onEnter =
            Html.Events.on "keyup"
                (Json.map
                    (\code ->
                        if code == 13 then
                            LoginRequest
                        else
                            Ignore
                    )
                    Html.Events.keyCode
                )
    in
        div
            [ style
                [ ( "padding", "8rem 2rem 2rem 2rem" )
                , ( "display", "flex" )
                , ( "align-items", "center" )
                , ( "justify-content", "center" )
                ]
            ]
            [ Textfield.view model.username
            , Textfield.view model.password
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Authorized _ ->
            Sub.batch
                [ Mouse.clicks (\_ -> UserActivity)
                , Keyboard.presses (\_ -> UserActivity)
                , Time.every Time.minute CheckIdleTimeout
                ]

        _ ->
            Sub.none



-- HTTP


getAuthData : String -> String -> String -> Cmd Msg
getAuthData path username password =
    let
        url =
            path ++ "?username=" ++ username ++ "&password=" ++ password
    in
        Http.send LoginResponse (Http.get url decodeAuthData)


decodeAuthData : Json.Decoder AuthData
decodeAuthData =
    Json.map3 AuthData
        (Json.field "token" Json.string)
        (Json.map Date.toTime (Json.field "expire" JsonExtra.date))
        (Json.field "role" Json.int)
