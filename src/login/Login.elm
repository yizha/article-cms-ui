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
import Html.Attributes exposing (href, class, style, disabled)
import Html.Events exposing (onClick)
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
                , Textfield.OnInput (Just UsernameInput)
                , Textfield.OnEnter (Just LoginRequest)
                ]

        ( passwordModel, passwordCmd ) =
            Textfield.init
                Password
                [ Textfield.Type Textfield.Password
                , Textfield.ID "login_password"
                , Textfield.Hint "Password"
                , Textfield.HelpAsValidationMsg True
                , Textfield.OnInput (Just PasswordInput)
                , Textfield.OnEnter (Just LoginRequest)
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
    | Username (Textfield.Msg Msg)
    | Password (Textfield.Msg Msg)
    | UsernameInput String
    | PasswordInput String
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

        m1 =
            { model | state = Unauthorized (Just authErr) }

        um =
            Textfield.updateModel
                model.username
                [ Textfield.Help ""
                , Textfield.Valid True
                ]

        pm =
            Textfield.updateModel
                model.password
                [ Textfield.Help ""
                , Textfield.Valid True
                ]

        m2 =
            { m1 | username = um, password = pm }
    in
        case authErr of
            OtherError err ->
                ( m2, Cmd.none, Global.None )

            NoSuchUser _ _ ->
                let
                    um =
                        Textfield.updateModel
                            model.username
                            [ Textfield.Help "No such user!"
                            , Textfield.Valid False
                            ]
                in
                    ( { m2 | username = um }
                    , Task.attempt ignore (Dom.focus "login_username")
                    , Global.None
                    )

            WrongPassword _ _ ->
                let
                    pm =
                        Textfield.updateModel
                            model.password
                            [ Textfield.Help "Wrong password!"
                            , Textfield.Valid False
                            ]
                in
                    ( { m2 | password = pm }, Task.attempt ignore (Dom.focus "login_password"), Global.None )


handleUsernameInput : Model -> String -> Textfield.Model Msg
handleUsernameInput model username =
    case model.state of
        Unauthorized (Just (NoSuchUser _ wrongUsername)) ->
            if username == wrongUsername then
                Textfield.updateModel
                    model.username
                    [ Textfield.Value username
                    , Textfield.Valid False
                    , Textfield.Help "No such user!"
                    ]
            else
                Textfield.updateModel
                    model.username
                    [ Textfield.Value username
                    , Textfield.Valid True
                    , Textfield.Help ""
                    ]

        _ ->
            Textfield.updateModel model.username [ Textfield.Value username ]


handlePasswordInput : Model -> String -> Textfield.Model Msg
handlePasswordInput model password =
    case model.state of
        Unauthorized (Just (WrongPassword _ wrongPassword)) ->
            if password == wrongPassword then
                Textfield.updateModel
                    model.password
                    [ Textfield.Value password
                    , Textfield.Valid False
                    , Textfield.Help "Wrong password!"
                    ]
            else
                Textfield.updateModel
                    model.password
                    [ Textfield.Value password
                    , Textfield.Valid True
                    , Textfield.Help ""
                    ]

        _ ->
            (Textfield.updateModel model.password [ Textfield.Value password ])


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

        UsernameInput v ->
            let
                um =
                    handleUsernameInput model v
            in
                ( { model | username = um }, Cmd.none, Global.None )

        PasswordInput v ->
            let
                pm =
                    handlePasswordInput model v
            in
                ( { model | password = pm }, Cmd.none, Global.None )

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


otherErr : Model -> String
otherErr model =
    case model.state of
        Unauthorized (Just (OtherError err)) ->
            httpErrorString model.loginPath err

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

        um =
            (Textfield.updateModel
                model.username
                [ Textfield.Disabled (model.state == InProgress) ]
            )

        pm =
            (Textfield.updateModel
                model.password
                [ Textfield.Disabled (model.state == InProgress) ]
            )
    in
        ul [ style [ ( "list-style-type", "none" ) ] ]
            [ li []
                [ div
                    [ style
                        [ ( "padding", "8rem 2rem 2rem 2rem" )
                        , ( "display", "flex" )
                        , ( "align-items", "center" )
                        , ( "justify-content", "center" )
                        ]
                    ]
                    [ ul [ class "mdc-list" ]
                        [ li [ class "mdc-list-item" ] [ Textfield.view um ]
                        , li [ class "mdc-list-item" ] []
                        , li [ class "mdc-list-item" ] [ Textfield.view pm ]
                        , li [ class "mdc-list-item" ] []
                        , li [ class "mdc-list-item" ]
                            [ button
                                [ class "mdc-button mdc-button--raised"
                                , disabled (disableLoginBtn model)
                                , onClick LoginRequest
                                ]
                                [ text "Login" ]
                            ]
                        ]
                    ]
                ]
            , li []
                [ p [ style [ ( "color", "red" ) ] ]
                    [ text (otherErr model) ]
                ]
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
