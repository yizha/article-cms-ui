module Page.Login
    exposing
        ( AuthData
        , AuthState(..)
        , Model
        , Msg
        , authToken
        , view
        , update
        , init
        , subscriptions
        )

import RemoteData
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


type alias CmsRole =
    Int


type alias AuthData =
    { token : String
    , expire : Time.Time
    , role : CmsRole
    }


type AuthError
    = NoSuchUser String
    | WrongPassword String
    | OtherError Http.Error


type AuthState
    = Unauthorized
    | AuthInProgress
    | AuthSuccess AuthData
    | AuthFailure AuthError


type alias Model =
    { loginPath : String
    , maxIdleTime : Time.Time
    , lastActivityTime : Maybe Time.Time
    , username : Textfield.Model Msg
    , password : Textfield.Model Msg
    , auth : AuthState
    }


authToken : Model -> Maybe String
authToken model =
    case model.auth of
        AuthSuccess data ->
            Just data.token

        _ ->
            Nothing


init : String -> Time.Time -> ( Model, Cmd Msg )
init loginPath maxIdleTime =
    let
        usernameModel =
            Textfield.init
                Username
                [ Textfield.ID "login_username"
                , Textfield.Hint "Username"
                , Textfield.HelpAsValidationMsg True
                , Textfield.OnInput (Just UsernameInput)
                , Textfield.OnEnter (Just (LoginRequest (Just "login_username")))
                ]

        passwordModel =
            Textfield.init
                Password
                [ Textfield.Type Textfield.Password
                , Textfield.ID "login_password"
                , Textfield.Hint "Password"
                , Textfield.HelpAsValidationMsg True
                , Textfield.OnInput (Just PasswordInput)
                , Textfield.OnEnter (Just (LoginRequest (Just "login_password")))
                ]

        m =
            { loginPath = loginPath
            , maxIdleTime = maxIdleTime
            , lastActivityTime = Nothing
            , username = usernameModel
            , password = passwordModel
            , auth = Unauthorized
            }
    in
        m ! [ Task.attempt ignore (Dom.focus "login_username") ]



-- ACTION, UPDATE


type Msg
    = Username (Textfield.Msg Msg)
    | Password (Textfield.Msg Msg)
    | UsernameInput String
    | PasswordInput String
    | LoginRequest (Maybe String)
    | LoginResponse (Result Http.Error AuthData)
    | UserActivity
    | LogUserActivity Time.Time
    | CheckIdleTimeout Time.Time
    | Noop


ignore : a -> Msg
ignore =
    always Noop


checkIdleTimeout : Time.Time -> Model -> ( Model, Cmd Msg )
checkIdleTimeout now model =
    case model.lastActivityTime of
        Just t ->
            if now - t > model.maxIdleTime then
                init model.loginPath model.maxIdleTime
            else
                model ! []

        Nothing ->
            model ! []


authError : Http.Error -> String -> String -> AuthError
authError err username password =
    case err of
        Http.BadStatus resp ->
            case resp.status.code of
                403 ->
                    case resp.body of
                        "username" ->
                            NoSuchUser username

                        "password" ->
                            WrongPassword password

                        _ ->
                            OtherError err

                _ ->
                    OtherError err

        _ ->
            OtherError err


handleLoginErr : Http.Error -> Model -> ( Model, Cmd Msg )
handleLoginErr err model =
    let
        authErr =
            authError err model.username.value model.password.value

        m1 =
            { model | auth = AuthFailure authErr }

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
                m2 ! []

            NoSuchUser _ ->
                let
                    um =
                        Textfield.updateModel
                            model.username
                            [ Textfield.Help "No such user!"
                            , Textfield.Valid False
                            ]

                    cmds =
                        [ Task.attempt ignore (Dom.focus "login_username") ]
                in
                    { m2 | username = um } ! cmds

            WrongPassword _ ->
                let
                    pm =
                        Textfield.updateModel
                            model.password
                            [ Textfield.Help "Wrong password!"
                            , Textfield.Valid False
                            ]

                    cmds =
                        [ Task.attempt ignore (Dom.focus "login_password") ]
                in
                    { m2 | password = pm } ! cmds


handleUsernameInput : Model -> String -> Textfield.Model Msg
handleUsernameInput model username =
    case model.auth of
        AuthFailure (NoSuchUser wrongUsername) ->
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
    case model.auth of
        AuthFailure (WrongPassword wrongPassword) ->
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


handleLoginRequest : Model -> Maybe String -> ( Model, Cmd Msg )
handleLoginRequest model source =
    if model.username.value /= "" && model.password.value /= "" then
        let
            cmds =
                case source of
                    Nothing ->
                        [ getAuthData model.loginPath model.username.value model.password.value ]

                    Just id ->
                        [ Task.attempt ignore (Dom.blur id)
                        , getAuthData model.loginPath model.username.value model.password.value
                        ]
        in
            (debug "login-request" { model | auth = AuthInProgress }) ! cmds
    else
        model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        Username tfm ->
            { model | username = Textfield.update tfm model.username } ! []

        Password tfm ->
            { model | password = Textfield.update tfm model.password } ! []

        UsernameInput v ->
            { model | username = handleUsernameInput model v } ! []

        PasswordInput v ->
            { model | password = handlePasswordInput model v } ! []

        LoginRequest source ->
            handleLoginRequest model source

        LoginResponse (Ok authdata) ->
            debug "login-ok" { model | auth = AuthSuccess authdata } ! []

        LoginResponse (Err err) ->
            handleLoginErr err model

        UserActivity ->
            model ! [ Task.perform LogUserActivity Time.now ]

        LogUserActivity time ->
            { model | lastActivityTime = Just time } ! []

        CheckIdleTimeout time ->
            checkIdleTimeout time model



-- VIEW


disableLoginBtn : Model -> Bool
disableLoginBtn model =
    model.username.value == "" || model.password.value == "" || model.auth == AuthInProgress


noSuchUserErr : Model -> String
noSuchUserErr model =
    case model.auth of
        AuthFailure (NoSuchUser username) ->
            if model.username.value == username then
                "No such user!"
            else
                ""

        _ ->
            ""


wrongPasswordErr : Model -> String
wrongPasswordErr model =
    case model.auth of
        AuthFailure (WrongPassword password) ->
            if model.password.value == password then
                "Wrong password!"
            else
                ""

        _ ->
            ""


otherErr : Model -> String
otherErr model =
    case model.auth of
        AuthFailure (OtherError err) ->
            httpErrorString model.loginPath err

        _ ->
            ""


view : Model -> Html Msg
view model =
    let
        um =
            (Textfield.updateModel
                model.username
                [ Textfield.Disabled (model.auth == AuthInProgress) ]
            )

        pm =
            (Textfield.updateModel
                model.password
                [ Textfield.Disabled (model.auth == AuthInProgress) ]
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
                                , onClick (LoginRequest Nothing)
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
    case model.auth of
        AuthSuccess _ ->
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
