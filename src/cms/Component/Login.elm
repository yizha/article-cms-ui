module Component.Login
    exposing
        ( view
        , update
        , init
        , subscriptions
        )

import Defs exposing (CmsRole, AuthData, AuthError(..), AuthState(..), Model, LoginModel, Msg(Login), LoginMsg(..))
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
import Common.Debug exposing (debug)
import Common.Util exposing (httpErrorString)
import MDC.Textfield as Textfield


-- MODEL


init : String -> String -> Time.Time -> ( LoginModel, Cmd Msg )
init loginPath title maxIdleTime =
    let
        ( m, c ) =
            init_ loginPath title maxIdleTime
    in
        ( m, Cmd.map Login c )


init_ : String -> String -> Time.Time -> ( LoginModel, Cmd LoginMsg )
init_ loginPath title maxIdleTime =
    let
        usernameModel =
            Textfield.init
                LoginUsername
                [ Textfield.ID "login_username"
                , Textfield.Hint "Username"
                , Textfield.HelpAsValidationMsg True
                , Textfield.OnInput (Just LoginUsernameInput)
                , Textfield.OnEnter (Just (LoginRequest (Just "login_username")))
                ]

        passwordModel =
            Textfield.init
                LoginPassword
                [ Textfield.Type Textfield.Password
                , Textfield.ID "login_password"
                , Textfield.Hint "Password"
                , Textfield.HelpAsValidationMsg True
                , Textfield.OnInput (Just LoginPasswordInput)
                , Textfield.OnEnter (Just (LoginRequest (Just "login_password")))
                ]

        m =
            { loginPath = loginPath
            , title = title
            , maxIdleTime = maxIdleTime
            , lastActivityTime = Nothing
            , username = usernameModel
            , password = passwordModel
            , auth = Unauthorized
            }
    in
        m ! [ Task.attempt ignore (Dom.focus "login_username") ]



-- ACTION, UPDATE


ignore : a -> LoginMsg
ignore =
    always LoginNoop


checkIdleTimeout : Time.Time -> LoginModel -> ( LoginModel, Cmd LoginMsg )
checkIdleTimeout now model =
    case model.lastActivityTime of
        Just t ->
            if now - t > model.maxIdleTime then
                init_ model.loginPath model.title model.maxIdleTime
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


handleLoginErr : Http.Error -> LoginModel -> ( LoginModel, Cmd LoginMsg )
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


handleUsernameInput : LoginModel -> String -> Textfield.Model LoginMsg
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


handlePasswordInput : LoginModel -> String -> Textfield.Model LoginMsg
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


handleLoginRequest : LoginModel -> Maybe String -> ( LoginModel, Cmd LoginMsg )
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


update : LoginMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( m, c ) =
            update_ msg model.login
    in
        ( { model | login = m }, Cmd.map Login c )


update_ : LoginMsg -> LoginModel -> ( LoginModel, Cmd LoginMsg )
update_ msg model =
    case msg of
        LoginNoop ->
            model ! []

        LoginUsername tfm ->
            { model | username = Textfield.update tfm model.username } ! []

        LoginPassword tfm ->
            { model | password = Textfield.update tfm model.password } ! []

        LoginUsernameInput v ->
            { model | username = handleUsernameInput model v } ! []

        LoginPasswordInput v ->
            { model | password = handlePasswordInput model v } ! []

        LoginRequest source ->
            handleLoginRequest model source

        LoginResponse (Ok authdata) ->
            debug "login-ok" { model | auth = AuthSuccess authdata } ! []

        LoginResponse (Err err) ->
            handleLoginErr err model

        LoginUserActivity ->
            model ! [ Task.perform LoginLogUserActivity Time.now ]

        LoginLogUserActivity time ->
            { model | lastActivityTime = Just time } ! []

        LoginCheckIdleTimeout time ->
            checkIdleTimeout time model

        LoginLogout ->
            init_ model.loginPath model.title model.maxIdleTime



-- VIEW


disableLoginBtn : LoginModel -> Bool
disableLoginBtn model =
    model.username.value == "" || model.password.value == "" || model.auth == AuthInProgress


noSuchUserErr : LoginModel -> String
noSuchUserErr model =
    case model.auth of
        AuthFailure (NoSuchUser username) ->
            if model.username.value == username then
                "No such user!"
            else
                ""

        _ ->
            ""


wrongPasswordErr : LoginModel -> String
wrongPasswordErr model =
    case model.auth of
        AuthFailure (WrongPassword password) ->
            if model.password.value == password then
                "Wrong password!"
            else
                ""

        _ ->
            ""


otherErr : LoginModel -> String
otherErr model =
    case model.auth of
        AuthFailure (OtherError err) ->
            httpErrorString model.loginPath err

        _ ->
            ""


view : Model -> Html Msg
view model =
    Html.map Login (view_ model)


view_ : Model -> Html LoginMsg
view_ model =
    case model.login.auth of
        AuthSuccess data ->
            authorizedView model.lockUI model.login

        _ ->
            unauthorizedView model.login


authorizedView : Bool -> LoginModel -> Html LoginMsg
authorizedView lockUI model =
    div []
        [ header [ class "mdc-toolbar mdc-toolbar--fixed" ]
            [ div
                [ class "mdc-toolbar__row" ]
                [ section [ class "mdc-toolbar__section mdc-toolbar__section--align-start" ]
                    [ span [ class "mdc-toolbar__title" ]
                        [ text model.title ]
                    ]
                , section [ class "mdc-toolbar__section mdc-toolbar__section--align-end" ]
                    [ button
                        [ class "mdc-button mdc-theme--text-primary-on-dark"
                        , disabled lockUI
                        , onClick LoginLogout
                        ]
                        [ text ("Logout (" ++ model.username.value ++ ")") ]
                    ]
                ]
            ]
        , div [ class "mdc-toolbar-fixed-adjust" ] []
        ]


unauthorizedView : LoginModel -> Html LoginMsg
unauthorizedView model =
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
    Sub.map Login (subscriptions_ model.login)


subscriptions_ : LoginModel -> Sub LoginMsg
subscriptions_ model =
    case model.auth of
        AuthSuccess _ ->
            Sub.batch
                [ Mouse.clicks (\_ -> LoginUserActivity)
                , Keyboard.presses (\_ -> LoginUserActivity)
                , Time.every Time.minute LoginCheckIdleTimeout
                ]

        _ ->
            Sub.none



-- HTTP


getAuthData : String -> String -> String -> Cmd LoginMsg
getAuthData path username password =
    let
        url =
            path ++ "?username=" ++ username ++ "&password=" ++ password
    in
        Http.send LoginResponse (Http.get url Defs.decodeAuthData)
