module Login exposing (Model, Msg, update, init, view, subscriptions)

import Html exposing (Html, div, input, p, text, button)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (disabled, placeholder, value, class, classList)
import Html.Events.Extra exposing (onEnter)
import Http
import Json.Decode as Json
import Json.Decode.Extra as JsonExtra
import String
import Date
import Time
import Task
import Mouse
import Keyboard
import Util.Debug exposing (debug)


-- MODEL


type alias AuthData =
    { token : String
    , expire : Time.Time
    , role : Int
    }


type State
    = Unauthorized (Maybe Http.Error)
    | LoginInProgress
    | Authorized AuthData


type alias Model =
    { username : String
    , password : String
    , state : State
    , lastUserActivityTime : Maybe Time.Time
    , maxIdleTime : Time.Time
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    , state = Unauthorized Nothing
    , lastUserActivityTime = Nothing
    , maxIdleTime = (Time.minute * 30)
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = Username String
    | Password String
    | LoginRequest
    | LoginResponse (Result Http.Error AuthData)
    | Logout
    | UserActivity
    | LogUserActivity Time.Time
    | CheckIdleTimeout Time.Time


getAuthError : Http.Error -> String
getAuthError err =
    case err of
        Http.BadUrl desc ->
            "BadUrl: " ++ desc

        Http.Timeout ->
            "Timeout!"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus resp ->
            "BadStatus! Code: " ++ (toString resp.status.code) ++ ", Body: " ++ resp.body

        Http.BadPayload desc resp ->
            "BadPayload: " ++ desc


checkIdleTimeout : Time.Time -> Model -> Model
checkIdleTimeout now model =
    case model.lastUserActivityTime of
        Just t ->
            if now - t > model.maxIdleTime then
                initModel
            else
                model

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username username ->
            ( { model | username = username }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        LoginRequest ->
            if disableLoginBtn model then
                ( model, Cmd.none )
            else
                ( debug "login-request" { model | state = LoginInProgress }
                , doLogin model.username model.password
                )

        LoginResponse (Ok authdata) ->
            ( debug "login-ok" { model | state = Authorized authdata }
            , Task.perform LogUserActivity Time.now
            )

        LoginResponse (Err err) ->
            ( debug "login-ng" { model | state = Unauthorized (Just err) }, Cmd.none )

        Logout ->
            ( debug "login-logout" initModel, Cmd.none )

        UserActivity ->
            ( model, Task.perform LogUserActivity Time.now )

        LogUserActivity time ->
            ( { model | lastUserActivityTime = Just time }, Cmd.none )

        CheckIdleTimeout time ->
            ( debug "login-check" (checkIdleTimeout time model), Cmd.none )



-- VIEW


disableLoginBtn : Model -> Bool
disableLoginBtn model =
    if model.username == "" || model.password == "" then
        True
    else
        case model.state of
            Unauthorized _ ->
                False

            _ ->
                True


disableLogoutBtn : Model -> Bool
disableLogoutBtn model =
    case model.state of
        Authorized _ ->
            False

        _ ->
            True


disableLoginInput : Model -> Bool
disableLoginInput model =
    case model.state of
        Unauthorized _ ->
            False

        _ ->
            True


logoutDiv : Model -> Html Msg
logoutDiv model =
    div [ class "level-right" ]
        [ div [ class "level-item" ]
            [ p [ class "subtitle is-5" ]
                [ text model.username ]
            ]
        , div [ class "level-item" ]
            [ p [ class "field" ]
                [ p [ class "control" ]
                    [ button
                        [ class "button is-warning"
                        , disabled (disableLogoutBtn model)
                        , onClick Logout
                        ]
                        [ text "Logout" ]
                    ]
                ]
            ]
        ]


fieldErr : Model -> String -> Bool
fieldErr model fieldName =
    case model.state of
        Unauthorized err ->
            case err of
                Just (Http.BadStatus resp) ->
                    if resp.status.code == 403 && (String.toLower resp.body) == fieldName then
                        True
                    else
                        False

                _ ->
                    False

        _ ->
            False


loginDiv : Model -> Html Msg
loginDiv model =
    div [ class "level-right" ]
        [ div [ class "level-item" ]
            [ p [ class "field is-grouped" ]
                [ p [ class "control" ]
                    [ input
                        [ classList
                            [ ( "input", True )
                            , ( "is-danger", fieldErr model "username" )
                            ]
                        , onInput Username
                        , placeholder "Username"
                        , onEnter LoginRequest
                        , disabled (disableLoginInput model)
                        , value model.username
                        ]
                        []
                    ]
                , p [ class "control" ]
                    [ input
                        [ classList
                            [ ( "input", True )
                            , ( "is-danger", fieldErr model "password" )
                            ]
                        , onInput Password
                        , placeholder "Password"
                        , onEnter LoginRequest
                        , disabled (disableLoginInput model)
                        , value model.password
                        ]
                        []
                    ]
                , p [ class "control" ]
                    [ button
                        [ class "button"
                        , disabled (disableLoginBtn model)
                        , onClick LoginRequest
                        ]
                        [ text "Login" ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model.state of
        Authorized _ ->
            logoutDiv model

        _ ->
            loginDiv model



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


doLogin : String -> String -> Cmd Msg
doLogin username password =
    let
        url =
            "/api/login?username=" ++ username ++ "&password=" ++ password
    in
        Http.send LoginResponse (Http.get url decodeAuthData)


decodeAuthData : Json.Decoder AuthData
decodeAuthData =
    Json.map3 AuthData
        (Json.field "token" Json.string)
        (Json.map Date.toTime (Json.field "expire" JsonExtra.date))
        (Json.field "role" Json.int)
