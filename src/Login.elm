module Login exposing (Model, Msg, update, init, view, subscriptions)

import Html exposing (Html, div, input, p, text, button)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (disabled, placeholder, value, class, classList)
import Html.Events.Extra exposing (onEnter)
import Http
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
import String
import Date
import Time
import Task
import Mouse
import Keyboard
import Util.Debug exposing (debug)


-- MODEL


type State
    = Unauthorized
    | InProgress
    | Authorized


type alias AuthData =
    { token : String
    , expire : Time.Time
    , role : Int
    }


type alias Model =
    { username : String
    , password : String
    , authdata : AuthData
    , error : String
    , state : State
    , lastUserActivityTime : Time.Time
    , maxIdleTime : Time.Time
    }


initAuthData : AuthData
initAuthData =
    AuthData "" 0 0


initModel : Model
initModel =
    { username = ""
    , password = ""
    , authdata = initAuthData
    , error = ""
    , state = Unauthorized
    , lastUserActivityTime = 0
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


getLoginError : Http.Error -> String
getLoginError err =
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
    if now - model.lastUserActivityTime > model.maxIdleTime then
        initModel
    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username username ->
            ( { model | username = username }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        LoginRequest ->
            if model.username /= "" && model.password /= "" && model.state == Unauthorized then
                ( debug "login" { model | state = InProgress }
                , getAuthData model.username model.password
                )
            else
                ( model, Cmd.none )

        LoginResponse (Ok authdata) ->
            ( debug "login" { model | authdata = authdata, state = Authorized, error = "" }
            , Task.perform LogUserActivity Time.now
            )

        LoginResponse (Err err) ->
            ( debug "login" { model | state = Unauthorized, error = (getLoginError err) }, Cmd.none )

        Logout ->
            ( debug "login" initModel, Cmd.none )

        UserActivity ->
            ( model, Task.perform LogUserActivity Time.now )

        LogUserActivity time ->
            ( { model | lastUserActivityTime = time }, Cmd.none )

        CheckIdleTimeout time ->
            ( debug "login" (checkIdleTimeout time model), Cmd.none )



-- VIEW


disableLoginBtn : Model -> Bool
disableLoginBtn model =
    (model.username == "") || (model.password == "") || (model.state /= Unauthorized)


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
                        , disabled (model.state /= Authorized)
                        , onClick Logout
                        ]
                        [ text "Logout" ]
                    ]
                ]
            ]
        ]


fieldErr : Model -> String -> Bool
fieldErr model fieldName =
    model.error /= "" && (String.contains fieldName (String.toLower model.error))


loginDiv : Model -> Html Msg
loginDiv model =
    div [ class "level-right" ]
        [ div [ class "level-item" ]
            [ p [ class "field is-grouped" ]
                [ p [ class "control" ]
                    [ input
                        [ classList
                            [ ( "input", True )
                            , ( "is-danger", fieldErr model "user" )
                            ]
                        , onInput Username
                        , placeholder "Username"
                        , onEnter LoginRequest
                        , disabled (model.state /= Unauthorized)
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
                        , disabled (model.state /= Unauthorized)
                        , value model.password
                        ]
                        []
                    ]
                , p [ class "control" ]
                    [ button
                        [ class "button is-primary"
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
    if model.state == Authorized then
        logoutDiv model
    else
        loginDiv model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state == Authorized then
        Sub.batch
            [ Mouse.clicks (\_ -> UserActivity)
            , Keyboard.presses (\_ -> UserActivity)
            , Time.every Time.minute CheckIdleTimeout
            ]
    else
        Sub.none



-- HTTP


getAuthData : String -> String -> Cmd Msg
getAuthData username password =
    let
        url =
            "/api/login?username=" ++ username ++ "&password=" ++ password
    in
        Http.send LoginResponse (Http.get url decodeAuthData)


decodeAuthData : Decode.Decoder AuthData
decodeAuthData =
    Decode.map3 AuthData
        (Decode.field "token" Decode.string)
        (Decode.map Date.toTime (Decode.field "expire" DecodeExtra.date))
        (Decode.field "role" Decode.int)
