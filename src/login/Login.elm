module Login
    exposing
        ( Model
        , Msg
        , view
        , update
        , init
        , subscriptions
        , authorized
        )

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Html.Events
import Date
import Time
import Http
import Task
import Mouse
import Keyboard
import Json.Decode as Json
import Json.Decode.Extra as JsonExtra
import Util.Debug exposing (debug)
import Material
import Material.Button as MButton
import Material.Textfield as MTextfield
import Material.List as MList
import Material.Card as MCard
import Material.Color as MColor
import Material.Elevation as MElevation
import Material.Snackbar as MSnackbar
import Material.Options as MOptions exposing (css)
import Material.Helpers exposing (map1st, map2nd)


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


httpErrorString : Http.Error -> String
httpErrorString err =
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
    { username : String
    , password : String
    , state : State
    , lastActivityTime : Maybe Time.Time
    , maxIdleTime : Time.Time
    , snackbar : MSnackbar.Model Int
    , mdl : Material.Model
    }


authorized : Model -> Bool
authorized model =
    case model.state of
        Authorized _ ->
            True

        _ ->
            False


initModel : Model
initModel =
    { username = ""
    , password = ""
    , state = Unauthorized Nothing
    , lastActivityTime = Nothing
    , maxIdleTime = (Time.minute * 30)
    , snackbar = MSnackbar.model
    , mdl = Material.model
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- ACTION, UPDATE


type Msg
    = Username String
    | Password String
    | LoginRequest
    | LoginResponse (Result Http.Error AuthData)
    | Logout
    | UserActivity
    | LogUserActivity Time.Time
    | CheckIdleTimeout Time.Time
    | Snackbar (MSnackbar.Msg Int)
    | Mdl (Material.Msg Msg)


checkIdleTimeout : Time.Time -> Model -> Model
checkIdleTimeout now model =
    case model.lastActivityTime of
        Just t ->
            if now - t > model.maxIdleTime then
                initModel
            else
                model

        Nothing ->
            model


handleLoginErr : Http.Error -> Model -> ( Model, Cmd Msg )
handleLoginErr err model =
    let
        authErr =
            authError err model.username model.password

        newModel =
            { model | state = Unauthorized (Just authErr) }
    in
        case authErr of
            OtherError err ->
                let
                    contents =
                        { message = httpErrorString err
                        , action = Nothing
                        , payload = 0
                        , timeout = 5 * Time.second
                        , fade = 250 * Time.millisecond
                        }

                    ( sbModel, sbCmd ) =
                        MSnackbar.add contents model.snackbar
                in
                    ( { newModel | snackbar = sbModel }, Cmd.map Snackbar sbCmd )

            _ ->
                ( newModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username username ->
            { model | username = username } ! [ Cmd.none ]

        Password password ->
            { model | password = password } ! [ Cmd.none ]

        LoginRequest ->
            debug "login-request" { model | state = InProgress } ! [ login model.username model.password ]

        LoginResponse (Ok authdata) ->
            debug "login-ok" { model | state = Authorized authdata } ! [ Task.perform LogUserActivity Time.now ]

        LoginResponse (Err err) ->
            handleLoginErr err model

        Logout ->
            ( debug "login-logout" initModel, Cmd.none )

        UserActivity ->
            ( model, Task.perform LogUserActivity Time.now )

        LogUserActivity time ->
            ( { model | lastActivityTime = Just time }, Cmd.none )

        CheckIdleTimeout time ->
            ( debug "login-check" (checkIdleTimeout time model), Cmd.none )

        Snackbar msg_ ->
            MSnackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


disableLoginBtn : Model -> Bool
disableLoginBtn model =
    model.username == "" || model.password == "" || model.state == InProgress


disableLoginInput : Model -> Bool
disableLoginInput model =
    model.state == InProgress


noSuchUserErr : Model -> MTextfield.Property m
noSuchUserErr model =
    let
        b =
            case model.state of
                Unauthorized (Just (NoSuchUser _ username)) ->
                    model.username == username

                _ ->
                    False
    in
        MOptions.when b (MTextfield.error "No such user!")


wrongPasswordErr : Model -> MTextfield.Property m
wrongPasswordErr model =
    let
        b =
            case model.state of
                Unauthorized (Just (WrongPassword _ password)) ->
                    model.password == password

                _ ->
                    False
    in
        MOptions.when b (MTextfield.error "Wrong password!")


shouldLogin : Model -> Int -> Json.Decoder Msg
shouldLogin model code =
    if model.username /= "" && model.password /= "" && code == 13 then
        Json.succeed LoginRequest
    else
        Json.fail ""


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "padding", "8rem 2rem 2rem 2rem" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]
        ]
        [ MSnackbar.view model.snackbar |> Html.map Snackbar
        , MCard.view
            [ MElevation.e2 ]
            [ MCard.title [] [ MCard.head [] [ text "Manage Users" ] ]
            , MCard.actions
                []
                [ MList.ul []
                    [ MList.li []
                        [ MList.content []
                            [ MTextfield.render Mdl
                                [ 0 ]
                                model.mdl
                                [ MTextfield.label "Username"
                                , MTextfield.floatingLabel
                                , MTextfield.text_
                                , MTextfield.value model.username
                                , MOptions.onInput Username
                                , MOptions.on "keyup" (Json.andThen (shouldLogin model) Html.Events.keyCode)
                                , noSuchUserErr model
                                , MOptions.when (disableLoginInput model) MTextfield.disabled
                                ]
                                []
                            ]
                        ]
                    , MList.li []
                        [ MList.content []
                            [ MTextfield.render Mdl
                                [ 1 ]
                                model.mdl
                                [ MTextfield.label "Password"
                                , MTextfield.floatingLabel
                                , MTextfield.password
                                , MTextfield.value model.password
                                , MOptions.onInput Password
                                , MOptions.on "keyup" (Json.andThen (shouldLogin model) Html.Events.keyCode)
                                , wrongPasswordErr model
                                , MOptions.when (disableLoginInput model) MTextfield.disabled
                                ]
                                []
                            ]
                        ]
                    , MList.li []
                        [ MList.content
                            [ css "display" "flex"
                            , css "align-items" "center"
                            , css "justify-content" "flex-end"
                            ]
                            [ MButton.render Mdl
                                [ 2 ]
                                model.mdl
                                [ MButton.raised
                                , MButton.colored
                                , MButton.ripple
                                , MOptions.when (disableLoginBtn model) MButton.disabled
                                , MOptions.onClick LoginRequest
                                ]
                                [ text "LOGIN" ]
                            ]
                        ]
                    ]
                ]
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


login : String -> String -> Cmd Msg
login username password =
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
