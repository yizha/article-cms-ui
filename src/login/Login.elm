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
    , username : String
    , password : String
    , state : State
    , lastActivityTime : Maybe Time.Time
    , maxIdleTime : Time.Time
    , snackbar : MSnackbar.Model Int
    , mdl : Material.Model
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
    ( initModel, Task.attempt ignore (Dom.focus "login_username") )


initModel : Model
initModel =
    { loginPath = "/api/login"
    , username = ""
    , password = ""
    , state = Unauthorized Nothing
    , lastActivityTime = Nothing
    , maxIdleTime = (Time.minute * 30)
    , snackbar = MSnackbar.model
    , mdl = Material.model
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Task.attempt ignore (Dom.focus "login_username") )



-- ACTION, UPDATE


type Msg
    = Ignore
    | Username String
    | Password String
    | LoginRequest
    | LoginResponse (Result Http.Error AuthData)
    | UserActivity
    | LogUserActivity Time.Time
    | CheckIdleTimeout Time.Time
    | Snackbar (MSnackbar.Msg Int)
    | Mdl (Material.Msg Msg)


ignore : a -> Msg
ignore =
    always Ignore


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


handleLoginErr : Http.Error -> Model -> ( Model, Cmd Msg, Global.Event )
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
                        { message = httpErrorString model.loginPath err
                        , action = Nothing
                        , payload = 0
                        , timeout = 5 * Time.second
                        , fade = 250 * Time.millisecond
                        }

                    ( sbModel, sbCmd ) =
                        MSnackbar.add contents model.snackbar
                in
                    ( { newModel | snackbar = sbModel }, Cmd.map Snackbar sbCmd, Global.None )

            NoSuchUser _ _ ->
                ( newModel, Task.attempt ignore (Dom.focus "login_username"), Global.None )

            WrongPassword _ _ ->
                ( newModel, Task.attempt ignore (Dom.focus "login_password"), Global.None )


update : Msg -> Model -> ( Model, Cmd Msg, Global.Event )
update msg model =
    case msg of
        Ignore ->
            ( model, Cmd.none, Global.None )

        Username username ->
            ( { model | username = username }, Cmd.none, Global.None )

        Password password ->
            ( { model | password = password }, Cmd.none, Global.None )

        LoginRequest ->
            ( debug "login-request" { model | state = InProgress }
            , getAuthData model.loginPath model.username model.password
            , Global.None
            )

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
            ( debug "login-check" (checkIdleTimeout time model), Cmd.none, Global.None )

        Snackbar msg_ ->
            let
                ( msg, cmd ) =
                    MSnackbar.update msg_ model.snackbar
                        |> map1st (\s -> { model | snackbar = s })
                        |> map2nd (Cmd.map Snackbar)
            in
                ( msg, cmd, Global.None )

        Mdl msg_ ->
            let
                ( msg, cmd ) =
                    Material.update Mdl msg_ model
            in
                ( msg, cmd, Global.None )



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
                                [ MOptions.id "login_username"
                                , MTextfield.label "Username"
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
                                [ MOptions.id "login_password"
                                , MTextfield.label "Password"
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
