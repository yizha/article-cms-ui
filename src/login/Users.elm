module Users
    exposing
        ( Model
        , Msg
        , view
        , update
        , init
        , subscriptions
        , login
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


-- MODEL


type alias CmsRole =
    { name : String
    , value : Int
    }


type alias CmsUser =
    { username : String
    , password : String
    , roles : List String
    }


type DataState
    = Loading
    | RolesLoaded (Result Http.Error (List CmsRole))
    | UsersLoaded (Result Http.Error (List CmsUser))
    | AllLoaded ( Result Http.Error (List CmsRole), Result Http.Error (List CmsUser) )


type alias Model =
    { loginUsername : String
    , loginToken : String
    , rolesPath : String
    , usersPath : String
    , dataState : DataState
    , editUser : Maybe CmsUser
    }


login : String -> String -> Model -> ( Model, Cmd Msg )
login username token model =
    ( initModel username token
    , Cmd.batch
        [ getRoles model.rolesPath token
        , getUsers model.usersPath token
        ]
    )


initModel : String -> String -> Model
initModel username token =
    { loginUsername = username
    , loginToken = token
    , rolesPath = "/api/login/roles"
    , usersPath = "/api/login/users"
    , dataState = Loading
    , editUser = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel "" "", Cmd.none )



-- ACTION, UPDATE


type Msg
    = Logout
    | Roles (Result Http.Error (List CmsRole))
    | Users (Result Http.Error (List CmsUser))
    | Reload
    | EditUser CmsUser



{-
   handleHttpErr : String -> Http.Error -> Model -> ( Model, Cmd Msg, Global.Event )
   handleHttpErr path err model =
       ( { model | error = httpErrorString path err }, Cmd.none, Global.None )
-}


updateRoles : Result Http.Error (List CmsRole) -> Model -> Model
updateRoles roles model =
    case model.dataState of
        Loading ->
            { model | dataState = RolesLoaded roles }

        RolesLoaded _ ->
            { model | dataState = RolesLoaded roles }

        UsersLoaded users ->
            { model | dataState = AllLoaded ( roles, users ) }

        AllLoaded ( _, users ) ->
            { model | dataState = AllLoaded ( roles, users ) }


updateUsers : Result Http.Error (List CmsUser) -> Model -> Model
updateUsers users model =
    case model.dataState of
        Loading ->
            { model | dataState = UsersLoaded users }

        RolesLoaded roles ->
            { model | dataState = AllLoaded ( roles, users ) }

        UsersLoaded _ ->
            { model | dataState = UsersLoaded users }

        AllLoaded ( roles, _ ) ->
            { model | dataState = AllLoaded ( roles, users ) }


update : Msg -> Model -> ( Model, Cmd Msg, Global.Event )
update msg model =
    case msg of
        Logout ->
            ( initModel "" "", Cmd.none, Global.Logout )

        Roles roles ->
            ( updateRoles roles model, Cmd.none, Global.None )

        Users users ->
            ( updateUsers users model, Cmd.none, Global.None )

        Reload ->
            ( { model | dataState = Loading }
            , Cmd.batch
                [ getRoles model.rolesPath model.loginToken
                , getUsers model.usersPath model.loginToken
                ]
            , Global.None
            )

        EditUser user ->
            ( { model | editUser = Just user }, Cmd.none, Global.None )



-- VIEW


disableNew : Model -> Bool
disableNew model =
    case model.dataState of
        AllLoaded ( Ok _, Ok _ ) ->
            False

        _ ->
            True


disableReload : Model -> Bool
disableReload model =
    case model.dataState of
        AllLoaded _ ->
            False

        _ ->
            True


disableEdit : Model -> Bool
disableEdit =
    disableNew


disableLogout : Model -> Bool
disableLogout =
    disableReload


toolbar : Model -> Html Msg
toolbar model =
    header [ class "mdc-toolbar mdc-toolbar--fixed" ]
        [ div
            [ class "mdc-toolbar__row" ]
            [ section [ class "mdc-toolbar__section mdc-toolbar__section--align-start mdc-toolbar__section--shrink-to-fit" ]
                [ span [ class "mdc-toolbar__title" ]
                    [ text "User Management" ]
                ]
            , section [ class "mdc-toolbar__section" ]
                [ span [ style [ ( "color", "red" ) ] ] [ text "" ] ]
            , section [ class "mdc-toolbar__section mdc-toolbar__section--align-end mdc-toolbar__section--shrink-to-fit" ]
                [ button
                    [ class "mdc-button mdc-theme--text-primary-on-dark"
                    , disabled (disableLogout model)
                    , onClick Logout
                    ]
                    [ text "Logout" ]
                ]
            ]
        ]


userRoleSpan : String -> Html Msg
userRoleSpan role =
    span [] [ text role ]


userNameSpan : String -> String -> Html Msg
userNameSpan loginUsername username =
    if username == loginUsername then
        span []
            [ span [] [ text username ]
            , span [] [ text " " ]
            , span [ style [ ( "color", "blue" ) ] ] [ text "(You)" ]
            ]
    else
        span [] [ text username ]


userRow : Model -> CmsUser -> Html Msg
userRow model user =
    tr []
        [ td [] [ userNameSpan model.loginUsername user.username ]
        , td [] (List.map userRoleSpan user.roles)
        , td []
            [ button
                [ class "mdc-button mdc-button--dense mdc-button--compact mdc-button--primary"
                , disabled (disableEdit model)
                , onClick (EditUser user)
                ]
                [ text "Edit" ]
            ]
        ]


userTable : Model -> Html Msg
userTable model =
    let
        users =
            getUsersFromModel model
    in
        table []
            [ thead []
                [ tr []
                    [ td [] [ text "Username" ]
                    , td [] [ text "Roles" ]
                    , td []
                        [ button
                            [ class "mdc-button mdc-button--dense mdc-button--compact mdc-button--primary"
                            , disabled (disableNew model)
                            ]
                            [ text "New" ]
                        , button
                            [ class "mdc-button mdc-button--dense mdc-button--compact mdc-button--primary"
                            , disabled (disableReload model)
                            , onClick Reload
                            ]
                            [ text "Reload" ]
                        ]
                    ]
                ]
            , tbody [] (List.map (userRow model) users)
            ]


getUsersFromModel : Model -> List CmsUser
getUsersFromModel model =
    case model.dataState of
        AllLoaded ( Ok _, Ok users ) ->
            users

        _ ->
            []


view : Model -> Html Msg
view model =
    div
        []
        [ toolbar model
        , div
            [ class "mdc-toolbar-fixed-adjust"
            , style [ ( "padding", "1rem" ) ]
            ]
            [ userTable model
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Http


getValue : String -> String -> Json.Decoder a -> (Result Http.Error a -> Msg) -> Cmd Msg
getValue path token decoder msgGenerator =
    let
        req =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" token ]
                , url = path
                , body = Http.emptyBody
                , expect = Http.expectJson decoder
                , timeout = Just (Time.second * 5)
                , withCredentials = False
                }
    in
        Http.send msgGenerator req


getRoles : String -> String -> Cmd Msg
getRoles path token =
    getValue path token decodeRoles Roles


decodeRoles : Json.Decoder (List CmsRole)
decodeRoles =
    Json.list <|
        Json.map2
            CmsRole
            (Json.field "name" Json.string)
            (Json.field "value" Json.int)


getUsers : String -> String -> Cmd Msg
getUsers path token =
    getValue path token decodeUsers Users


decodeUsers : Json.Decoder (List CmsUser)
decodeUsers =
    Json.list <|
        Json.map2
            (\username role -> CmsUser username "" role)
            (Json.field "username" Json.string)
            (Json.field "role" (Json.list Json.string))
