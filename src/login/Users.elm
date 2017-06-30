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
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (href, class, style)
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
import Material
import Material.Color as MColor
import Material.Layout as MLayout
import Material.Button as MButton
import Material.Table as MTable
import Material.Snackbar as MSnackbar
import Material.Options as MOptions exposing (css)
import Material.Chip as MChip
import Material.Helpers exposing (map1st, map2nd)


-- MODEL


type alias CmsRole =
    { name : String
    , value : Int
    }


type alias CmsUser =
    { username : String
    , password : String
    , role : List String
    }


type alias Model =
    { rolesPath : String
    , roles : List CmsRole
    , usersPath : String
    , users : List CmsUser
    , snackbar : MSnackbar.Model Int
    , mdl : Material.Model
    }


login : String -> Model -> ( Model, Cmd Msg )
login token model =
    ( initModel
    , Cmd.batch
        [ getRoles model.rolesPath token
        , getUsers model.usersPath token
        ]
    )


initModel : Model
initModel =
    { rolesPath = "/api/login/roles"
    , roles = []
    , usersPath = "/api/login/users"
    , users = []
    , snackbar = MSnackbar.model
    , mdl = Material.model
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- ACTION, UPDATE


type Msg
    = Logout
    | Roles (Result Http.Error (List CmsRole))
    | Users (Result Http.Error (List CmsUser))
    | Snackbar (MSnackbar.Msg Int)
    | Mdl (Material.Msg Msg)


handleHttpErr : String -> Http.Error -> Model -> ( Model, Cmd Msg, Global.Event )
handleHttpErr path err model =
    let
        contents =
            { message = httpErrorString path err
            , action = Nothing
            , payload = 0
            , timeout = 5 * Time.second
            , fade = 250 * Time.millisecond
            }

        ( sbModel, sbCmd ) =
            MSnackbar.add contents model.snackbar
    in
        ( { model | snackbar = sbModel }, Cmd.map Snackbar sbCmd, Global.None )


update : Msg -> Model -> ( Model, Cmd Msg, Global.Event )
update msg model =
    case msg of
        Logout ->
            ( initModel, Cmd.none, Global.Logout )

        Roles (Ok roles) ->
            ( { model | roles = roles }, Cmd.none, Global.None )

        Roles (Err err) ->
            handleHttpErr model.rolesPath err model

        Users (Ok users) ->
            ( { model | users = users }, Cmd.none, Global.None )

        Users (Err err) ->
            handleHttpErr model.usersPath err model

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


userRole : String -> Html Msg
userRole role =
    MChip.span [] [ MChip.content [] [ text role ] ]


usersTable : List CmsUser -> Html Msg
usersTable users =
    MTable.table []
        [ MTable.thead []
            [ MTable.tr []
                [ MTable.th [] [ text "Username" ]
                , MTable.th [] [ text "Role" ]
                , MTable.th [] []
                ]
            ]
        , MTable.tbody []
            (users
                |> List.map
                    (\u ->
                        MTable.tr []
                            [ MTable.td [] [ text u.username ]
                            , MTable.td [] (List.map userRole u.role)
                            , MTable.td [] []
                            ]
                    )
            )
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ MSnackbar.view model.snackbar |> Html.map Snackbar
        , MLayout.render Mdl
            model.mdl
            [ MLayout.fixedHeader ]
            { header =
                [ MLayout.row
                    []
                    [ MLayout.title [] [ text "Users" ]
                    , MLayout.spacer
                    , MLayout.navigation []
                        [ MButton.render Mdl
                            [ 0 ]
                            model.mdl
                            [ MColor.text MColor.white
                            , MOptions.onClick Logout
                            ]
                            [ text "Create User" ]
                        , MButton.render Mdl
                            [ 1 ]
                            model.mdl
                            [ MColor.text MColor.white
                            , MOptions.onClick Logout
                            ]
                            [ text "Logout" ]
                        ]
                    ]
                ]
            , drawer = []
            , tabs = ( [], [] )
            , main = [ usersTable model.users ]
            }
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
