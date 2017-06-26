module Users
    exposing
        ( Model
        , Msg
        , view
        , update
        , init
        , subscriptions
        , login
        , logout
        )

import Global
import Html exposing (..)
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
import Util.Debug exposing (debug)
import Material
import Material.Color as MColor
import Material.Layout as MLayout
import Material.Button as MButton
import Material.Options as MOptions exposing (css)


-- MODEL


type alias Model =
    { mdl : Material.Model
    }


login : Model -> ( Model, Cmd Msg )
login model =
    ( initModel, Cmd.none )


logout : Model -> ( Model, Cmd Msg )
logout model =
    ( model, Cmd.none )


initModel : Model
initModel =
    { mdl = Material.model
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- ACTION, UPDATE


type Msg
    = Logout
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg, Global.Event )
update msg model =
    case msg of
        Logout ->
            ( model, Cmd.none, Global.Logout )

        Mdl msg_ ->
            let
                ( msg, cmd ) =
                    Material.update Mdl msg_ model
            in
                ( msg, cmd, Global.None )



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ MLayout.render Mdl
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
                            [ --MButton.raised
                              MColor.text MColor.white

                            --MButton.colored
                            , MOptions.onClick Logout
                            ]
                            [ text "Logout" ]
                        ]
                    ]
                ]
            , drawer = []
            , tabs = ( [], [] )
            , main = []
            }
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
