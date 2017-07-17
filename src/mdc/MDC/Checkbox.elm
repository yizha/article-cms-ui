module MDC.Checkbox exposing (Model, Msg, init, update, updateModel, view, Config(..))

import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (attribute, class, classList, type_, id, disabled, for, checked)
import Html.Events exposing (onCheck)
import Svg exposing (svg, path)
import Svg.Attributes exposing (viewBox, fill, stroke, d)
import Json.Decode as Json


type Config msg
    = ID String
    | Label String
    | Disabled Bool
    | Checked Bool
    | OnCheck (Maybe (Bool -> msg))


type alias Model msg =
    { id : String
    , label : String
    , disabled : Bool
    , checked : Bool
    , onCheck : Maybe (Bool -> msg)
    , msgWrapper : Msg msg -> msg
    }


configModel : Config msg -> Model msg -> Model msg
configModel conf m =
    case conf of
        ID v ->
            { m | id = v }

        Label v ->
            { m | label = v }

        Disabled v ->
            { m | disabled = v }

        Checked v ->
            { m | checked = v }

        OnCheck v ->
            { m | onCheck = v }


init : (Msg msg -> msg) -> List (Config msg) -> ( Model msg, Cmd msg )
init f confs =
    let
        defaultModel =
            { id = ""
            , label = ""
            , disabled = False
            , checked = False
            , onCheck = Nothing
            , msgWrapper = f
            }
    in
        ( List.foldl configModel defaultModel confs, Cmd.none )



-- UPDATE


type Msg msg
    = Noop
    | Check Bool


updateModel : Model msg -> List (Config msg) -> Model msg
updateModel model confs =
    List.foldl configModel model confs


update : Msg msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Check v ->
            ( { model | checked = v }, Cmd.none )



-- VIEW


checkbox : Model msg -> Html msg
checkbox model =
    div
        [ classList
            [ ( "mdc-checkbox", True )
            , ( "mdc-checkbox--disabled", model.disabled )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , class "mdc-checkbox__native-control"
            , id model.id
            , checked model.checked
            , disabled model.disabled
            , onCheck
                (case model.onCheck of
                    Nothing ->
                        (model.msgWrapper << Check)

                    Just f ->
                        f
                )
            ]
            []
        , div [ class "mdc-checkbox__background" ]
            [ svg
                [ Svg.Attributes.class "mdc-checkbox__checkmark"
                , viewBox "0 0 24 24"
                ]
                [ path
                    [ Svg.Attributes.class "mdc-checkbox__checkmark__path"
                    , fill "none"
                    , stroke "white"
                    , d "M1.73,12.91 8.1,19.28 22.79,4.59"
                    ]
                    []
                ]
            , div [ class "mdc-checkbox__mixedmark" ] []
            ]
        ]


view : Model msg -> Html msg
view model =
    if model.label == "" then
        checkbox model
    else
        div [ class "mdc-form-field" ]
            [ checkbox model
            , label [ for model.id ] [ text model.label ]
            ]
