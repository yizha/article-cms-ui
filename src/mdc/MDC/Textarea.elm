module MDC.Textarea exposing (Model, Msg, init, update, updateModel, view, Config(..))

import Html exposing (Html, div, textarea, label, span, text, p)
import Html.Attributes exposing (attribute, class, classList, id, value, required, disabled, value, rows, cols)
import Html.Events
import Json.Decode as Json


type Config msg
    = ID String
    | Valid Bool
    | Required Bool
    | Disabled Bool
    | Hint String
    | Help String
    | HelpPersistent Bool
    | HelpAsValidationMsg Bool
    | Value String
    | Rows Int
    | Cols Int


type alias Model msg =
    { id : String
    , valid : Bool
    , required : Bool
    , disabled : Bool
    , rows : Int
    , cols : Int
    , hint : String
    , help : String
    , helpPersistent : Bool
    , helpAsValidationMsg : Bool
    , focused : Bool
    , init : Bool
    , msgWrapper : Msg msg -> msg
    , value : String
    }


configModel : Config msg -> Model msg -> Model msg
configModel conf m =
    case conf of
        ID v ->
            { m | id = v }

        Valid v ->
            { m | valid = v }

        Required v ->
            { m | required = v }

        Disabled v ->
            { m | disabled = v }

        Hint v ->
            { m | hint = v }

        Help v ->
            { m | help = v }

        HelpPersistent v ->
            { m | helpPersistent = v }

        HelpAsValidationMsg v ->
            { m | helpAsValidationMsg = v }

        Value v ->
            { m | value = v }

        Rows v ->
            { m | rows = v }

        Cols v ->
            { m | cols = v }


init : (Msg msg -> msg) -> List (Config msg) -> Model msg
init f confs =
    let
        defaultModel =
            { id = ""
            , required = False
            , disabled = False
            , valid = True
            , hint = ""
            , help = ""
            , helpPersistent = False
            , helpAsValidationMsg = False
            , focused = False
            , init = True
            , msgWrapper = f
            , value = ""
            , rows = 8
            , cols = 40
            }
    in
        List.foldl configModel defaultModel confs



-- UPDATE


type Msg msg
    = Noop
    | Input String
    | Focus
    | Blur


updateModel : Model msg -> List (Config msg) -> Model msg
updateModel model confs =
    List.foldl configModel model confs


update : Msg msg -> Model msg -> Model msg
update msg model =
    case msg of
        Noop ->
            model

        Input v ->
            { model | value = v }

        Focus ->
            { model | focused = True, init = False }

        Blur ->
            { model | focused = False }



-- VIEW


helpAttrs : Model msg -> List (Html.Attribute msg)
helpAttrs model =
    let
        classes =
            classList
                [ ( "mdc-textfield-helptext", True )
                , ( "mdc-textfield-helptext--persistent", model.helpPersistent )
                , ( "mdc-textfield-helptext--validation-msg", model.helpAsValidationMsg )
                ]
    in
        if model.helpAsValidationMsg && not model.valid then
            [ classes, attribute "role" "alert" ]
        else
            [ classes ]


helptext : Model msg -> Html msg
helptext model =
    p (helpAttrs model) [ text model.help ]


nativeInput : Model msg -> Html msg
nativeInput model =
    let
        attrs =
            [ id model.id
            , class "mdc-textfield__input"
            , required model.required
            , value model.value
            , rows model.rows
            , cols model.cols
            , Html.Events.onFocus (model.msgWrapper Focus)
            , Html.Events.onBlur (model.msgWrapper Blur)
            , Html.Events.onInput (model.msgWrapper << Input)
            ]
    in
        textarea attrs []


hintLabel : Model msg -> Html msg
hintLabel model =
    label
        [ classList
            [ ( "mdc-textfield__label", True )
            , ( "mdc-textfield__label--float-above", model.focused || model.value /= "" )
            ]
        , disabled model.disabled
        , attribute "for" model.id
        ]
        [ text model.hint ]


inputarea : Model msg -> Html msg
inputarea model =
    div
        [ classList
            [ ( "mdc-textfield", True )
            , ( "mdc-textfield--multiline", True )
            , ( "mdc-textfield--upgraded", model.hint /= "" )
            , ( "mdc-textfield--focused", model.focused )
            , ( "mdc-textfield--invalid", (not model.init) && (not model.valid) )
            , ( "mdc-textfield--disabled", model.disabled )
            ]
        ]
        [ nativeInput model, hintLabel model ]


view : Model msg -> Html msg
view model =
    div [] [ inputarea model, helptext model ]
