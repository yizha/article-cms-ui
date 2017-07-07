module MDC.Textfield exposing (Model, Msg, init, update, updateModel, view, InputType(..), Config(..))

import Html exposing (Html, div, input, label, span, text, p)
import Html.Attributes exposing (attribute, class, classList, type_, id, value, required)
import Html.Events
import Uuid.Barebones as Uuid
import Random.Pcg as Random
import Json.Decode as Json


type InputType
    = Text
    | Password


type Config msg
    = ID String
    | Type InputType
    | Valid Bool
    | Required Bool
    | Hint String
    | Help String
    | HelpPersistent Bool
    | HelpAsValidationMsg Bool
    | Value String
    | OnInput (Maybe (String -> msg))
    | OnEnter (Maybe msg)


type alias Model msg =
    { id : String
    , type_ : InputType
    , valid : Bool
    , required : Bool
    , hint : String
    , help : String
    , helpPersistent : Bool
    , helpAsValidationMsg : Bool
    , focused : Bool
    , init : Bool
    , msgWrapper : Msg msg -> msg
    , value : String
    , onInput : Maybe (String -> msg)
    , onEnter : Maybe msg
    }


configModel : Config msg -> Model msg -> Model msg
configModel conf m =
    case conf of
        ID v ->
            { m | id = v }

        Type v ->
            { m | type_ = v }

        Valid v ->
            { m | valid = v }

        Required v ->
            { m | required = v }

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

        OnInput v ->
            { m | onInput = v }

        OnEnter v ->
            { m | onEnter = v }


init : (Msg msg -> msg) -> List (Config msg) -> ( Model msg, Cmd msg )
init f confs =
    let
        defaultModel =
            { id = ""
            , type_ = Text
            , required = False
            , valid = True
            , hint = ""
            , help = ""
            , helpPersistent = False
            , helpAsValidationMsg = False
            , focused = False
            , init = True
            , msgWrapper = f
            , value = ""
            , onInput = Nothing
            , onEnter = Nothing
            }

        m =
            List.foldl configModel defaultModel confs
    in
        if m.id == "" then
            ( m, Cmd.map f <| Random.generate SetId Uuid.uuidStringGenerator )
        else
            ( m, Cmd.none )



-- UPDATE


type Msg msg
    = Noop
    | SetId String
    | Input String
    | Focus
    | Blur


updateModel : Model msg -> List (Config msg) -> Model msg
updateModel model confs =
    List.foldl configModel model confs


update : Msg msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        SetId id ->
            ( { model | id = id }, Cmd.none )

        Input v ->
            ( { model | value = v }, Cmd.none )

        Focus ->
            ( { model | focused = True, init = False }, Cmd.none )

        Blur ->
            ( { model | focused = False }, Cmd.none )



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


helptext : Model msg -> Maybe (Html msg)
helptext model =
    if model.help == "" then
        Nothing
    else
        Just (p (helpAttrs model) [ text model.help ])


inputType : InputType -> String
inputType t =
    case t of
        Text ->
            "text"

        Password ->
            "password"


onEnter : Model msg -> msg -> Html.Attribute msg
onEnter model msg =
    Html.Events.on
        "keyup"
        (Json.map
            (\code ->
                if code == 13 then
                    msg
                else
                    model.msgWrapper Noop
            )
            Html.Events.keyCode
        )


nativeInput : Model msg -> Html msg
nativeInput model =
    let
        attrs =
            [ type_ (inputType model.type_)
            , id model.id
            , class "mdc-textfield__input"
            , required model.required
            , Html.Events.onFocus (model.msgWrapper Focus)
            , Html.Events.onBlur (model.msgWrapper Blur)
            , Html.Events.onInput
                (case model.onInput of
                    Nothing ->
                        (model.msgWrapper << Input)

                    Just f ->
                        f
                )
            ]

        attrs1 =
            case model.onEnter of
                Nothing ->
                    attrs

                Just f ->
                    (onEnter model f) :: attrs
    in
        input attrs1 []


hintLabel : Model msg -> Html msg
hintLabel model =
    label
        [ classList
            [ ( "mdc-textfield__label", True )
            , ( "mdc-textfield__label--float-above", model.focused || model.value /= "" )
            ]
        , attribute "for" model.id
        ]
        [ text model.hint ]


textfield : Model msg -> Html msg
textfield model =
    div
        [ classList
            [ ( "mdc-textfield", True )
            , ( "mdc-textfield--upgraded", model.hint /= "" )
            , ( "mdc-textfield--focused", model.focused )
            , ( "mdc-textfield--invalid", (not model.init) && (not model.valid) )
            ]
        ]
        [ nativeInput model, hintLabel model ]


view : Model msg -> Html msg
view model =
    case helptext model of
        Nothing ->
            div [] [ textfield model ]

        Just elem ->
            div [] [ textfield model, elem ]
