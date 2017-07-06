module MDC.Textfield exposing (Model, Msg, init, update, view, InputType(..), Config(..))

import Html exposing (Html, div, input, label, span, text, p)
import Html.Attributes exposing (attribute, class, classList, type_, id, value, required)
import Html.Events exposing (onFocus, onBlur, onInput)
import Uuid.Barebones as Uuid
import Random.Pcg as Random


type InputType
    = Text
    | Password


type Config
    = ID String
    | Type InputType
    | Valid Bool
    | Required Bool
    | Hint String
    | Help String
    | HelpPersistent Bool
    | HelpAsValidationMsg Bool
    | Value String


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
    , msgWrapper : Msg -> msg
    , value : String
    }


configModel : Config -> Model msg -> Model msg
configModel conf m =
    case conf of
        ID id ->
            { m | id = id }

        Type t ->
            { m | type_ = t }

        Valid b ->
            { m | valid = b }

        Required b ->
            { m | required = b }

        Hint hint ->
            { m | hint = hint }

        Help help ->
            { m | help = help }

        HelpPersistent b ->
            { m | helpPersistent = b }

        HelpAsValidationMsg b ->
            { m | helpAsValidationMsg = b }

        Value v ->
            { m | value = v }


init : (Msg -> msg) -> List Config -> ( Model msg, Cmd msg )
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
            }

        m =
            List.foldl configModel defaultModel confs
    in
        if m.id == "" then
            ( m, Cmd.map f <| Random.generate SetId Uuid.uuidStringGenerator )
        else
            ( m, Cmd.none )



-- UPDATE


type Msg
    = UpdateModel (List Config)
    | SetId String
    | Input String
    | Focus
    | Blur


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        UpdateModel confs ->
            ( List.foldl configModel model confs, Cmd.none )

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


nativeInput : Model msg -> Html msg
nativeInput model =
    input
        [ type_ (inputType model.type_)
        , id model.id
        , class "mdc-textfield__input"
        , required model.required
        , onFocus (model.msgWrapper Focus)
        , onInput (model.msgWrapper << Input)
        , onBlur (model.msgWrapper Blur)
        ]
        []


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
