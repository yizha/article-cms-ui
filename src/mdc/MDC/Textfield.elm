module MDC.Textfield exposing (Model, Msg, init, update, view, defaultConfig, InputType(..))

import Html exposing (Html, div, input, label, span, text, p)
import Html.Attributes exposing (attribute, class, classList, type_, id, value, required)
import Html.Events exposing (onFocus, onBlur, onInput)
import Uuid.Barebones as Uuid
import Random.Pcg as Random


type InputType
    = Text
    | Password


type alias Config a =
    { a
        | id : String
        , type_ : InputType
        , hint : String
        , help : String
        , helpPersistent : Bool
        , helpAsValidationMsg : Bool
        , required : Bool
        , validator : Maybe (String -> Bool)
    }


type alias Model =
    Config
        { focused : Bool
        , valid : Bool
        , init : Bool
        , value : String
        }


defaultConfig : Config {}
defaultConfig =
    { id = ""
    , type_ = Text
    , hint = ""
    , help = ""
    , helpPersistent = False
    , helpAsValidationMsg = False
    , required = False
    , validator = Nothing
    }


init : Config {} -> ( Model, Cmd Msg )
init conf =
    let
        m =
            { id = conf.id
            , type_ = conf.type_
            , hint =
                if conf.required && conf.hint == "" then
                    "REQUIRED"
                else
                    conf.hint
            , help = conf.help
            , helpPersistent = conf.helpPersistent
            , helpAsValidationMsg = conf.helpAsValidationMsg
            , required = conf.required
            , validator = conf.validator
            , focused = False
            , valid = True
            , init = True
            , value = ""
            }
    in
        if conf.id == "" then
            ( m, Random.generate SetId Uuid.uuidStringGenerator )
        else
            ( m, Cmd.none )



-- UPDATE


type Msg
    = SetId String
    | Input String
    | Focus
    | Blur


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetId id ->
            ( { model | id = id }, Cmd.none )

        Input value ->
            ( { model | value = value }, Cmd.none )

        Focus ->
            ( { model | focused = True, init = False }, Cmd.none )

        Blur ->
            case model.validator of
                Nothing ->
                    ( { model | focused = False, valid = True }, Cmd.none )

                Just f ->
                    ( { model | focused = False, valid = f model.value }, Cmd.none )



-- VIEW


invalid : Model -> Bool
invalid model =
    (not model.init)
        && ((model.required && model.value == "") || not model.valid)


helpAttrs : Model -> Bool -> Bool -> List (Html.Attribute Msg)
helpAttrs model persistent validation =
    let
        classes =
            classList
                [ ( "mdc-textfield-helptext", True )
                , ( "mdc-textfield-helptext--persistent", persistent )
                , ( "mdc-textfield-helptext--validation-msg", validation )
                ]
    in
        if (invalid model) && validation then
            [ classes, attribute "role" "alert" ]
        else
            [ classes ]


helptext : Model -> Maybe (Html Msg)
helptext model =
    if model.help == "" then
        Nothing
    else
        Just (p (helpAttrs model model.helpPersistent model.helpAsValidationMsg) [ text model.help ])


inputType : InputType -> String
inputType t =
    case t of
        Text ->
            "text"

        Password ->
            "password"


nativeInput : Model -> Html Msg
nativeInput model =
    input
        [ type_ (inputType model.type_)
        , id model.id
        , class "mdc-textfield__input"
        , value model.value
        , required model.required
        , onFocus Focus
        , onInput Input
        , onBlur Blur
        ]
        []


hintLabel : Model -> Html Msg
hintLabel model =
    label
        [ classList
            [ ( "mdc-textfield__label", True )
            , ( "mdc-textfield__label--float-above", model.focused || model.value /= "" )
            ]
        , attribute "for" model.id
        ]
        [ text model.hint ]


textfield : Model -> Html Msg
textfield model =
    div
        [ classList
            [ ( "mdc-textfield", True )
            , ( "mdc-textfield--upgraded", model.hint /= "" )
            , ( "mdc-textfield--focused", model.focused )
            , ( "mdc-textfield--invalid", (not model.focused) && (invalid model) )
            ]
        ]
        [ nativeInput model, hintLabel model ]


view : Model -> Html Msg
view model =
    case helptext model of
        Nothing ->
            div [] [ textfield model ]

        Just elem ->
            div [] [ textfield model, elem ]
