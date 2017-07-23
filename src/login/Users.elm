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
import Html.Keyed
import Html.Attributes exposing (href, class, style, disabled, type_)
import Html.Events exposing (onClick)
import Dom
import Date
import Time
import Http
import Task
import Mouse
import Keyboard
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Decode.Extra as JsonExtra
import Common.Debug exposing (debug)
import Common.Util exposing (httpErrorString, httpGetJson)
import MDC.Textfield as Textfield
import MDC.Checkbox as Checkbox


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


type UserOpState
    = Editing (Maybe ( String, Http.Error ))
    | Submitting
    | Deleting (Maybe ( String, Http.Error ))


type alias Model =
    { loginUsername : String
    , loginToken : String
    , roles : Maybe ( String, Result Http.Error (List CmsRole) )
    , users : Maybe ( String, Result Http.Error (List CmsUser) )
    , userOpState : Maybe UserOpState
    , userOpUsername : Textfield.Model Msg
    , userOpPassword : Textfield.Model Msg
    , userOpRoles : Dict String (Checkbox.Model Msg)
    }


login : String -> String -> Model -> ( Model, Cmd Msg )
login username token model =
    ( model, Task.perform (always (Reload username token)) (Task.succeed True) )


reload : String -> String -> ( Model, Cmd Msg )
reload username token =
    ( initModel username token
    , Cmd.batch
        [ getRoles "/api/login/roles" token
        , getUsers "/api/login/users" token
        ]
    )


initModel : String -> String -> Model
initModel username token =
    let
        um =
            Textfield.init
                UserOpUsername
                [ Textfield.ID "user_op_username"
                , Textfield.Hint "Username"
                ]

        pm =
            Textfield.init
                UserOpPassword
                [ Textfield.ID "user_op_password"
                , Textfield.Hint "Password"
                ]
    in
        { loginUsername = username
        , loginToken = token
        , roles = Nothing
        , users = Nothing
        , userOpState = Nothing
        , userOpUsername = um
        , userOpPassword = pm
        , userOpRoles = Dict.empty
        }


init : ( Model, Cmd Msg )
init =
    ( initModel "" "", Cmd.none )



-- ACTION, UPDATE


type Msg
    = Logout
    | Roles String (Result Http.Error (List CmsRole))
    | Users String (Result Http.Error (List CmsUser))
    | Reload String String
    | UserOpNew
    | UserOpEdit CmsUser
    | UserOpDelete CmsUser
    | UserOpDeleteResult String (Result Http.Error String)
    | UserOpCancel
    | UserOpSubmit Bool
    | UserOpSubmitResult String (Result Http.Error String)
    | UserOpUsername (Textfield.Msg Msg)
    | UserOpPassword (Textfield.Msg Msg)
    | UserOpRole String (Checkbox.Msg Msg)
    | Noop


roleCheckboxModel : CmsRole -> Checkbox.Model Msg
roleCheckboxModel role =
    Checkbox.init
        (UserOpRole role.name)
        [ Checkbox.Label role.name
        , Checkbox.ID ("user_op_ckb_" ++ role.name)
        ]


initRoleCheckboxModels : List CmsRole -> Dict String (Checkbox.Model Msg)
initRoleCheckboxModels roles =
    List.foldl
        (\role d ->
            let
                m =
                    roleCheckboxModel role
            in
                Dict.insert role.name m d
        )
        Dict.empty
        roles


getCmsUserFromModel : Model -> CmsUser
getCmsUserFromModel model =
    { username = model.userOpUsername.value
    , password = model.userOpPassword.value
    , roles =
        Dict.foldl
            (\k v acc ->
                if v.checked then
                    k :: acc
                else
                    acc
            )
            []
            model.userOpRoles
    }


handleUserOpNew : Model -> ( Model, Cmd Msg, Global.Event )
handleUserOpNew model =
    let
        roles =
            case model.roles of
                Just ( _, Ok roles ) ->
                    roles

                _ ->
                    []

        um =
            Textfield.updateModel
                model.userOpUsername
                [ Textfield.Disabled False
                , Textfield.Required True
                , Textfield.Value ""
                ]

        pm =
            Textfield.updateModel
                model.userOpPassword
                [ Textfield.Disabled False
                , Textfield.Required True
                , Textfield.Value ""
                ]
    in
        ( { model
            | userOpState = Just (Editing Nothing)
            , userOpUsername = um
            , userOpPassword = pm
            , userOpRoles = Dict.map (\k v -> Checkbox.updateModel v [ Checkbox.Checked False ]) model.userOpRoles
          }
        , Task.attempt (always Noop) (Dom.focus um.id)
        , Global.None
        )


handleUserOpEdit : Model -> CmsUser -> ( Model, Cmd Msg, Global.Event )
handleUserOpEdit model user =
    let
        roles =
            case model.roles of
                Just ( _, Ok roles ) ->
                    roles

                _ ->
                    []

        um =
            Textfield.updateModel
                model.userOpUsername
                [ Textfield.Disabled True
                , Textfield.Required True
                , Textfield.Value user.username
                ]

        pm =
            Textfield.updateModel
                model.userOpPassword
                [ Textfield.Disabled False
                , Textfield.Required False
                , Textfield.Value ""
                ]

        isVoidUser =
            user.username == "void"
    in
        ( { model
            | userOpState = Just (Editing Nothing)
            , userOpUsername = um
            , userOpPassword = pm
            , userOpRoles =
                Dict.map
                    (\k v ->
                        Checkbox.updateModel v
                            [ Checkbox.Checked (List.member k user.roles)
                            , Checkbox.Disabled isVoidUser
                            ]
                    )
                    model.userOpRoles
          }
        , Task.attempt (always Noop) (Dom.focus pm.id)
        , Global.None
        )


handleUserOpSubmit : Model -> Bool -> ( Model, Cmd Msg, Global.Event )
handleUserOpSubmit model edit =
    let
        user =
            getCmsUserFromModel model

        cmd =
            if edit then
                updateUser model.loginToken user
            else
                createUser model.loginToken user
    in
        ( { model | userOpState = Just Submitting }, cmd, Global.None )


handleUserOpDelete : Model -> CmsUser -> ( Model, Cmd Msg, Global.Event )
handleUserOpDelete model user =
    ( { model | userOpState = Just (Deleting Nothing) }
    , deleteUser model.loginToken user
    , Global.None
    )


update : Msg -> Model -> ( Model, Cmd Msg, Global.Event )
update msg model =
    case msg of
        Logout ->
            ( initModel "" "", Cmd.none, Global.Logout )

        Roles path result ->
            let
                d =
                    case result of
                        Err _ ->
                            Dict.empty

                        Ok roles ->
                            initRoleCheckboxModels roles
            in
                ( { model | roles = Just ( path, result ), userOpRoles = d }, Cmd.none, Global.None )

        Users path result ->
            ( { model | users = Just ( path, result ) }, Cmd.none, Global.None )

        Reload username token ->
            let
                ( m, c ) =
                    reload username token
            in
                ( m, c, Global.None )

        UserOpNew ->
            handleUserOpNew model

        UserOpEdit user ->
            handleUserOpEdit model user

        UserOpDelete user ->
            handleUserOpDelete model user

        UserOpDeleteResult path (Ok _) ->
            let
                ( m, c ) =
                    reload model.loginUsername model.loginToken
            in
                ( m, c, Global.None )

        UserOpDeleteResult path (Err err) ->
            ( { model | userOpState = Just (Deleting (Just ( path, err ))) }, Cmd.none, Global.None )

        UserOpCancel ->
            ( { model | userOpState = Nothing }, Cmd.none, Global.None )

        UserOpSubmit edit ->
            handleUserOpSubmit model edit

        UserOpSubmitResult path (Ok _) ->
            let
                ( m, c ) =
                    reload model.loginUsername model.loginToken
            in
                ( m, c, Global.None )

        UserOpSubmitResult path (Err err) ->
            ( { model | userOpState = Just (Editing (Just ( path, err ))) }, Cmd.none, Global.None )

        UserOpUsername u_msg ->
            let
                m =
                    Textfield.update u_msg model.userOpUsername
            in
                ( { model | userOpUsername = m }, Cmd.none, Global.None )

        UserOpPassword p_msg ->
            let
                m =
                    Textfield.update p_msg model.userOpPassword
            in
                ( { model | userOpPassword = m }, Cmd.none, Global.None )

        UserOpRole key r_msg ->
            case (Dict.get key model.userOpRoles) of
                Nothing ->
                    ( model, Cmd.none, Global.None )

                Just m ->
                    let
                        newM =
                            Checkbox.update r_msg m

                        roleModelMap =
                            Dict.insert key newM model.userOpRoles
                    in
                        ( { model | userOpRoles = roleModelMap }, Cmd.none, Global.None )

        Noop ->
            ( model, Cmd.none, Global.None )



-- VIEW


disableNew : Model -> Bool
disableNew model =
    case model.roles of
        Just ( _, Ok _ ) ->
            case model.users of
                Just ( _, Ok _ ) ->
                    case model.userOpState of
                        Just (Deleting Nothing) ->
                            True

                        _ ->
                            False

                _ ->
                    True

        _ ->
            True


disableReload : Model -> Bool
disableReload model =
    let
        disableIt =
            case model.userOpState of
                Just (Deleting Nothing) ->
                    True

                _ ->
                    False
    in
        disableIt || (model.roles == Nothing) || (model.users == Nothing)


disableEdit : Model -> CmsUser -> Bool
disableEdit model user =
    disableNew model || (model.loginUsername /= "void" && user.username == "void")


disableDelete : Model -> CmsUser -> Bool
disableDelete model user =
    disableNew model || user.username == "void"


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
    li [] [ span [] [ text role ] ]


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


userRow : Model -> CmsUser -> ( String, Html Msg )
userRow model user =
    ( user.username
    , tr []
        [ td [] [ userNameSpan model.loginUsername user.username ]
        , td []
            [ ul
                [ style
                    [ ( "list-style-type", "none" )
                    , ( "margin", "0rem" )
                    , ( "padding", "0rem" )
                    ]
                ]
                (List.map userRoleSpan user.roles)
            ]
        , td []
            [ button
                [ class "mdc-button mdc-button--dense mdc-button--compact mdc-button--primary"
                , disabled (disableEdit model user)
                , onClick (UserOpEdit user)
                ]
                [ text "Edit" ]
            , button
                [ class "mdc-button mdc-button--dense mdc-button--compact mdc-button--primary"
                , disabled (disableDelete model user)
                , onClick (UserOpDelete user)
                ]
                [ text "Delete" ]
            ]
        ]
    )


dataErrLi : Maybe ( String, Result Http.Error a ) -> Maybe (Html Msg)
dataErrLi data =
    case data of
        Just ( path, Err err ) ->
            Just (li [] [ text (httpErrorString path err) ])

        _ ->
            Nothing


dataErrUl : Model -> Html Msg
dataErrUl model =
    ul
        [ style
            [ ( "color", "red" )
            , ( "list-style-type", "none" )
            , ( "margin", "0rem" )
            , ( "padding", "0rem" )
            ]
        ]
        (List.filterMap
            identity
            (case model.userOpState of
                Just (Deleting (Just ( path, err ))) ->
                    [ dataErrLi model.users, dataErrLi model.roles, Just (li [] [ text (httpErrorString path err) ]) ]

                _ ->
                    [ dataErrLi model.users, dataErrLi model.roles ]
            )
        )


userTable : Model -> Html Msg
userTable model =
    let
        users =
            getUsersFromModel model
    in
        div []
            [ div [] [ dataErrUl model ]
            , table []
                [ thead []
                    [ tr []
                        [ td [] [ text "Username" ]
                        , td [] [ text "Roles" ]
                        , td []
                            [ button
                                [ class "mdc-button mdc-button--dense mdc-button--compact mdc-button--primary"
                                , disabled (disableNew model)
                                , onClick UserOpNew
                                ]
                                [ text "New" ]
                            , button
                                [ class "mdc-button mdc-button--dense mdc-button--compact mdc-button--primary"
                                , disabled (disableReload model)
                                , onClick (Reload model.loginUsername model.loginToken)
                                ]
                                [ text "Reload" ]
                            ]
                        ]
                    ]
                , Html.Keyed.node "tbody" [] (List.map (userRow model) users)
                ]
            ]


getUsersFromModel : Model -> List CmsUser
getUsersFromModel model =
    case model.users of
        Just ( _, Ok users ) ->
            users

        _ ->
            []


userOpRoleCheckbox : Bool -> String -> Checkbox.Model Msg -> List (Html Msg) -> List (Html Msg)
userOpRoleCheckbox disableIt name ckbModel ckbList =
    div []
        [ Checkbox.view
            (Checkbox.updateModel ckbModel
                [ Checkbox.Disabled (disableIt || ckbModel.disabled) ]
            )
        ]
        :: ckbList


userOpErrStr : Model -> String
userOpErrStr model =
    case model.userOpState of
        Just (Editing (Just ( path, err ))) ->
            httpErrorString path err

        _ ->
            ""


userOpView : Model -> Html Msg
userOpView model =
    let
        submitting =
            model.userOpState == Just Submitting

        editUser =
            model.userOpUsername.disabled

        um =
            (Textfield.updateModel
                model.userOpUsername
                [ Textfield.Disabled (submitting || editUser) ]
            )

        pm =
            (Textfield.updateModel model.userOpPassword [ Textfield.Disabled submitting ])

        title =
            if model.userOpUsername.disabled then
                "Edit User"
            else
                "New User"
    in
        div []
            [ p [] [ text title ]
            , p [ style [ ( "color", "red" ) ] ] [ text (userOpErrStr model) ]
            , Textfield.view um
            , Textfield.view pm
            , div [ style [ ( "margin-top", "1rem" ) ] ]
                [ div [] [ text "Roles" ]
                , div [] (Dict.foldl (userOpRoleCheckbox submitting) [] model.userOpRoles)
                ]
            , div [ style [ ( "margin-top", "1rem" ) ] ]
                [ button
                    [ class "mdc-button mdc-button--raised mdc-button--dense mdc-button-compact"
                    , onClick UserOpCancel
                    , disabled submitting
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "mdc-button mdc-button--raised mdc-button--dense mdc-button-compact"
                    , style [ ( "margin-left", "1rem" ) ]
                    , onClick (UserOpSubmit model.userOpUsername.disabled)
                    , disabled
                        (submitting
                            || (not editUser
                                    && (model.userOpUsername.value
                                            == ""
                                            || model.userOpPassword.value
                                            == ""
                                       )
                               )
                        )
                    ]
                    [ text "Submit" ]
                ]
            ]


view : Model -> Html Msg
view model =
    let
        content =
            case model.userOpState of
                Nothing ->
                    userTable model

                Just (Deleting _) ->
                    userTable model

                _ ->
                    userOpView model
    in
        div
            []
            [ toolbar model
            , div [ class "mdc-toolbar-fixed-adjust" ]
                []
            , div [ style [ ( "padding", "1rem" ) ] ]
                [ content ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Http


createOrUpdateUser : String -> String -> CmsUser -> (String -> Result Http.Error String -> Msg) -> Cmd Msg
createOrUpdateUser path token user msgGenerator =
    let
        username =
            Http.encodeUri user.username

        role =
            Http.encodeUri (String.join "," user.roles)

        url =
            if user.password == "" then
                path ++ "?username=" ++ username ++ "&role=" ++ role
            else
                path ++ "?username=" ++ username ++ "&password=" ++ (Http.encodeUri user.password) ++ "&role=" ++ role

        req =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" token ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectString
                , timeout = Just (Time.second * 5)
                , withCredentials = False
                }
    in
        Http.send (msgGenerator path) req


createUser : String -> CmsUser -> Cmd Msg
createUser token user =
    createOrUpdateUser "/api/login/create" token user UserOpSubmitResult


updateUser : String -> CmsUser -> Cmd Msg
updateUser token user =
    createOrUpdateUser "/api/login/update" token user UserOpSubmitResult


deleteUser : String -> CmsUser -> Cmd Msg
deleteUser token user =
    let
        path =
            "/api/login/delete"

        username =
            Http.encodeUri user.username

        req =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" token ]
                , url = path ++ "?username=" ++ username
                , body = Http.emptyBody
                , expect = Http.expectString
                , timeout = Just (Time.second * 5)
                , withCredentials = False
                }
    in
        Http.send (UserOpDeleteResult path) req


getRoles : String -> String -> Cmd Msg
getRoles path token =
    httpGetJson path token decodeRoles (Roles path)


decodeRoles : Json.Decoder (List CmsRole)
decodeRoles =
    Json.list <|
        Json.map2
            CmsRole
            (Json.field "name" Json.string)
            (Json.field "value" Json.int)


getUsers : String -> String -> Cmd Msg
getUsers path token =
    httpGetJson path token decodeUsers (Users path)


decodeUsers : Json.Decoder (List CmsUser)
decodeUsers =
    Json.list <|
        Json.map2
            (\username role -> CmsUser username "" role)
            (Json.field "username" Json.string)
            (Json.field "role" (Json.list Json.string))
