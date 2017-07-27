module Main exposing (main)

import Defs exposing (..)
import Html exposing (Html, div, text)
import Component.Login as LoginComp
import Component.Article as ArticleComp
import Time


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        ( loginModel, loginCmd ) =
            LoginComp.init "/api/login" "Article CMS" (Time.minute * 30)

        ( articleModel, articleCmd ) =
            ArticleComp.init
    in
        ( { lockUI = False
          , login = loginModel
          , article = articleModel
          }
        , Cmd.batch
            [ loginCmd
            , articleCmd
            ]
        )



-- UPDATE


handleLoginStateChange : Model -> Model -> ( Model, Cmd Msg )
handleLoginStateChange prevModel newModel =
    case prevModel.login.auth of
        AuthSuccess _ ->
            case newModel.login.auth of
                Unauthorized ->
                    ArticleComp.logout newModel

                _ ->
                    ( newModel, Cmd.none )

        AuthInProgress ->
            case newModel.login.auth of
                AuthSuccess data ->
                    ArticleComp.login data newModel

                _ ->
                    ( newModel, Cmd.none )

        _ ->
            ( newModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login loginMsg ->
            let
                ( m, c ) =
                    LoginComp.update loginMsg model

                ( m1, c1 ) =
                    handleLoginStateChange model m
            in
                m1 ! [ c, c1 ]

        Article articleMsg ->
            ArticleComp.update articleMsg model

        Noop ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ LoginComp.view model
        , ArticleComp.view model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ LoginComp.subscriptions model ]
