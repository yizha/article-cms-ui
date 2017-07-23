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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login loginMsg ->
            LoginComp.update loginMsg model

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
