module Component.Article exposing (view, update, init)

import Defs exposing (Model, ArticleModel, Msg(Article), ArticleMsg(..), AuthData, AuthState(..))
import Html exposing (Html, div, text)
import Common.Debug exposing (debug)
import Common.Util exposing (httpErrorString, httpGetJson)


initModel : ArticleModel
initModel =
    { articles = [] }


init : ( ArticleModel, Cmd Msg )
init =
    initModel ! []


update : ArticleMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( m, c ) =
            update_ msg model.article
    in
        ( { model | article = m }, Cmd.map Article c )


update_ : ArticleMsg -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg )
update_ msg model =
    case msg of
        ArticleNoop ->
            model ! []

        ArticleLogin ->
            model ! []

        ArticleLogout ->
            { model | articles = [] } ! []


view : Model -> Html Msg
view model =
    case model.login.auth of
        AuthSuccess data ->
            Html.map Article (view_ data model.article)

        _ ->
            div [] []


view_ : AuthData -> ArticleModel -> Html ArticleMsg
view_ auth model =
    div [] [ text "articles" ]
