module Component.Article exposing (view, update, init, login, logout)

import Defs
    exposing
        ( Model
        , ArticleModel
        , Msg(Article)
        , ArticleMsg(..)
        , AuthData
        , AuthState(..)
        , ArticlePageState(..)
        , ArticleListState(..)
        , ArticleEditMode(..)
        , AjaxError
        , ArticleDraft
        , decodeArticleDraft
        , encodeArticleDraft
        , CmsArticle
        , CmsArticleListResponse
        , decodeCmsArticle
        , decodeCmsArticleListResponse
        , CmsRoleValue(..)
        , hasCmsRole
        , hasCmsRoles
        )
import Task
import Bitwise
import Http
import Html exposing (Html, div, text, button, span, label)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Common.Debug exposing (debug)
import Common.Util
    exposing
        ( ajaxErrorString
        , httpGetJson
        , httpGetString
        , httpPostJson
        )
import MDC.Textfield as Textfield


initArticles : CmsArticleListResponse
initArticles =
    CmsArticleListResponse [] Nothing


initModel : ArticleModel
initModel =
    { state = ArticlePageListing ArticleListLoading Nothing
    , articles = Ok initArticles
    , edited = False
    , headline = Textfield.init ArticleHeadline []
    , summary = Textfield.init ArticleSummary []
    , content = Textfield.init ArticleContent []
    }


init : ( ArticleModel, Cmd Msg )
init =
    initModel ! []


reload : AuthData -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg )
reload auth model =
    ( { model
        | state = ArticlePageListing ArticleListLoading Nothing
        , articles = Ok initArticles
      }
    , getArticles auth.token Nothing
    )


login : AuthData -> Model -> ( Model, Cmd Msg )
login auth model =
    let
        ( m, c ) =
            reload auth model.article
    in
        ( { model | article = m, lockUI = True }, Cmd.map Article c )


logout : Model -> ( Model, Cmd Msg )
logout model =
    let
        m =
            model.article

        m1 =
            { m
                | state = ArticlePageListing ArticleListLoading Nothing
                , articles = Ok initArticles
            }
    in
        ( { model | article = m1 }
        , Cmd.none
        )


update : ArticleMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.login.auth of
        AuthSuccess data ->
            let
                ( m, c, lock ) =
                    update_ msg data model.article
            in
                ( { model | article = m, lockUI = lock }, Cmd.map Article c )

        _ ->
            model ! []


handleArticleListLoaded : String -> Result Http.Error CmsArticleListResponse -> ArticleModel -> ArticleModel
handleArticleListLoaded path result model =
    case result of
        Ok resp ->
            { model
                | state = ArticlePageListing ArticleListLoadedSuccess Nothing
                , articles = Ok resp
            }

        Err err ->
            { model
                | state = ArticlePageListing ArticleListLoadedFailure Nothing
                , articles = Err (AjaxError path err)
            }


storeListingActionError : ArticleListState -> AjaxError -> ArticleModel -> ArticleModel
storeListingActionError listState err model =
    { model | state = ArticlePageListing listState (Just err) }


clearListingActionError : ArticleModel -> ArticleModel
clearListingActionError model =
    case model.state of
        ArticlePageListing listState _ ->
            { model | state = ArticlePageListing listState Nothing }

        _ ->
            model


handleArticleNewResponse : ArticleListState -> String -> Result Http.Error ArticleDraft -> ArticleModel -> ArticleModel
handleArticleNewResponse listState path result model =
    case result of
        Ok articleData ->
            let
                hm =
                    Textfield.updateModel
                        model.headline
                        [ Textfield.Hint "Headline"
                        , Textfield.Value articleData.headline
                        ]

                sm =
                    Textfield.updateModel
                        model.summary
                        [ Textfield.Hint "Summary"
                        , Textfield.Value articleData.summary
                        ]

                cm =
                    Textfield.updateModel
                        model.content
                        [ Textfield.Hint "Content"
                        , Textfield.Value articleData.content
                        ]
            in
                { model
                    | state = ArticlePageEditing ArticleEditCreate articleData Nothing
                    , edited = True
                    , headline = hm
                    , summary = sm
                    , content = cm
                }

        Err err ->
            storeListingActionError listState (AjaxError path err) model


handleArticleDiscardResponse : ArticleEditMode -> ArticleDraft -> String -> Result Http.Error String -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleDiscardResponse mode article path result model =
    case result of
        Ok _ ->
            ( { model
                | state = ArticlePageListing ArticleListLoading Nothing
                , edited = True
              }
            , Task.perform (always ArticleReload) (Task.succeed True)
            , True
            )

        Err err ->
            ( { model | state = ArticlePageEditing mode article (Just (AjaxError path err)) }
            , Cmd.none
            , False
            )


populateArticleDraft : ArticleModel -> ArticleDraft -> ArticleDraft
populateArticleDraft model article =
    { article
        | headline = model.headline.value
        , summary = model.summary.value
        , content = model.content.value
    }


handleArticleSaveRequest : AuthData -> ArticleEditMode -> ArticleDraft -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleSaveRequest auth mode article model =
    ( model, saveArticle auth mode (populateArticleDraft model article), True )


handleArticleSaveResponse : ArticleEditMode -> ArticleDraft -> String -> Result Http.Error String -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleSaveResponse mode article path result model =
    case result of
        Ok _ ->
            ( { model
                | state = ArticlePageEditing mode { article | guid = article.id } Nothing
                , edited = True
              }
            , Cmd.none
            , False
            )

        Err err ->
            ( { model | state = ArticlePageEditing mode article (Just (AjaxError path err)) }
            , Cmd.none
            , False
            )


handleArticleSubmitRequest : AuthData -> ArticleEditMode -> ArticleDraft -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleSubmitRequest auth mode article model =
    ( model, submitArticle auth mode (populateArticleDraft model article), True )


handleArticleSubmitResponse : ArticleEditMode -> ArticleDraft -> String -> Result Http.Error String -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleSubmitResponse mode article path result model =
    case result of
        Ok _ ->
            ( { model
                | state = ArticlePageListing ArticleListLoading Nothing
                , edited = True
              }
            , Task.perform (always ArticleReload) (Task.succeed True)
            , True
            )

        Err err ->
            ( { model | state = ArticlePageEditing mode article (Just (AjaxError path err)) }
            , Cmd.none
            , False
            )


handleArticleClose : AuthData -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleClose auth model =
    if model.edited then
        let
            ( m, c ) =
                reload auth model
        in
            ( m, c, True )
    else
        ( { model
            | state = ArticlePageListing ArticleListLoadedSuccess Nothing
            , edited = False
          }
        , Cmd.none
        , False
        )


update_ : ArticleMsg -> AuthData -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
update_ msg auth model =
    case msg of
        ArticleNoop ->
            ( model, Cmd.none, False )

        ArticleHeadline tfm ->
            ( { model | headline = Textfield.update tfm model.headline }, Cmd.none, False )

        ArticleSummary tfm ->
            ( { model | summary = Textfield.update tfm model.summary }, Cmd.none, False )

        ArticleContent tfm ->
            ( { model | content = Textfield.update tfm model.content }, Cmd.none, False )

        ArticleNewRequest listState ->
            ( model, createArticle listState auth, True )

        ArticleNewResponse listState path result ->
            ( handleArticleNewResponse listState path result model, Cmd.none, False )

        ArticleDiscardRequest auth mode article ->
            ( model, discardArticle auth mode article, True )

        ArticleDiscardResponse mode article path result ->
            handleArticleDiscardResponse mode article path result model

        ArticleSaveRequest auth mode article ->
            handleArticleSaveRequest auth mode article model

        ArticleSaveResponse mode article path result ->
            handleArticleSaveResponse mode article path result model

        ArticleSubmitRequest auth mode article ->
            handleArticleSubmitRequest auth mode article model

        ArticleSubmitResponse mode article path result ->
            handleArticleSubmitResponse mode article path result model

        ArticleClose ->
            handleArticleClose auth model

        ArticleReload ->
            let
                ( m, c ) =
                    reload auth model
            in
                ( m, c, True )

        ArticleListLoaded path result ->
            ( handleArticleListLoaded path result model, Cmd.none, False )


view : Model -> Html Msg
view model =
    case model.login.auth of
        AuthSuccess data ->
            Html.map Article (view_ data model.lockUI model.article)

        _ ->
            div [] []


disableNew : AuthData -> Bool -> ArticleListState -> Bool
disableNew auth lockUI listState =
    not (hasCmsRole auth.role CmsRoleArticleCreate) || lockUI || (listState /= ArticleListLoadedSuccess)


disableReload : Bool -> ArticleListState -> Bool
disableReload lockUI listState =
    lockUI || (listState == ArticleListLoading)


listingErrStr : ArticleModel -> Maybe AjaxError -> String
listingErrStr model actionErr =
    case model.articles of
        Ok _ ->
            case actionErr of
                Nothing ->
                    ""

                Just err ->
                    ajaxErrorString err

        Err err ->
            ajaxErrorString err


viewListing : ArticleListState -> Maybe AjaxError -> AuthData -> Bool -> ArticleModel -> Html ArticleMsg
viewListing listState actionErr auth lockUI model =
    div []
        [ div [ style [ ( "color", "red" ) ] ] [ text (listingErrStr model actionErr) ]
        , div [ class "mdc-layout-grid" ]
            [ div [ class "mdc-layout-grid__inner" ]
                [ div [ class "mdc-layout-grid__cell mdc-layout-grid--align-left" ]
                    [ button
                        [ class "mdc-button mdc-button--raised"
                        , disabled (disableNew auth lockUI listState)
                        , onClick (ArticleNewRequest listState)
                        ]
                        [ text "New" ]
                    ]
                , div [ class "mdc-layout-grid__cell" ] []
                , div [ class "mdc-layout-grid__cell mdc-layout-grid--align-right" ]
                    [ button
                        [ class "mdc-button mdc-button--raised"
                        , disabled (disableReload lockUI listState)
                        , onClick ArticleReload
                        ]
                        [ text "Reload" ]
                    ]
                ]
            ]
        ]


editingError : Maybe AjaxError -> String
editingError me =
    case me of
        Nothing ->
            ""

        Just err ->
            ajaxErrorString err


disableDiscard : Bool -> AuthData -> ArticleDraft -> Bool
disableDiscard lockUI auth article =
    lockUI
        || ((auth.username /= article.lockedBy) && (not (hasCmsRole auth.role CmsRoleArticleSubmit)))


disableSubmit : Bool -> AuthData -> ArticleDraft -> Bool
disableSubmit =
    disableDiscard


viewCreating : AuthData -> Bool -> ArticleModel -> ArticleDraft -> Maybe AjaxError -> Html ArticleMsg
viewCreating auth lockUI model article me =
    div [ style [ ( "padding", "2rem 1rem 2rem 1rem" ) ] ]
        [ div [ style [ ( "color", "red" ) ] ] [ text (editingError me) ]
        , div [] [ label [ class "mdc-typography--title" ] [ text ("Article: " ++ article.id) ] ]
        , div [] [ Textfield.view (Textfield.updateModel model.headline [ Textfield.Disabled lockUI ]) ]
        , div [] [ Textfield.view (Textfield.updateModel model.summary [ Textfield.Disabled lockUI ]) ]
        , div [] [ Textfield.view (Textfield.updateModel model.content [ Textfield.Disabled lockUI ]) ]
        , div []
            [ button
                [ class "mdc-button mdc-button--raised"
                , disabled (lockUI || article.guid == "")
                , onClick ArticleClose
                ]
                [ text "Close" ]
            , button
                [ class "mdc-button mdc-button--raised"
                , disabled (disableDiscard lockUI auth article)
                , onClick (ArticleDiscardRequest auth ArticleEditCreate article)
                ]
                [ text "Discard" ]
            , button
                [ class "mdc-button mdc-button--raised"
                , disabled (lockUI || auth.username /= article.lockedBy)
                , onClick (ArticleSaveRequest auth ArticleEditCreate article)
                ]
                [ text "Save" ]
            , button
                [ class "mdc-button mdc-button--raised"
                , disabled (disableSubmit lockUI auth article)
                , onClick (ArticleSubmitRequest auth ArticleEditCreate article)
                ]
                [ text "Submit" ]
            ]
        ]


view_ : AuthData -> Bool -> ArticleModel -> Html ArticleMsg
view_ auth lockUI model =
    case model.state of
        ArticlePageListing listState actionErr ->
            viewListing listState actionErr auth lockUI model

        ArticlePageEditing mode article err ->
            case mode of
                ArticleEditCreate ->
                    viewCreating auth lockUI model article err

                ArticleEditEdit ->
                    div [] [ text "editing" ]


getArticles : String -> Maybe String -> Cmd ArticleMsg
getArticles token cursorMark =
    let
        path =
            case cursorMark of
                Nothing ->
                    "/api/articles"

                Just cm ->
                    "/api/articles?cursorMark=" ++ cm
    in
        httpGetJson path token decodeCmsArticleListResponse (ArticleListLoaded path)


createArticle : ArticleListState -> AuthData -> Cmd ArticleMsg
createArticle listState auth =
    let
        path =
            "/api/article/create?username=" ++ auth.username
    in
        httpGetJson path auth.token decodeArticleDraft (ArticleNewResponse listState path)


discardArticle : AuthData -> ArticleEditMode -> ArticleDraft -> Cmd ArticleMsg
discardArticle auth mode article =
    let
        path =
            if auth.username == article.lockedBy then
                "/api/article/discard-self?id=" ++ article.id
            else
                "/api/article/discard-other?id=" ++ article.id
    in
        httpGetString path auth.token (ArticleDiscardResponse mode article path)


saveArticle : AuthData -> ArticleEditMode -> ArticleDraft -> Cmd ArticleMsg
saveArticle auth mode article =
    let
        path =
            "/api/article/save?id=" ++ article.id
    in
        httpPostJson path auth.token (encodeArticleDraft article) (ArticleSaveResponse mode article path)


submitArticle : AuthData -> ArticleEditMode -> ArticleDraft -> Cmd ArticleMsg
submitArticle auth mode article =
    let
        path =
            if auth.username == article.lockedBy then
                "/api/article/submit-self?id=" ++ article.id
            else
                "/api/article/submit-other?id=" ++ article.id
    in
        httpPostJson path auth.token (encodeArticleDraft article) (ArticleSubmitResponse mode article path)
