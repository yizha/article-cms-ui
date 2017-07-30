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
        , AjaxError
        , ArticleDraft
        , ArticleVersion
        , decodeArticleDraft
        , encodeArticleDraft
        , CmsArticle
        , CmsArticleView
        , CmsArticleViewData(..)
        , CmsArticleVersion(..)
        , getArticleVersion
        , isVersionEditing
        , CmsArticleListResponse
        , CmsArticleListView
        , cmsArticleList2View
        , decodeCmsArticle
        , decodeCmsArticleListResponse
        , CmsRoleValue(..)
        , hasCmsRole
        , hasAnyCmsRole
        )
import Task
import Bitwise
import Json.Decode as Json
import Http
import Date exposing (Date)
import Date.Extra as DateExtra
import Html exposing (Html, div, text, button, span, label, select, option, p)
import Html.Attributes exposing (class, disabled, style, selected, value)
import Html.Events exposing (onClick, on, targetValue)
import Html.Keyed
import Common.Debug exposing (debug)
import Common.Util
    exposing
        ( ajaxErrorString
        , httpGetJson
        , httpGetString
        , httpPostJson
        )
import MDC.Textfield as Textfield


initArticles : CmsArticleListView
initArticles =
    CmsArticleListView [] Nothing Nothing


initModel : ArticleModel
initModel =
    { state = ArticlePageListLoading
    , articles = initArticles
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
        | state = ArticlePageListLoading
        , articles = initArticles
      }
    , getArticles auth.token Nothing Nothing
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
                | state = ArticlePageListLoading
                , articles = initArticles
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
                | state = ArticlePageListLoadedSuccess Nothing
                , articles = cmsArticleList2View resp
                , edited = False
            }

        Err err ->
            { model
                | state = ArticlePageListLoadedFailure (AjaxError path err)
                , edited = False
            }


storeListingActionError : AjaxError -> ArticleModel -> ArticleModel
storeListingActionError err model =
    { model | state = ArticlePageListLoadedSuccess (Just err) }


clearListingActionError : ArticleModel -> ArticleModel
clearListingActionError model =
    case model.state of
        ArticlePageListLoadedSuccess _ ->
            { model | state = ArticlePageListLoadedSuccess Nothing }

        _ ->
            model


openArticleDraft : ArticleDraft -> ArticleModel -> ArticleModel
openArticleDraft draft model =
    let
        hm =
            Textfield.updateModel
                model.headline
                [ Textfield.Hint "Headline"
                , Textfield.Value draft.headline
                ]

        sm =
            Textfield.updateModel
                model.summary
                [ Textfield.Hint "Summary"
                , Textfield.Value draft.summary
                ]

        cm =
            Textfield.updateModel
                model.content
                [ Textfield.Hint "Content"
                , Textfield.Value draft.content
                ]
    in
        { model
            | state = ArticlePageEditing draft Nothing
            , headline = hm
            , summary = sm
            , content = cm
        }


handleArticleNewResponse : String -> Result Http.Error ArticleDraft -> ArticleModel -> ArticleModel
handleArticleNewResponse path result model =
    case result of
        Ok draft ->
            let
                m =
                    openArticleDraft draft model
            in
                { m | edited = True }

        Err err ->
            storeListingActionError (AjaxError path err) model


handleArticleDiscardResponse : ArticleDraft -> String -> Result Http.Error String -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleDiscardResponse article path result model =
    case result of
        Ok _ ->
            ( { model
                | state = ArticlePageListLoading
                , edited = True
              }
            , Task.perform (always ArticleReload) (Task.succeed True)
            , True
            )

        Err err ->
            ( { model | state = ArticlePageEditing article (Just (AjaxError path err)) }
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


handleArticleSaveRequest : AuthData -> ArticleDraft -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleSaveRequest auth article model =
    ( model, saveArticle auth (populateArticleDraft model article), True )


handleArticleSaveResponse : ArticleDraft -> String -> Result Http.Error String -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleSaveResponse article path result model =
    case result of
        Ok _ ->
            ( { model
                | state = ArticlePageEditing { article | guid = article.id } Nothing
                , edited = True
              }
            , Cmd.none
            , False
            )

        Err err ->
            ( { model | state = ArticlePageEditing article (Just (AjaxError path err)) }
            , Cmd.none
            , False
            )


handleArticleSubmitRequest : AuthData -> ArticleDraft -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleSubmitRequest auth article model =
    ( model, submitArticle auth (populateArticleDraft model article), True )


handleArticleSubmitResponse : ArticleDraft -> String -> Result Http.Error String -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleSubmitResponse article path result model =
    case result of
        Ok _ ->
            ( { model
                | state = ArticlePageListLoading
                , edited = True
              }
            , Task.perform (always ArticleReload) (Task.succeed True)
            , True
            )

        Err err ->
            ( { model | state = ArticlePageEditing article (Just (AjaxError path err)) }
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
            | state = ArticlePageListLoadedSuccess Nothing
            , edited = False
          }
        , Cmd.none
        , False
        )


handleArticleVersionSelect : String -> String -> ArticleModel -> ArticleModel
handleArticleVersionSelect guid ver model =
    let
        f =
            \one ->
                if one.guid == guid then
                    case one.data of
                        CmsArticleViewDataVersions head currentSelected tail ->
                            let
                                versions =
                                    List.concat [ head, [ currentSelected ], tail ]

                                ( h, s, t ) =
                                    List.foldr
                                        (\x ( h, s, t ) ->
                                            if List.length s > 0 then
                                                ( x :: h, s, t )
                                            else
                                                let
                                                    v =
                                                        getArticleVersion x
                                                in
                                                    if v.version == ver then
                                                        ( h, x :: s, t )
                                                    else
                                                        ( h, s, x :: t )
                                        )
                                        ( [], [], [] )
                                        versions
                            in
                                case List.head s of
                                    Nothing ->
                                        one

                                    Just selectedArticle ->
                                        { one | data = CmsArticleViewDataVersions h selectedArticle t }

                        _ ->
                            one
                else
                    one

        articleListView =
            model.articles
    in
        { model | articles = { articleListView | articles = (List.map f articleListView.articles) } }


handleArticleEditResponse : String -> Result Http.Error ArticleDraft -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleEditResponse path result model =
    case result of
        Ok draft ->
            ( openArticleDraft draft { model | edited = True }
            , Cmd.none
            , False
            )

        Err err ->
            ( { model | state = ArticlePageListLoadedSuccess (Just (AjaxError path err)) }
            , Cmd.none
            , False
            )


handleArticlePublishResponse : String -> Result Http.Error String -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticlePublishResponse path result model =
    case result of
        Ok _ ->
            ( { model
                | state = ArticlePageListLoading
                , edited = True
              }
            , Task.perform (always ArticleReload) (Task.succeed True)
            , True
            )

        Err err ->
            ( { model
                | state = ArticlePageListLoadedSuccess (Just (AjaxError path err))
                , edited = False
              }
            , Cmd.none
            , False
            )


handleArticleUnpublishResponse : String -> Result Http.Error String -> ArticleModel -> ( ArticleModel, Cmd ArticleMsg, Bool )
handleArticleUnpublishResponse path result model =
    case result of
        Ok _ ->
            ( { model
                | state = ArticlePageListLoading
                , edited = True
              }
            , Task.perform (always ArticleReload) (Task.succeed True)
            , True
            )

        Err err ->
            ( { model
                | state = ArticlePageListLoadedSuccess (Just (AjaxError path err))
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

        ArticleNewRequest ->
            ( model, createArticle auth, True )

        ArticleNewResponse path result ->
            ( handleArticleNewResponse path result model, Cmd.none, False )

        ArticleDiscardRequest article ->
            ( model, discardArticle auth article, True )

        ArticleDiscardResponse article path result ->
            handleArticleDiscardResponse article path result model

        ArticleSaveRequest article ->
            handleArticleSaveRequest auth article model

        ArticleSaveResponse article path result ->
            handleArticleSaveResponse article path result model

        ArticleSubmitRequest article ->
            handleArticleSubmitRequest auth article model

        ArticleSubmitResponse article path result ->
            handleArticleSubmitResponse article path result model

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

        ArticleVersionSelect guid ver ->
            ( handleArticleVersionSelect guid ver model, Cmd.none, False )

        ArticleOpenDraft draft ->
            ( openArticleDraft draft model, Cmd.none, False )

        ArticleEditRequest ver ->
            ( model, editArticle auth ver, True )

        ArticleEditResponse path result ->
            handleArticleEditResponse path result model

        ArticlePublishRequest ver ->
            ( model, publishArticle auth ver, True )

        ArticlePublishResponse path result ->
            handleArticlePublishResponse path result model

        ArticleUnpublishRequest ver ->
            ( model, unpublishArticle auth ver, True )

        ArticleUnpublishResponse path result ->
            handleArticleUnpublishResponse path result model


view : Model -> Html Msg
view model =
    case model.login.auth of
        AuthSuccess data ->
            Html.map Article (view_ data model.lockUI model.article)

        _ ->
            div [] []


disableNew : AuthData -> Bool -> ArticleModel -> Bool
disableNew auth lockUI model =
    lockUI
        || (not (hasCmsRole auth.role CmsRoleArticleCreate))
        || (case model.state of
                ArticlePageListLoadedSuccess _ ->
                    False

                _ ->
                    True
           )


disableReload : Bool -> ArticleModel -> Bool
disableReload lockUI model =
    lockUI || (model.state == ArticlePageListLoading)


listingErrStr : ArticleModel -> Maybe AjaxError -> String
listingErrStr model actionErr =
    case model.state of
        ArticlePageListLoadedFailure err ->
            ajaxErrorString err

        ArticlePageListLoadedSuccess actionErr ->
            case actionErr of
                Just err ->
                    ajaxErrorString err

                Nothing ->
                    ""

        _ ->
            ""


viewCmsArticleSelectOpt : ArticleVersion -> CmsArticleVersion -> ( String, Html ArticleMsg )
viewCmsArticleSelectOpt selectedArticle version =
    let
        ( optVal, dispVal ) =
            case version of
                CmsArticleVersionOnly v ->
                    ( v.version, v.version )

                CmsArticleVersionEditing v _ ->
                    ( v.version, v.version ++ " (editing)" )

                CmsArticleVersionPublished v _ ->
                    ( v.version, v.version ++ " (published)" )

                CmsArticleVersionEditingAndPublished v _ _ ->
                    ( v.version, v.version ++ " (editing/published)" )
    in
        ( optVal
        , option
            [ value optVal
            , selected (optVal == selectedArticle.version)
            ]
            [ text dispVal ]
        )


viewCmsArticleSelect : Bool -> ArticleVersion -> String -> List CmsArticleVersion -> Html ArticleMsg
viewCmsArticleSelect lockUI selectedArticle guid versions =
    div []
        [ Html.Keyed.node
            "select"
            [ class "mdc-select"
            , disabled lockUI
            , on "change" (Json.map (ArticleVersionSelect guid) targetValue)
            ]
            (List.map (viewCmsArticleSelectOpt selectedArticle) versions)
        ]


viewCmsArticleSelected : CmsArticleVersion -> Html ArticleMsg
viewCmsArticleSelected cav =
    let
        ver =
            getArticleVersion cav
    in
        div []
            [ p [ class "mdc-typography--title" ] [ text ver.headline ]
            , p [ class "mdc-typography--subheadline2" ] [ text ver.summary ]
            ]


viewCmsArticleDraft : ArticleDraft -> Html ArticleMsg
viewCmsArticleDraft draft =
    div []
        [ p [ class "mdc-typography--title" ] [ text draft.headline ]
        , p [ class "mdc-typography--subheadline2" ] [ text draft.summary ]
        ]


showOpenBtn : AuthData -> ArticleDraft -> Bool
showOpenBtn auth draft =
    let
        role =
            auth.role

        username =
            auth.username

        lockedBy =
            draft.lockedBy
    in
        ((hasCmsRole role CmsRoleArticleCreate) && (username == lockedBy))
            || ((hasCmsRole role CmsRoleArticleSubmit) && (username /= lockedBy))
            || ((hasAnyCmsRole role [ CmsRoleArticleEditSelf, CmsRoleArticleEditOther ]) && (username == lockedBy))


showEditBtn : AuthData -> ArticleVersion -> Bool
showEditBtn auth ver =
    let
        role =
            auth.role

        username =
            auth.username

        revisedBy =
            ver.revisedBy
    in
        ((hasCmsRole role CmsRoleArticleEditSelf) && (username == revisedBy))
            || ((hasCmsRole role CmsRoleArticleEditOther) && (username /= revisedBy))


showPublishBtn : AuthData -> Bool
showPublishBtn auth =
    hasCmsRole auth.role CmsRoleArticlePublish


openBtn : AuthData -> Bool -> ArticleDraft -> Html ArticleMsg
openBtn auth lockUI draft =
    if showOpenBtn auth draft then
        button
            [ class "mdc-button mdc-button--raised"
            , disabled lockUI
            , onClick (ArticleOpenDraft draft)
            ]
            [ text "Open" ]
    else
        div [] []


editBtn : AuthData -> Bool -> ArticleVersion -> Bool -> Html ArticleMsg
editBtn auth lockUI ver editing =
    if (not editing) && (showEditBtn auth ver) then
        button
            [ class "mdc-button mdc-button--raised"
            , disabled lockUI
            , onClick (ArticleEditRequest ver)
            ]
            [ text "Edit" ]
    else
        div [] []


publishBtn : AuthData -> Bool -> ArticleVersion -> Html ArticleMsg
publishBtn auth lockUI ver =
    if showPublishBtn auth then
        button
            [ class "mdc-button mdc-button--raised"
            , disabled lockUI
            , onClick (ArticlePublishRequest ver)
            ]
            [ text "Publish" ]
    else
        div [] []


unpublishBtn : AuthData -> Bool -> ArticleVersion -> Html ArticleMsg
unpublishBtn auth lockUI ver =
    if showPublishBtn auth then
        button
            [ class "mdc-button mdc-button--raised"
            , disabled lockUI
            , onClick (ArticleUnpublishRequest ver)
            ]
            [ text "Unpublish" ]
    else
        div [] []


viewCmsArticleSelectedActions : AuthData -> Bool -> CmsArticleVersion -> Bool -> Html ArticleMsg
viewCmsArticleSelectedActions auth lockUI cav editing =
    div
        [ style
            [ ( "display", "flex" )
            , ( "justify-content", "space-between" )
            ]
        ]
        (case cav of
            CmsArticleVersionOnly v ->
                [ editBtn auth lockUI v editing
                , publishBtn auth lockUI v
                ]

            CmsArticleVersionEditing v draft ->
                [ openBtn auth lockUI draft
                , publishBtn auth lockUI v
                ]

            CmsArticleVersionPublished v published ->
                [ editBtn auth lockUI v editing
                , unpublishBtn auth lockUI published
                ]

            CmsArticleVersionEditingAndPublished v draft published ->
                [ openBtn auth lockUI draft
                , unpublishBtn auth lockUI published
                ]
        )


viewCmsArticle : AuthData -> Bool -> CmsArticleView -> ( String, Html ArticleMsg )
viewCmsArticle auth lockUI article =
    ( article.guid
    , div [ class "mdc-layout-grid__cell--span-3 mdc-elevation--z4" ]
        [ div [ style [ ( "margin", "1rem" ) ] ]
            (case article.data of
                CmsArticleViewDataDraft draft ->
                    [ select
                        [ class "mdc-select"
                        , disabled True
                        ]
                        [ option [ selected True ] [ text "DRAFT ARTICLE" ] ]
                    , viewCmsArticleDraft draft
                    , openBtn auth lockUI draft
                    ]

                CmsArticleViewDataVersions head selectedArticle tail ->
                    let
                        av =
                            getArticleVersion selectedArticle

                        versions =
                            (List.concat [ head, [ selectedArticle ], tail ])

                        editing =
                            (List.any isVersionEditing versions)
                    in
                        [ viewCmsArticleSelect lockUI av article.guid versions
                        , viewCmsArticleSelected selectedArticle
                        , viewCmsArticleSelectedActions auth lockUI selectedArticle editing
                        ]
            )
        ]
    )


viewCmsArticleList : AuthData -> Bool -> ArticleModel -> Html ArticleMsg
viewCmsArticleList auth lockUI model =
    div [ class "mdc-layout-grid" ]
        [ Html.Keyed.node "div"
            [ class "mdc-layout-grid__inner" ]
            (List.map
                (viewCmsArticle auth lockUI)
                model.articles.articles
            )
        ]


viewListActions : AuthData -> Bool -> ArticleModel -> Html ArticleMsg
viewListActions auth lockUI model =
    div [ class "mdc-layout-grid" ]
        [ div [ class "mdc-layout-grid__inner" ]
            [ div [ class "mdc-layout-grid__cell mdc-layout-grid--align-left" ]
                [ button
                    [ class "mdc-button mdc-button--raised"
                    , disabled (disableNew auth lockUI model)
                    , onClick ArticleNewRequest
                    ]
                    [ text "New" ]
                ]
            , div [ class "mdc-layout-grid__cell" ] []
            , div [ class "mdc-layout-grid__cell mdc-layout-grid--align-right" ]
                [ button
                    [ class "mdc-button mdc-button--raised"
                    , disabled (disableReload lockUI model)
                    , onClick ArticleReload
                    ]
                    [ text "Reload" ]
                ]
            ]
        ]


viewLoadMore_ : AuthData -> Bool -> ArticleModel -> Html ArticleMsg
viewLoadMore_ auth lockUI model =
    div [ class "mdc-layout-grid" ]
        [ div [ class "mdc-layout-grid__inner" ]
            [ div [ class "mdc-layout-grid__cell--span-12 mdc-layout-grid--align-left" ]
                [ button
                    [ class "mdc-button mdc-button--raised"
                    ]
                    [ text "More" ]
                ]
            ]
        ]


viewLoadMore : AuthData -> Bool -> ArticleModel -> Html ArticleMsg
viewLoadMore auth lockUI model =
    case model.state of
        ArticlePageListLoadedSuccess _ ->
            if model.articles.cursorMark /= Nothing then
                viewLoadMore_ auth lockUI model
            else
                div [] []

        _ ->
            div [] []


viewListing : Maybe AjaxError -> AuthData -> Bool -> ArticleModel -> Html ArticleMsg
viewListing actionErr auth lockUI model =
    div []
        [ div [ style [ ( "color", "red" ) ] ] [ text (listingErrStr model actionErr) ]
        , viewListActions auth lockUI model
        , viewCmsArticleList auth lockUI model
        , viewLoadMore auth lockUI model
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


viewEditing : AuthData -> Bool -> ArticleModel -> ArticleDraft -> Maybe AjaxError -> Html ArticleMsg
viewEditing auth lockUI model article me =
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
                , onClick (ArticleDiscardRequest article)
                ]
                [ text "Discard" ]
            , button
                [ class "mdc-button mdc-button--raised"
                , disabled (lockUI || auth.username /= article.lockedBy)
                , onClick (ArticleSaveRequest article)
                ]
                [ text "Save" ]
            , button
                [ class "mdc-button mdc-button--raised"
                , disabled (disableSubmit lockUI auth article)
                , onClick (ArticleSubmitRequest article)
                ]
                [ text "Submit" ]
            ]
        ]


view_ : AuthData -> Bool -> ArticleModel -> Html ArticleMsg
view_ auth lockUI model =
    case model.state of
        ArticlePageListLoading ->
            viewListing Nothing auth lockUI model

        ArticlePageListLoadedSuccess actionErr ->
            viewListing actionErr auth lockUI model

        ArticlePageListLoadedFailure err ->
            viewListing (Just err) auth lockUI model

        ArticlePageEditing article err ->
            viewEditing auth lockUI model article err



--HTTP


formatDate : Date -> String
formatDate d =
    DateExtra.toUtcFormattedString "yyyy-MM-ddTHH:mm:ss.SSSZ" d


getArticles : String -> Maybe String -> Maybe Date -> Cmd ArticleMsg
getArticles token cursorMark createdAt =
    let
        path =
            case cursorMark of
                Nothing ->
                    case createdAt of
                        Nothing ->
                            "/api/articles"

                        Just d ->
                            "/api/articles?before=" ++ (formatDate d)

                Just cm ->
                    case createdAt of
                        Nothing ->
                            "/api/articles?cursorMark=" ++ cm

                        Just d ->
                            "/api/articles?cursorMark=" ++ cm ++ "&before=" ++ (formatDate d)
    in
        httpGetJson path token decodeCmsArticleListResponse (ArticleListLoaded path)


createArticle : AuthData -> Cmd ArticleMsg
createArticle auth =
    let
        path =
            "/api/article/create?username=" ++ auth.username
    in
        httpGetJson path auth.token decodeArticleDraft (ArticleNewResponse path)


discardArticle : AuthData -> ArticleDraft -> Cmd ArticleMsg
discardArticle auth article =
    let
        path =
            if auth.username == article.lockedBy then
                "/api/article/discard-self?id=" ++ article.id
            else
                "/api/article/discard-other?id=" ++ article.id
    in
        httpGetString path auth.token (ArticleDiscardResponse article path)


saveArticle : AuthData -> ArticleDraft -> Cmd ArticleMsg
saveArticle auth article =
    let
        path =
            "/api/article/save?id=" ++ article.id
    in
        httpPostJson path auth.token (encodeArticleDraft article) (ArticleSaveResponse article path)


submitArticle : AuthData -> ArticleDraft -> Cmd ArticleMsg
submitArticle auth article =
    let
        path =
            if auth.username == article.lockedBy then
                "/api/article/submit-self?id=" ++ article.id
            else
                "/api/article/submit-other?id=" ++ article.id
    in
        httpPostJson path auth.token (encodeArticleDraft article) (ArticleSubmitResponse article path)


editArticle : AuthData -> ArticleVersion -> Cmd ArticleMsg
editArticle auth article =
    let
        path =
            "/api/article/edit?id=" ++ article.id
    in
        httpGetJson path auth.token decodeArticleDraft (ArticleEditResponse path)


publishArticle : AuthData -> ArticleVersion -> Cmd ArticleMsg
publishArticle auth article =
    let
        path =
            "/api/article/publish?id=" ++ article.id
    in
        httpGetString path auth.token (ArticlePublishResponse path)


unpublishArticle : AuthData -> ArticleVersion -> Cmd ArticleMsg
unpublishArticle auth article =
    let
        path =
            "/api/article/unpublish?id=" ++ article.id
    in
        httpGetString path auth.token (ArticleUnpublishResponse path)
