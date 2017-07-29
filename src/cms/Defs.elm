module Defs exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Http
import Bitwise
import Dict exposing (Dict)
import Json.Encode as JsonEncode
import Json.Decode as Json
import Json.Decode.Extra as JsonExtra
import MDC.Textfield as Textfield
import Common.Debug exposing (debug)


type alias CmsRole =
    Int


type CmsRoleValue
    = CmsRoleArticleCreate
    | CmsRoleArticleEditSelf
    | CmsRoleArticleEditOther
    | CmsRoleArticleSubmit
    | CmsRoleArticlePublish
    | CmsRoleLoginManage


roleValueToInt : CmsRoleValue -> Int
roleValueToInt v =
    case v of
        CmsRoleArticleCreate ->
            Bitwise.shiftLeftBy 0 1

        CmsRoleArticleEditSelf ->
            Bitwise.shiftLeftBy 1 1

        CmsRoleArticleEditOther ->
            Bitwise.shiftLeftBy 2 1

        CmsRoleArticleSubmit ->
            Bitwise.shiftLeftBy 3 1

        CmsRoleArticlePublish ->
            Bitwise.shiftLeftBy 4 1

        CmsRoleLoginManage ->
            Bitwise.shiftLeftBy 20 1


hasCmsRole : CmsRole -> CmsRoleValue -> Bool
hasCmsRole role v =
    let
        x =
            roleValueToInt v
    in
        x == (Bitwise.and role x)


hasAnyCmsRole : CmsRole -> List CmsRoleValue -> Bool
hasAnyCmsRole role values =
    List.any (hasCmsRole role) values


type alias AuthData =
    { username : String
    , token : String
    , expire : Time.Time
    , role : CmsRole
    }


decodeAuthData : String -> Json.Decoder AuthData
decodeAuthData username =
    Json.map4 AuthData
        (Json.succeed username)
        (Json.field "token" Json.string)
        (Json.map Date.toTime (Json.field "expire" JsonExtra.date))
        (Json.field "role" Json.int)


type AuthError
    = NoSuchUser String
    | WrongPassword String
    | OtherError Http.Error


type AuthState
    = Unauthorized
    | AuthInProgress
    | AuthSuccess AuthData
    | AuthFailure AuthError


type alias LoginModel =
    { loginPath : String
    , title : String
    , maxIdleTime : Time.Time
    , lastActivityTime : Maybe Time.Time
    , username : Textfield.Model LoginMsg
    , password : Textfield.Model LoginMsg
    , auth : AuthState
    }


type alias ArticleDraft =
    { id : String
    , guid : String
    , fromVersion : String
    , createdAt : Date
    , createdBy : String
    , revisedAt : Date
    , revisedBy : String
    , headline : String
    , summary : String
    , content : String
    , tag : List String
    , note : String
    , lockedBy : String
    }


encodeArticleDraft : ArticleDraft -> JsonEncode.Value
encodeArticleDraft article =
    JsonEncode.object
        [ ( "guid", (JsonEncode.string article.guid) )
        , ( "headline", (JsonEncode.string article.headline) )
        , ( "summary", (JsonEncode.string article.summary) )
        , ( "content", (JsonEncode.string article.content) )
        , ( "tag", (JsonEncode.list (List.map JsonEncode.string article.tag)) )
        , ( "note", (JsonEncode.string article.note) )
        ]


decodeArticleDraft : Json.Decoder ArticleDraft
decodeArticleDraft =
    Json.succeed ArticleDraft
        |> JsonExtra.andMap (Json.field "id" Json.string)
        |> JsonExtra.andMap (Json.field "guid" Json.string)
        |> JsonExtra.andMap (Json.field "from_version" Json.string)
        |> JsonExtra.andMap (Json.field "created_at" JsonExtra.date)
        |> JsonExtra.andMap (Json.field "created_by" Json.string)
        |> JsonExtra.andMap (Json.field "revised_at" JsonExtra.date)
        |> JsonExtra.andMap (Json.field "revised_by" Json.string)
        |> JsonExtra.andMap (Json.field "headline" Json.string)
        |> JsonExtra.andMap (Json.field "summary" Json.string)
        |> JsonExtra.andMap (Json.field "content" Json.string)
        |> JsonExtra.andMap (Json.field "tag" (Json.list Json.string))
        |> JsonExtra.andMap (Json.field "note" Json.string)
        |> JsonExtra.andMap (Json.field "locked_by" Json.string)


type alias ArticleVersion =
    { id : String
    , guid : String
    , version : String
    , fromVersion : String
    , createdAt : Date
    , createdBy : String
    , revisedAt : Date
    , revisedBy : String
    , headline : String
    , summary : String
    , content : String
    , tag : List String
    , note : String
    }


decodeArticleVersion : Json.Decoder ArticleVersion
decodeArticleVersion =
    Json.succeed ArticleVersion
        |> JsonExtra.andMap (Json.field "id" Json.string)
        |> JsonExtra.andMap (Json.field "guid" Json.string)
        |> JsonExtra.andMap (Json.field "version" Json.string)
        |> JsonExtra.andMap (Json.field "from_version" Json.string)
        |> JsonExtra.andMap (Json.field "created_at" JsonExtra.date)
        |> JsonExtra.andMap (Json.field "created_by" Json.string)
        |> JsonExtra.andMap (Json.field "revised_at" JsonExtra.date)
        |> JsonExtra.andMap (Json.field "revised_by" Json.string)
        |> JsonExtra.andMap (Json.field "headline" Json.string)
        |> JsonExtra.andMap (Json.field "summary" Json.string)
        |> JsonExtra.andMap (Json.field "content" Json.string)
        |> JsonExtra.andMap (Json.field "tag" (Json.list Json.string))
        |> JsonExtra.andMap (Json.field "note" Json.string)


type alias CmsArticle =
    { guid : String
    , createdAt : Date
    , draft : Maybe ArticleDraft
    , versions : Maybe (List ArticleVersion)
    , publish : Maybe ArticleVersion
    }


decodeCmsArticle : Json.Decoder CmsArticle
decodeCmsArticle =
    Json.map5 CmsArticle
        (Json.field "guid" Json.string)
        (Json.field "created_at" JsonExtra.date)
        (Json.maybe (Json.field "draft" decodeArticleDraft))
        (Json.maybe (Json.field "versions" (Json.list decodeArticleVersion)))
        (Json.maybe (Json.field "publish" decodeArticleVersion))


type CmsArticleVersion
    = CmsArticleVersionOnly ArticleVersion
    | CmsArticleVersionEditing ArticleVersion ArticleDraft
    | CmsArticleVersionPublished ArticleVersion ArticleVersion
    | CmsArticleVersionEditingAndPublished ArticleVersion ArticleDraft ArticleVersion


getArticleVersion : CmsArticleVersion -> ArticleVersion
getArticleVersion a =
    case a of
        CmsArticleVersionOnly v ->
            v

        CmsArticleVersionEditing v _ ->
            v

        CmsArticleVersionPublished v _ ->
            v

        CmsArticleVersionEditingAndPublished v _ _ ->
            v


type CmsArticleViewData
    = CmsArticleViewDataDraft ArticleDraft
    | CmsArticleViewDataVersions (List CmsArticleVersion) CmsArticleVersion (List CmsArticleVersion)


type alias CmsArticleView =
    { guid : String
    , createdAt : Date
    , data : CmsArticleViewData
    }


cmsArticle2ViewData : CmsArticle -> Maybe CmsArticleViewData
cmsArticle2ViewData article =
    case article.versions of
        Nothing ->
            case article.draft of
                Nothing ->
                    Nothing

                Just draft ->
                    Just (CmsArticleViewDataDraft draft)

        Just versions ->
            let
                viewDataVersions =
                    List.map
                        (\v ->
                            case article.draft of
                                Nothing ->
                                    case article.publish of
                                        Nothing ->
                                            CmsArticleVersionOnly v

                                        Just publish ->
                                            CmsArticleVersionPublished v publish

                                Just draft ->
                                    case article.publish of
                                        Nothing ->
                                            CmsArticleVersionEditing v draft

                                        Just publish ->
                                            CmsArticleVersionEditingAndPublished v draft publish
                        )
                        versions

                head =
                    List.head viewDataVersions

                rest =
                    List.tail viewDataVersions
            in
                case head of
                    Nothing ->
                        Nothing

                    Just h ->
                        Just
                            (CmsArticleViewDataVersions
                                []
                                h
                                (case rest of
                                    Nothing ->
                                        []

                                    Just x ->
                                        x
                                )
                            )


type alias CmsArticleListResponse =
    { articles : List CmsArticle
    , cursorMark : Maybe String
    }


decodeCmsArticleListResponse : Json.Decoder CmsArticleListResponse
decodeCmsArticleListResponse =
    Json.map2 CmsArticleListResponse
        (Json.field "articles" (Json.list decodeCmsArticle))
        (Json.maybe (Json.field "cursor_mark" Json.string))


type alias CmsArticleListView =
    { articles : List CmsArticleView
    , cursorMark : Maybe String
    }


cmsArticleList2View : CmsArticleListResponse -> CmsArticleListView
cmsArticleList2View resp =
    { resp
        | articles =
            List.filterMap
                (\x ->
                    let
                        data =
                            cmsArticle2ViewData x
                    in
                        case data of
                            Nothing ->
                                Nothing

                            Just d ->
                                Just
                                    { guid = x.guid
                                    , createdAt = x.createdAt
                                    , data = d
                                    }
                )
                resp.articles
    }


type ArticleListState
    = ArticleListLoading
    | ArticleListLoadedSuccess
    | ArticleListLoadedFailure


type ArticleEditMode
    = ArticleEditCreate
    | ArticleEditEdit


type ArticlePageState
    = ArticlePageListing ArticleListState (Maybe AjaxError)
    | ArticlePageEditing ArticleEditMode ArticleDraft (Maybe AjaxError)


type alias ArticleModel =
    { state : ArticlePageState
    , articles : Result AjaxError CmsArticleListView
    , edited : Bool
    , headline : Textfield.Model ArticleMsg
    , summary : Textfield.Model ArticleMsg
    , content : Textfield.Model ArticleMsg
    }


type alias AjaxError =
    { path : String
    , error : Http.Error
    }


type alias Model =
    { lockUI : Bool
    , login : LoginModel
    , article : ArticleModel
    }



-- Msgs


type LoginMsg
    = LoginUsername (Textfield.Msg LoginMsg)
    | LoginPassword (Textfield.Msg LoginMsg)
    | LoginUsernameInput String
    | LoginPasswordInput String
    | LoginRequest (Maybe String)
    | LoginResponse (Result Http.Error AuthData)
    | LoginUserActivity
    | LoginLogUserActivity Time.Time
    | LoginCheckIdleTimeout Time.Time
    | LoginLogout
    | LoginNoop


type ArticleMsg
    = ArticleHeadline (Textfield.Msg ArticleMsg)
    | ArticleSummary (Textfield.Msg ArticleMsg)
    | ArticleContent (Textfield.Msg ArticleMsg)
    | ArticleNewRequest ArticleListState
    | ArticleNewResponse ArticleListState String (Result Http.Error ArticleDraft)
    | ArticleDiscardRequest AuthData ArticleEditMode ArticleDraft
    | ArticleDiscardResponse ArticleEditMode ArticleDraft String (Result Http.Error String)
    | ArticleSaveRequest AuthData ArticleEditMode ArticleDraft
    | ArticleSaveResponse ArticleEditMode ArticleDraft String (Result Http.Error String)
    | ArticleSubmitRequest AuthData ArticleEditMode ArticleDraft
    | ArticleSubmitResponse ArticleEditMode ArticleDraft String (Result Http.Error String)
    | ArticleClose
    | ArticleReload
    | ArticleListLoaded String (Result Http.Error CmsArticleListResponse)
    | ArticleVersionSelect String String
    | ArticleOpenDraft ArticleDraft
    | ArticleNoop


type Msg
    = Noop
    | Login LoginMsg
    | Article ArticleMsg
