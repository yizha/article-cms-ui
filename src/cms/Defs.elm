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


hasCmsRoles : CmsRole -> List CmsRoleValue -> Bool
hasCmsRoles role values =
    List.all (hasCmsRole role) values


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
    { draft : Maybe ArticleDraft
    , versions : Maybe (List ArticleVersion)
    , publish : Maybe ArticleVersion
    }


decodeCmsArticle : Json.Decoder CmsArticle
decodeCmsArticle =
    Json.map3 CmsArticle
        (Json.maybe (Json.field "draft" decodeArticleDraft))
        (Json.maybe (Json.field "versions" (Json.list decodeArticleVersion)))
        (Json.maybe (Json.field "publish" decodeArticleVersion))


type alias CmsArticleListResponse =
    { articles : List CmsArticle
    , cursorMark : Maybe String
    }


decodeCmsArticleListResponse : Json.Decoder CmsArticleListResponse
decodeCmsArticleListResponse =
    Json.map2 CmsArticleListResponse
        (Json.field "articles" (Json.list decodeCmsArticle))
        (Json.maybe (Json.field "cursor_mark" Json.string))


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
    , articles : Result AjaxError CmsArticleListResponse
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
    | ArticleNoop


type Msg
    = Noop
    | Login LoginMsg
    | Article ArticleMsg
