module Defs exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Http
import Json.Decode as Json
import Json.Decode.Extra as JsonExtra
import MDC.Textfield as Textfield


-- Login Component


type alias CmsRole =
    Int


type alias AuthData =
    { token : String
    , expire : Time.Time
    , role : CmsRole
    }


decodeAuthData : Json.Decoder AuthData
decodeAuthData =
    Json.map3 AuthData
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



-- Article Component


type alias ArticleData =
    { id : String
    , guid : String
    , version : String
    , createdAt : Date
    , createdBy : String
    , headline : Maybe String
    , summary : Maybe String
    , content : Maybe String
    , tag : Maybe (List String)
    , note : Maybe String
    , revisedAt : Maybe Date
    , revisedBy : Maybe String
    , fromVersion : Maybe String
    , lockedBy : Maybe String
    }


decodeArticleData : Json.Decoder ArticleData
decodeArticleData =
    Json.succeed ArticleData
        |> JsonExtra.andMap (Json.field "id" Json.string)
        |> JsonExtra.andMap (Json.field "guid" Json.string)
        |> JsonExtra.andMap (Json.field "version" Json.string)
        |> JsonExtra.andMap (Json.field "created_at" JsonExtra.date)
        |> JsonExtra.andMap (Json.field "created_by" Json.string)
        |> JsonExtra.andMap (Json.field "headline" (Json.maybe Json.string))
        |> JsonExtra.andMap (Json.field "summary" (Json.maybe Json.string))
        |> JsonExtra.andMap (Json.field "content" (Json.maybe Json.string))
        |> JsonExtra.andMap (Json.field "tag" (Json.maybe (Json.list Json.string)))
        |> JsonExtra.andMap (Json.field "note" (Json.maybe Json.string))
        |> JsonExtra.andMap (Json.field "revised_at" (Json.maybe JsonExtra.date))
        |> JsonExtra.andMap (Json.field "revised_by" (Json.maybe Json.string))
        |> JsonExtra.andMap (Json.field "from_version" (Json.maybe Json.string))
        |> JsonExtra.andMap (Json.field "locked_by" (Json.maybe Json.string))


type alias ArticleModel =
    { articles : List ArticleData }



-- Main


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
    = ArticleLogin
    | ArticleLogout
    | ArticleNoop


type Msg
    = Noop
    | Login LoginMsg
    | Article ArticleMsg
