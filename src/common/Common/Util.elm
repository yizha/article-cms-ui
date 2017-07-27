module Common.Util
    exposing
        ( ajaxErrorString
        , httpErrorString
        , httpGetJson
        , httpGetString
        , httpPostJson
        )

import Time
import Http
import Json.Encode as JsonEncode
import Json.Decode as Json
import Defs exposing (AjaxError)


{-
   maybeStringValue : Maybe String -> String
   maybeStringValue =
       maybeStringValueWithDefault ""


   maybeStringValueWithDefault : String -> Maybe String -> String
   maybeStringValueWithDefault default ms =
       case ms of
           Nothing ->
               default

           Just s ->
               s
-}


ajaxErrorString : AjaxError -> String
ajaxErrorString err =
    httpErrorString err.path err.error


httpErrorString : String -> Http.Error -> String
httpErrorString uri err =
    case err of
        Http.BadUrl desc ->
            "[" ++ uri ++ "] BadUrl: " ++ desc

        Http.Timeout ->
            "[" ++ uri ++ "] Timeout!"

        Http.NetworkError ->
            "[" ++ uri ++ "] NetworkError"

        Http.BadStatus resp ->
            "[" ++ uri ++ "] BadStatus! Code: " ++ (toString resp.status.code) ++ ", Body: " ++ resp.body

        Http.BadPayload desc resp ->
            "[" ++ uri ++ "] BadPayload: " ++ desc


httpGetJson : String -> String -> Json.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
httpGetJson path token decoder msgGenerator =
    let
        req =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" token ]
                , url = path
                , body = Http.emptyBody
                , expect = Http.expectJson decoder
                , timeout = Just (Time.second * 5)
                , withCredentials = False
                }
    in
        Http.send msgGenerator req


httpGetString : String -> String -> (Result Http.Error String -> msg) -> Cmd msg
httpGetString path token msgGenerator =
    let
        req =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" token ]
                , url = path
                , body = Http.emptyBody
                , expect = Http.expectString
                , timeout = Just (Time.second * 5)
                , withCredentials = False
                }
    in
        Http.send msgGenerator req


httpPostJson : String -> String -> JsonEncode.Value -> (Result Http.Error String -> msg) -> Cmd msg
httpPostJson path token jsonValue msgGenerator =
    let
        req =
            Http.request
                { method = "POST"
                , headers = [ Http.header "X-Auth-Token" token ]
                , url = path
                , body = Http.jsonBody jsonValue
                , expect = Http.expectString
                , timeout = Just (Time.second * 5)
                , withCredentials = False
                }
    in
        Http.send msgGenerator req
