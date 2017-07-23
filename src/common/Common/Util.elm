module Common.Util exposing (httpErrorString)

import Time
import Http
import Json.Decode as Json


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
