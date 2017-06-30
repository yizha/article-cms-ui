module Common.Util exposing (httpErrorString)

import Http


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
