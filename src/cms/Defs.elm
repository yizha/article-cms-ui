module Defs
    exposing
        ( Model
        , Msg(..)
        )

import Dict exposing (Dict)
import Time exposing (Time)
import Http
import MDC.Textfield as Textfield
import MDC.Checkbox as Checkbox
import Page.Login as LoginPage


type alias Model =
    { login : LoginPage.Model
    }


type Msg
    = Noop
    | Login LoginPage.Msg
