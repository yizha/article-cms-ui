module Common.Debug exposing (debug)

import Debug


debug : String -> a -> a
debug =
    Debug.log
