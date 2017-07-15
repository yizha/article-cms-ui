module Global exposing (Event(..))


type Event
    = None
    | Login String String
    | Logout
