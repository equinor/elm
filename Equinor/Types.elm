module Equinor.Types exposing (..)


type alias Size =
    { width : Float
    , height : Float
    }

type WebData a
    = NotLoaded
    | Loading String (Maybe a)
    | DataError String (Maybe a)
    | Loaded String a