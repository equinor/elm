module Equinor.Data.Procosys.Status exposing (Status(..), encoder,decoder,toHexColor,fromString,toString,toColor,sortOrder,worst,worstWithDefault)
import Json.Encode as E
import Json.Decode as D
import Equinor.Palette as Palette
import Element exposing (Color)

type Status
    = OK
    | PB
    | PA
    | OS



encoder : Status -> E.Value
encoder status =
    E.string <|
        case status of
            OK ->
                "OK"

            PA ->
                "PA"

            PB ->
                "PB"

            OS ->
                "OS"


decoder : D.Decoder Status
decoder =
    D.oneOf
        [ D.string |> D.andThen (fromString >> D.succeed)
        , D.null OS
        ]


fromString : String -> Status
fromString str =
    case str of
        "OK" ->
            OK

        "PA" ->
            PA

        "PB" ->
            PB

        _ ->
            OS

toString : Status -> String
toString status =
    case status of
        OS ->
            "OS"

        PA ->
            "PA"

        PB ->
            "PB"

        OK ->
            "OK"





toColor : Maybe Status -> Color
toColor maybeStatus =
    case maybeStatus of
        Nothing ->
            Palette.darkGrey

        Just status ->
            case status of
                OS ->
                    Palette.white

                PB ->
                    Palette.yellow

                PA ->
                    Palette.red

                OK ->
                    Palette.alphaMossGreen


toHexColor : Status -> String
toHexColor status =
    case status of
        OS ->
            Palette.whiteHex

        PB ->
            Palette.yellowHex

        PA ->
            Palette.redHex

        OK ->
            Palette.alphaMossGreenHex




worst : List Status -> Maybe Status
worst list =
    worst_ list Nothing

worst_ : List Status -> Maybe Status -> Maybe Status
worst_ list current =
    case list of
        [] ->
            current

        first :: rest ->
            case first of
                OS ->
                    Just OS

                PA ->
                    worst_ rest
                        (if current == Just OS then
                            current

                         else
                            Just PA
                        )

                PB ->
                    worst_ rest
                        (if current == Just PA || current == Just OS then
                            current

                         else
                            Just PB
                        )

                OK ->
                    worst_ rest
                        (if current == Just PA || current == Just PB || current == Just OS then
                            current

                         else
                            Just OK
                        )


worstWithDefault : Status -> List Status -> Status
worstWithDefault defaultStatus list =
    case worst (defaultStatus :: list) of
        Just nextStatus ->
            nextStatus

        Nothing ->
            defaultStatus





sortOrder : Status -> Int
sortOrder status =
    case status of
        OS ->
            1

        PB ->
            3

        PA ->
            2

        OK ->
            4
