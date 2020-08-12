module Equinor.Palette exposing (alphaEnergyRed, scaled,unit,iconSize,alphaGreen, alphaMistBlue, alphaMossGreen, highlight,alphaMossGreenHex, kv,alphaSlateBlue, scaledInt,alphaWhite, alphaYellow, black, blue, buttonShadow, combination, darkGrey, darkGreyHex, energeticRedOnEnergeticRed8, energeticRedOnWhite, energyRed, energyRed8, green, grey, header, heritageRed, lichenGreen, lightGrey, mainMenu, mistBlue, mistBlueOnAlphaMossGreen, mistBlueOnAlphaSlate, mistBlueOnMossGreen, mistBlueOnSlate, mossGreen, mossGreenOnAlphaWhite, mossGreenOnWhite, red, redHex, slateBlue, slateBlueHex, slateBlueOnWhite, slateOnEnergeticRed8, slateOnMistBlue, spruceWood, spruceWood8, stateButton, white, whiteHex, whiteOnEnergeticRed, whiteOnGreen, whiteOnMistBlue, whiteOnMossGreen, whiteOnSlateBlue, yellow, yellowDisabled, yellowHex)




import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Equinor.Device as Device exposing (DeviceDetails)


-- Custom


buttonShadow =
    { color = darkGrey
    , offset = ( 1, 1 )
    , blur = 10
    , size = 1
    }


mainMenu =
    { steps = [ white ]
    , angle = 3.14
    }


yellow =
    rgb255 251 202 54


yellowHex =
    "#fbca36"


yellowDisabled =
    rgb255 255 221 176


alphaYellow =
    rgba255 251 202 54 1


red =
    rgb255 255 59 59


redHex =
    "#ff3b3b"


green =
    rgb255 75 183 72


blue =
    rgb255 95 192 220


alphaGreen =
    rgba255 75 183 72 1


lightGrey =
    rgb 0.9 0.9 0.9



-- Primary


energyRed =
    rgb255 255 18 67


energyRed8 =
    rgb255
        255
        236
        240


alphaEnergyRed =
    rgba255 255 18 67 0.2


white =
    rgb 1 1 1


whiteHex =
    "#ffffff"


alphaWhite =
    rgba 1 1 1 0.7


black =
    rgb255 51 51 51


grey =
    rgb255 217 217 217



-- Supporting


slateBlue =
    rgb255 36 55 70


slateBlueHex =
    "#243746"


alphaSlateBlue alpha =
    rgba255 36 55 70 alpha


heritageRed =
    rgb255 125 0 35


mossGreen =
    rgb255 0 112 121


alphaMossGreen =
    rgba255 0 112 121 0.7


alphaMossGreenHex =
    "#007079"


mistBlue =
    rgb255 213 234 244


alphaMistBlue alpha =
    rgba255 213 234 244 alpha


spruceWood =
    rgb255 255 231 214


spruceWood8 =
    rgb255 255 243 235


lichenGreen =
    rgb255 230 250 236


combination fontColor backgroundColor attributes =
    attributes
        ++ [ Font.color fontColor
           , Background.color backgroundColor
           ]


whiteOnGreen =
    combination white green


whiteOnEnergeticRed =
    combination white energyRed


energeticRedOnWhite =
    combination energyRed white


energeticRedOnEnergeticRed8 =
    combination energyRed energyRed8


slateOnMistBlue =
    combination slateBlue mistBlue


slateOnEnergeticRed8 =
    combination slateBlue energyRed8


mistBlueOnSlate =
    combination mistBlue slateBlue


mistBlueOnAlphaSlate =
    combination mistBlue (alphaSlateBlue 0.7)


mistBlueOnMossGreen =
    combination mistBlue mossGreen


mistBlueOnAlphaMossGreen =
    combination mistBlue alphaMossGreen


slateBlueOnWhite =
    combination slateBlue white


whiteOnSlateBlue =
    combination white slateBlue


whiteOnMistBlue =
    combination white mistBlue


whiteOnMossGreen =
    combination white mossGreen


mossGreenOnWhite =
    combination mossGreen white


mossGreenOnAlphaWhite =
    combination mossGreen alphaWhite


stateButton isActive attributes =
    let
        fixed =
            [ Border.color slateBlue ]
    in
    if isActive then
        fixed
            ++ attributes
            |> whiteOnSlateBlue

    else
        fixed
            ++ [ mouseOver
                    [ Font.color slateBlue
                    , Background.color mistBlue
                    ]
               ]
            ++ attributes
            |> slateBlueOnWhite


header =
    { angle = 3.14
    , steps =
        [ black, darkGrey ]
    }


darkGrey =
    rgb255 128 128 128


darkGreyHex =
    "#808080"


iconSize : Float -> Float
iconSize size =
    size * 3


scaled : Float -> (Int -> Float)
scaled size =
    modular size 1.15


scaledInt : Float -> (Int -> Int)
scaledInt size =
    scaled size >> round


unit : DeviceDetails -> Float
unit device =
    Device.userScaleToFloat device.userScale
        * (case device.class.class of
            Phone ->
                15

            Tablet ->
                17

            Desktop ->
                16

            BigDesktop ->
                16
          )


kv size heading value subValue =
    let
        dontRender =
            value == ""
    in
    if dontRender then
        none

    else
        column
            [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
            , width fill
            , Font.size <| round size

            --, Border.dashed
            , Border.color mistBlue
            ]
            [ el
                [ Font.color mossGreen
                , Font.bold
                , Font.size <| scaledInt size -3
                , width fill
                ]
                (text heading)
            , wrappedRow [ width fill ]
                [ paragraph [] [ text value ]
                , if subValue == "" then
                    none

                  else
                    paragraph [ Font.size <| scaledInt size -2 ] [ text subValue ]
                ]
            ]

highlight maybeHighlight txt =
    case maybeHighlight of
        Nothing ->
            [ text txt ]

        Just highLight ->
            let
                indexes =
                    highLightIndexes txt (String.words highLight) []
            in
            applyHighLight txt indexes []


applyHighLight : String -> List ( Int, Int ) -> List (Element msg) -> List (Element msg)
applyHighLight str indexes acc =
    case indexes of
        [] ->
            text str
                :: acc
                |> List.reverse

        ( start, length ) :: rest ->
            let
                normalPart =
                    text (String.left start str)

                highlightPart =
                    el [ Background.color (rgba 1 1 0 0.5) ] (text (String.slice start (start + length) str))

                nextStr =
                    String.dropLeft (start + length) str
            in
            applyHighLight nextStr rest (highlightPart :: normalPart :: acc)


highLightIndexes : String -> List String -> List ( Int, Int ) -> List ( Int, Int )
highLightIndexes str searchTerms acc =
    case searchTerms of
        [] ->
            List.reverse acc

        first :: rest ->
            case List.head (String.indexes (String.toUpper first) (String.toUpper str)) of
                Just index ->
                    highLightIndexes (String.dropLeft (index + String.length first) str) rest (( index, String.length first ) :: acc)

                Nothing ->
                    acc

