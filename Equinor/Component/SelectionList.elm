module Equinor.Component.SelectionList exposing (defaultSelectItem, loadingMessage, selectionList, webDataSelectionList)

import Equinor.Palette as Palette exposing (scaledInt)
import Equinor.Types exposing (WebData(..))


import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Keyed as Keyed
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D



webDataSelectionList : (a -> Element msg) -> WebData a -> Element msg
webDataSelectionList viewFunction webData =
    case webData of
        NotLoaded ->
            msgBox (text "Initializing")

        Loading str _ ->
            loadingMessage str

        DataError err _ ->
            msgBox (text err)

        Loaded _ items ->
            viewFunction items


msgBox : Element msg -> Element msg
msgBox content =
    el
        [ centerX
        , Border.rounded 4
        , Border.color Palette.slateBlue
        , padding 4
        , centerY
        , Background.color Palette.white
        ]
    <|
        content


loadingMessage str =
    el
        [ centerX
        , centerY
        , clip
        , height (px 100)
        , width (px 100)
        , behindContent <| fancySpinner
        ]
    <|
        msgBox (text str)


fancySpinner =
    html <|
        H.div [ HA.class "fancy-spinner" ]
            [ H.div [ HA.class "ring" ] []
            , H.div [ HA.class "ring" ] []
            ]


selectionList : (a -> ( String, Element msg )) -> List a -> Element msg
selectionList viewFunction items =
    Keyed.column
        [ width fill
        , height fill

        --, paddingXY 0 4
        , scrollbars
        , htmlAttribute <|
            HA.style "-webkit-overflow-scrolling" "touch"
        , spacing -1
        ]
        (items
            |> List.map viewFunction
        )


defaultSelectItem : Float -> msg -> Element msg -> Element msg
defaultSelectItem size msg element =
    el
        [ Background.color Palette.white
        , width fill
        , padding (scaledInt size 5)
        , Border.width 1
        , Border.color Palette.mistBlue
        , mouseOver
            [ Background.color Palette.mistBlue ]
        , pointer
        , onClick msg
        ]
        element


onClick : msg -> Element.Attribute msg
onClick msg =
    HE.custom "click"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = False
            }
        )
        |> htmlAttribute
