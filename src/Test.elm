module Test exposing (..)

import Element exposing (Element)
import Element.Events as Events
import Html exposing (Html)
import Styles exposing (..)
import Util


type Model
    = Bool
    | Int


type alias Msg =
    Model


updateCmd : Msg -> Model -> ( Model, Cmd msg )
updateCmd msg model =
    msg ! []


viewElement : Model -> Html Msg
viewElement model =
    Element.layout stylesheet (view model)


view : Model -> Element Styles Variations Msg
view model =
    case model of
        Bool ->
            Element.column NoStyle
                [ Events.onClick Int ]
                [ Util.styledText Identifier "Bool"
                , Util.styledText Identifier "Click to change"
                ]

        Int ->
            Util.styledText Identifier "Int"


main : Program Never Model Msg
main =
    Html.program
        { init = Bool ! []
        , update = updateCmd
        , subscriptions = always Sub.none
        , view = viewElement
        }
