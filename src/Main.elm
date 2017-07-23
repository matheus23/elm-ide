module Main exposing (..)

import AstEdit exposing (..)
import ContentEditable
import Element as Element exposing (Element)
import Html exposing (Html)
import Styles exposing (..)


type alias Model =
    Function


type alias Msg =
    Function


init : ( Model, Cmd msg )
init =
    { name = ContentEditable.create "append", args = [] } ! []


update : Msg -> Model -> ( Model, Cmd msg )
update newModel model =
    newModel ! []


view : Model -> Element Styles Variations Msg
view model =
    Element.column StdStyle
        []
        [ Element.textArea TextArea [] "lol"
        , viewFunction model
        ]


viewElement : Model -> Html Msg
viewElement model =
    Element.layout stylesheet (view model)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = viewElement
        }
