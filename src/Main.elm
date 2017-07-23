module Main exposing (..)

import AstEdit exposing (..)
import Element exposing (Element)
import Element.Attributes as Element
import Html exposing (Html)
import Styles exposing (..)
import Util


init : ( Model, Cmd msg )
init =
    { name = name "append"
    , args = [ ( var "arg1", Int ), ( var "arg2", Int ) ]
    }
        ! []


updateCmd : Msg -> Model -> ( Model, Cmd msg )
updateCmd msg model =
    update msg model ! []


viewElement : Model -> Html Msg
viewElement model =
    Element.layout stylesheet (view model)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = updateCmd
        , subscriptions = always Sub.none
        , view = viewElement
        }
