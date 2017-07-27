module Main exposing (..)

import Edit.Function as Function
import Edit.Type as Type
import Element exposing (Element)
import Html exposing (Html)
import Html5.DragDrop as DragDrop
import Styles exposing (..)


init : ( Function.Model, Cmd msg )
init =
    { name = Function.name "append"
    , args =
        [ ( Function.argName "arg1", Type.hole )
        , ( Function.argName "arg2", Type.hole )
        , ( Function.argName "record"
          , Type.recordType [ ( "key", Type.Int ) ]
          )
        ]
    , dragDrop = DragDrop.init
    }
        ! []


updateCmd : Function.Msg -> Function.Model -> ( Function.Model, Cmd msg )
updateCmd msg model =
    Function.update msg model ! []


viewElement : Function.Model -> Html Function.Msg
viewElement model =
    Element.layout stylesheet (Function.view model)


main : Program Never Function.Model Function.Msg
main =
    Html.program
        { init = init
        , update = updateCmd
        , subscriptions = always Sub.none
        , view = viewElement
        }
