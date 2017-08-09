module Main exposing (..)

import DragAndDrop.ReorderList as ReorderList
import Edit.Arg as Arg
import Edit.Function as Function
import Edit.Type as Type
import Element exposing (Element)
import Html exposing (Html)
import Styles exposing (..)


init : ( Function.Model, Cmd msg )
init =
    { name = Function.name "append"
    , args =
        ReorderList.init
            [ Arg.init "arg1" Type.hole
            , Arg.init "arg2" Type.hole
            , Arg.init "record"
                (Type.recordType True
                    [ ( "key", Type.int )
                    , ( "key2", Type.hole )
                    ]
                )
            ]
    }
        ! []


updateCmd : Function.Msg -> Function.Model -> ( Function.Model, Cmd msg )
updateCmd msg model =
    Function.update msg model ! []


viewElement : Function.Model -> Html Function.Msg
viewElement model =
    Element.layout stylesheet (Function.view model)


subscriptions : Function.Model -> Sub Function.Msg
subscriptions =
    Function.subscriptions


main : Program Never Function.Model Function.Msg
main =
    Html.program
        { init = init
        , update = updateCmd
        , subscriptions = subscriptions
        , view = viewElement
        }
