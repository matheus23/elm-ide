module Main exposing (..)

import DragAndDrop.ReorderList as ReorderList
import Edit.Arg as Arg
import Edit.Expression as Expression
import Edit.Function as Function
import Edit.Type as Type
import Element exposing (Element)
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
    , body =
        Expression.application Expression.hole [ Expression.intLiteral 15, Expression.intLiteral 42 ]
    }
        ! []


updateCmd : Function.Msg -> Function.Model -> ( Function.Model, Cmd msg )
updateCmd msg model =
    Function.update msg model ! []


main : Program Never Function.Model Function.Msg
main =
    styleElementsProgram
        { init = init
        , update = updateCmd
        , subscriptions = Function.subscriptions
        , view = Function.view
        }
