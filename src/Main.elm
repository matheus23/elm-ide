module Main exposing (..)

import Edit.Expression as Expression
import Edit.Function as Function
import Edit.Playground as Playground
import Edit.Type as Type
import Styles exposing (..)


init : ( Playground.Model, Cmd msg )
init =
    Playground.init
        (Function.init "append"
            [ ( "arg1", Type.hole )
            , ( "arg2", Type.hole )
            , ( "record"
              , Type.recordType False
                    [ ( "key", Type.int )
                    , ( "key2", Type.hole )
                    ]
              )
            ]
            (Expression.application Expression.hole [ Expression.intLiteral 15, Expression.intLiteral 42 ])
        )
        ! []


updateCmd : Playground.Msg -> Playground.Model -> ( Playground.Model, Cmd Playground.Msg )
updateCmd msg model =
    Playground.update msg model


main : Program Never Playground.Model Playground.Msg
main =
    styleElementsProgram
        { init = init
        , update = updateCmd
        , subscriptions = Playground.subscriptions
        , view = Playground.view
        }
