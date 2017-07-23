module AstEdit exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Html exposing (Html)
import Styles exposing (..)


type alias Name =
    String


type alias Var =
    String


type Type
    = Int


type alias Function =
    { name : Name
    , args : List ( Var, Type )
    }


viewFunction : Function -> Element Styles Variations Function
viewFunction function =
    Element.row StdStyle
        []
        [ viewName function ]


viewName : Function -> Element Styles Variations Function
viewName model =
    Element.inputText VarInput [] model.name
