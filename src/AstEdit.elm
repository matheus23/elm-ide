module AstEdit exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Styles exposing (..)


type alias Name =
    String


type alias Var =
    String


type Type
    = Int


type alias Function =
    { name : ContentEditable.Model
    , args : List ( Var, Type )
    }


viewFunction : Function -> Element Styles Variations Function
viewFunction function =
    Element.row StdStyle
        []
        [ viewName function ]


viewName : Function -> Element Styles Variations Function
viewName model =
    let
        updateName str =
            { model | name = str }
    in
    Element.map updateName (ContentEditable.tightView VarInput model.name)
