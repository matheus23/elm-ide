module DividedList exposing (..)

import Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Keyed as Keyed


viewKeyed :
    style
    -> List (Element.Attribute variation msg)
    -> (Int -> Int -> ( String, Element style variation msg ))
    -> List ( String, Element style variation msg )
    -> Element style variation msg
viewKeyed style attributes viewDivider elements =
    let
        numElements =
            List.length elements

        addDivider index element =
            [ element, viewDivider (index + 1) (numElements - index - 1) ]
    in
    Keyed.row style
        attributes
        (viewDivider 0 numElements :: List.concat (List.indexedMap addDivider elements))
