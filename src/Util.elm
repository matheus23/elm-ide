module Util exposing (..)

import Element exposing (Element)
import Element.Attributes exposing (..)
import Focus exposing (..)
import Html exposing (Html)


fst : Setter ( a, c ) ( b, c ) a b
fst f ( x, y ) =
    ( f x, y )


snd : Setter ( c, a ) ( c, b ) a b
snd f ( x, y ) =
    ( x, f y )


index : Int -> Setter (List a) (List a) a a
index index f list =
    let
        applyAtIndex i elem =
            if i == index then
                f elem
            else
                elem
    in
    List.indexedMap applyAtIndex list


tightHtml : (msg -> model -> model) -> (model -> Html msg) -> model -> Html model
tightHtml update view model =
    let
        applyUpdate msg =
            update msg model
    in
    Html.map applyUpdate (view model)


tightElement :
    (msg -> model -> model)
    -> (model -> Element style variation msg)
    -> model
    -> Element style variation model
tightElement update view model =
    let
        applyUpdate msg =
            update msg model
    in
    Element.map applyUpdate (view model)


replaceIndex : Int -> a -> List a -> List a
replaceIndex replacementIndex replacement list =
    let
        maybeReplace index element =
            if index == replacementIndex then
                replacement
            else
                element
    in
    List.indexedMap maybeReplace list


elementList :
    (model -> Element style variation model)
    -> List model
    -> List (Element style variation (List model))
elementList view models =
    let
        updateModel index model =
            replaceIndex index model models

        viewModel index model =
            Element.map (updateModel index) (view model)
    in
    List.indexedMap viewModel models


styledText : style -> String -> Element style variation model
styledText style text =
    Element.el style [] (Element.text text)


hcenter : style -> Element style variation model -> Element style variation model
hcenter style elem =
    Element.row style [ center ] [ elem ]


vcenter : style -> Element style variation model -> Element style variation model
vcenter style elem =
    Element.column style [ center ] [ elem ]


centeredElement : style -> Element style variation model -> Element style variation model
centeredElement style elem =
    hcenter style (vcenter style elem)
