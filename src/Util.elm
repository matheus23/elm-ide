module Util exposing (..)

import Element exposing (Element)
import Element.Attributes exposing (..)
import Focus exposing (..)
import Html exposing (Html)


type alias FieldSetter record field =
    Setter record record field field


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


orTry : Maybe a -> Maybe a -> Maybe a
orTry maybe1 maybe2 =
    case maybe1 of
        Just x ->
            Just x

        Nothing ->
            maybe2


getIndex : Int -> List a -> Maybe a
getIndex i =
    let
        justOnEqual index =
            if index == i then
                Just
            else
                always Nothing
    in
    List.foldr orTry Nothing << List.indexedMap justOnEqual


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


styledText : style -> String -> Element style variation msg
styledText style text =
    Element.el style [] (Element.text text)


styledTextAttr : style -> List (Element.Attribute variation msg) -> String -> Element style variation msg
styledTextAttr style attributes text =
    Element.el style attributes (Element.text text)


hcenter : style -> Element style variation msg -> Element style variation msg
hcenter style elem =
    Element.row style [ center ] [ elem ]


vcenter : style -> Element style variation msg -> Element style variation msg
vcenter style elem =
    Element.column style [ center ] [ elem ]


centeredElement : style -> Element style variation msg -> Element style variation msg
centeredElement style elem =
    hcenter style (vcenter style elem)


isJust : Maybe a -> Bool
isJust =
    Maybe.withDefault False << Maybe.map (always True)
