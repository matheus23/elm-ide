module Util exposing (..)

import Element exposing (Element)
import Element.Attributes exposing (..)
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Html exposing (Html)
import Murmur3


addKey : model -> a -> ( String, a )
addKey model sth =
    ( stringHash model, sth )


stringHash : a -> String
stringHash =
    toString >> Murmur3.hashString 1319285 >> toString


equalsMaybe : a -> Maybe a -> Bool
equalsMaybe a maybe =
    Maybe.withDefault False (Maybe.map (\b -> a == b) maybe)


zip : List a -> List b -> List ( a, b )
zip =
    let
        tuple2 x y =
            ( x, y )
    in
    List.map2 tuple2


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


moveByIndex : Int -> Int -> List a -> List a
moveByIndex fromIndex toIndex list =
    Maybe.withDefault list
        (Maybe.map
            (\elem -> applyMove fromIndex toIndex elem list)
            (getIndex fromIndex list)
        )


applyMove : Int -> Int -> a -> List a -> List a
applyMove dragIndex dropIndex draggedElem list =
    let
        removeIndex i =
            Focus.indexConcat i .= []

        insertIndex i toInsert list =
            if i == List.length list then
                list ++ [ toInsert ]
            else
                list & Focus.indexConcat i $= (\e -> [ toInsert, e ])
    in
    -- Dropping above or below the dragged element has no effect
    if dragIndex == dropIndex || dragIndex + 1 == dropIndex then
        list
        -- Be careful to not alter indices by removing or inserting an element
    else if dragIndex > dropIndex then
        list |> removeIndex dragIndex |> insertIndex dropIndex draggedElem
    else
        list |> insertIndex dropIndex draggedElem |> removeIndex dragIndex


styledText : style -> String -> Element style variation msg
styledText style text =
    Element.el style [] (Element.text text)


styledTextAttr : style -> List (Element.Attribute variation msg) -> String -> Element style variation msg
styledTextAttr style attributes text =
    Element.el style attributes (Element.text text)


wrapStyles : List style -> Element style variation msg -> Element style variation msg
wrapStyles styles elem =
    List.foldr (\style -> Element.el style []) elem styles


isJust : Maybe a -> Bool
isJust =
    Maybe.withDefault False << Maybe.map (always True)


replaceNothingIfEqual : a -> Maybe a -> Maybe a
replaceNothingIfEqual value maybe =
    case maybe of
        Just sth ->
            if value == sth then
                Nothing
            else
                Just sth

        Nothing ->
            Nothing


replaceJustIfNothing : Maybe a -> Maybe a -> Maybe a
replaceJustIfNothing value maybe =
    case maybe of
        Nothing ->
            value

        _ ->
            maybe


appendWhen : Bool -> List a -> List a -> List a
appendWhen predicate toAppend list =
    if predicate then
        list ++ toAppend
    else
        list


onMaybe : (a -> b -> b) -> Maybe a -> b -> b
onMaybe update maybeMsg =
    case maybeMsg of
        Just msg ->
            update msg

        Nothing ->
            identity
