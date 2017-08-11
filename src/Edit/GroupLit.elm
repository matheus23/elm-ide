module Edit.GroupLit exposing (..)

import DividedList
import DragAndDrop
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import PlainElement exposing (PlainAttribute, PlainElement)
import Styles exposing (..)


type alias Model value =
    { elements : List value
    , oneline : Bool
    , dragModel : DragAndDrop.Model Int Int
    }


type Msg innerMsg
    = UpdateIndex Int innerMsg
    | FlipOneLine


type alias Settings =
    { prefixFor : List (PlainAttribute Variations) -> Int -> PlainElement Styles Variations
    , suffix : PlainElement Styles Variations
    }


init : Bool -> List value -> Model value
init oneline elements =
    { oneline = oneline
    , elements = elements
    , dragModel = DragAndDrop.init
    }



-- Update


update :
    (valueMsg -> value -> List value)
    -> Msg valueMsg
    -> Model value
    -> Model value
update updateValue msg model =
    case msg of
        UpdateIndex index valueMsg ->
            model & elements => Focus.indexConcat index $= updateValue valueMsg

        FlipOneLine ->
            model & oneline $= not


subscriptions : (value -> Sub innerMsg) -> Model value -> Sub (Msg innerMsg)
subscriptions innerSub model =
    let
        subscriptionForIndex index element =
            Sub.map (UpdateIndex index) (innerSub element)
    in
    Sub.batch
        [ Sub.batch (List.indexedMap subscriptionForIndex model.elements)
        ]



-- View


view :
    Settings
    -> (innerModel -> Element Styles Variations msg)
    -> Model innerModel
    -> Element Styles Variations (Msg msg)
view settings viewInner model =
    let
        viewElement index innerModel =
            if model.oneline then
                Element.map (UpdateIndex index) (viewInner innerModel)
            else
                Element.row NoStyle
                    []
                    [ PlainElement.view (settings.prefixFor [ paddingRight 10 ] index)
                    , Element.map (UpdateIndex index) (viewInner innerModel)
                    ]

        elementsRendered =
            if model.oneline then
                List.indexedMap viewElement model.elements
            else
                List.indexedMap viewElement model.elements ++ [ PlainElement.view settings.suffix ]
    in
    render settings model [ Events.onDoubleClick FlipOneLine ] elementsRendered


render :
    Settings
    -> Model innerModel
    -> List (Element.Attribute Variations msg)
    -> List (Element Styles Variations msg)
    -> Element Styles Variations msg
render settings model attributes elementsRendered =
    let
        combineElements attributes =
            if model.oneline then
                Element.row NoStyle attributes
            else
                Element.column NoStyle ([ spacing 4 ] ++ attributes)
    in
    if List.isEmpty model.elements then
        PlainElement.view <|
            Element.row NoStyle
                [ spacing 10 ]
                [ settings.prefixFor [] 0, settings.suffix ]
    else
        combineElements attributes
            elementsRendered


plain :
    Settings
    -> (innerModel -> PlainElement Styles Variations)
    -> Model innerModel
    -> PlainElement Styles Variations
plain settings plainInner model =
    render settings model [] (List.map plainInner model.elements)



-- Lenses


elements : FieldSetter (Model value) (List value)
elements f model =
    { model | elements = f model.elements }


oneline : FieldSetter (Model value) Bool
oneline f model =
    { model | oneline = f model.oneline }
