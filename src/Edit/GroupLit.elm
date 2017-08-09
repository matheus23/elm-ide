module Edit.GroupLit exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import PlainElement exposing (PlainAttribute, PlainElement)
import Styles exposing (..)
import Util


type alias Model value =
    { elements : List value
    , oneline : Bool
    }


type Msg innerMsg
    = UpdateIndex Int innerMsg
    | FlipOneLine


type alias Settings =
    { prefixFor : List (PlainAttribute Variations) -> Int -> PlainElement Styles Variations
    , suffix : PlainElement Styles Variations
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



-- View


view :
    Settings
    -> (innerModel -> Element Styles Variations msg)
    -> Model innerModel
    -> Element Styles Variations (Msg msg)
view settings viewInner model =
    let
        viewElement index innerModel =
            Element.row NoStyle
                []
                [ PlainElement.view (settings.prefixFor [ paddingRight 10 ] index)
                , Element.map (UpdateIndex index) (viewInner innerModel)
                ]

        elementsRendered =
            List.indexedMap viewElement model.elements
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
        suffixElement =
            PlainElement.el NoStyle lastBrackedPadding settings.suffix

        lastBrackedPadding =
            if model.oneline then
                [ paddingLeft 10 ]
            else
                []

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
            (elementsRendered ++ [ PlainElement.view suffixElement ])


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
