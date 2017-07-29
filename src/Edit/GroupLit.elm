module Edit.GroupLit exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import Styles exposing (..)
import Util exposing (FieldSetter)


type alias Model value =
    { elements : List value
    , oneline : Bool
    }


type Msg innerMsg
    = UpdateIndex Int innerMsg


type alias Settings msg innerModel =
    { prefixFor : List (Element.Attribute Variations msg) -> Int -> Element Styles Variations msg
    , suffix : Element Styles Variations msg
    , viewInner : Int -> innerModel -> Element Styles Variations msg
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
            model & elements => Util.indexConcat index $= updateValue valueMsg



-- View


view :
    Settings msg innerModel
    -> Model innerModel
    -> Element Styles Variations msg
view settings model =
    let
        suffixElement =
            Element.el NoStyle lastBrackedPadding settings.suffix

        lastBrackedPadding =
            if model.oneline then
                [ paddingLeft 10 ]
            else
                []

        assocsRendered =
            List.indexedMap (viewElement settings) model.elements

        combineAssociations attributes =
            if model.oneline then
                Element.row NoStyle attributes
            else
                Element.column NoStyle ([ spacing 4 ] ++ attributes)
    in
    if List.isEmpty model.elements then
        Element.row NoStyle
            [ spacing 10 ]
            [ settings.prefixFor [] 0, settings.suffix ]
    else
        combineAssociations []
            (assocsRendered ++ [ suffixElement ])


viewElement :
    Settings msg innerModel
    -> Int
    -> innerModel
    -> Element Styles Variations msg
viewElement settings index innerModel =
    Element.row NoStyle
        []
        [ settings.prefixFor [ paddingRight 10 ] index
        , settings.viewInner index innerModel
        ]



-- Lenses


elements : FieldSetter (Model value) (List value)
elements f model =
    { model | elements = f model.elements }


oneline : FieldSetter (Model value) Bool
oneline f model =
    { model | oneline = f model.oneline }
