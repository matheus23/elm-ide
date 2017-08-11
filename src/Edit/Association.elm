module Edit.Association exposing (..)

import ContentEditable as ContentEditable
import Edit.Actionbar as Actionbar
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Focusable
import Styles exposing (..)
import Util


type alias Key =
    ContentEditable.Model


type alias Model a =
    Focusable.Model
        { key : Key
        , value : a
        }


type Msg innerMsg
    = UpdateKey ContentEditable.Msg
    | UpdateValue innerMsg
    | UpdateFocus Focusable.Msg
    | Remove



-- Update


update : (innerMsg -> innerModel -> innerModel) -> Msg innerMsg -> Model innerModel -> List (Model innerModel)
update updateInner msg model =
    case msg of
        UpdateKey contentEditableMsg ->
            [ model & key $= ContentEditable.update contentEditableMsg ]

        UpdateValue innerMsg ->
            [ model & value $= updateInner innerMsg ]

        UpdateFocus focusMsg ->
            [ Focusable.update focusMsg model ]

        Remove ->
            []


subscriptions : (a -> Sub innerMsg) -> Model a -> Sub (Msg innerMsg)
subscriptions innerSubs model =
    Sub.map UpdateValue (innerSubs model.value)



-- View


view :
    (innerModel -> Element Styles Variations innerMsg)
    -> Actionbar.Model
    -> Model innerModel
    -> Element Styles Variations (Msg innerMsg)
view viewInner actionbar model =
    render (Focusable.attributes UpdateFocus)
        (Element.map UpdateKey << ContentEditable.viewAttr [] Identifier (not (Actionbar.anyActive actionbar)))
        (Element.map UpdateValue << viewInner)
        model
        |> (if model.focused then
                viewGadgets
            else
                identity
           )


viewGadgets : Element Styles Variations (Msg innerMsg) -> Element Styles Variations (Msg innerMsg)
viewGadgets =
    Element.onRight
        [ Util.styledTextAttr Button [ Events.onClick Remove, paddingXY 8 0 ] "-"
        ]



-- Rendering functions


render :
    List (Element.Attribute Variations msg)
    -> (Key -> Element Styles Variations msg)
    -> (innerModel -> Element Styles Variations msg)
    -> Model innerModel
    -> Element Styles Variations msg
render attributes renderKey renderType association =
    Element.row NoStyle
        ([ spacing 10 ] ++ attributes)
        [ renderKey association.key
        , Util.styledText Keyword ":"
        , renderType association.value
        ]



-- Plain rendering


plain : (innerModel -> Element Styles Variations msg) -> Model innerModel -> Element Styles Variations msg
plain plainInner =
    render [] plainKey plainInner


plainKey : Key -> Element Styles Variations msg
plainKey key =
    Util.styledText Identifier key.liveContent



-- Lenses


key : FieldSetter (Model a) Key
key f model =
    { model | key = f model.key }


value : FieldSetter (Model a) a
value f model =
    { model | value = f model.value }



-- Smart Constructors


init : ( String, a ) -> Model a
init ( name, innerValue ) =
    { key = ContentEditable.create name
    , value = innerValue
    , focused = False
    }
