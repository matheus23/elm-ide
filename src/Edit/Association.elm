module Edit.Association exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Focus exposing (..)
import Focusable
import Styles exposing (..)
import Util exposing (FieldSetter)


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



-- Update


update : (innerMsg -> innerModel -> innerModel) -> Msg innerMsg -> Model innerModel -> Model innerModel
update updateInner msg model =
    case msg of
        UpdateKey contentEditableMsg ->
            model & key $= ContentEditable.update contentEditableMsg

        UpdateValue innerMsg ->
            model & value $= updateInner innerMsg

        UpdateFocus focusMsg ->
            Focusable.update focusMsg model



-- View


view :
    (innerModel -> Element Styles Variations innerMsg)
    -> Model innerModel
    -> Element Styles Variations (Msg innerMsg)
view viewInner =
    render (Focusable.attributes UpdateFocus)
        (Element.map UpdateKey << ContentEditable.view Identifier)
        (Element.map UpdateValue << viewInner)



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


model : ( String, a ) -> Model a
model ( name, innerValue ) =
    { key = ContentEditable.create name
    , value = innerValue
    , focused = False
    }
