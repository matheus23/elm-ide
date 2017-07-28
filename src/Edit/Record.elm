module Edit.Record exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import Styles exposing (..)
import Util exposing (FieldSetter)


-- Model


type alias Key =
    ContentEditable.Model


type alias Association a =
    { key : Key
    , value : a
    , focused : Bool
    }


type alias Model a =
    { associations : List (Association a)
    , oneline : Bool
    }


type Msg innerMsg
    = UpdateKey Int ContentEditable.Msg
    | UpdateValue Int innerMsg
    | UpdateFocus Int Bool



-- Update


update : (innerMsg -> innerModel -> innerModel) -> Msg innerMsg -> Model innerModel -> Model innerModel
update updateInner msg model =
    case msg of
        UpdateKey index contentEditableMsg ->
            model & associations => Util.index index => key $= ContentEditable.update contentEditableMsg

        UpdateValue index innerMsg ->
            model & associations => Util.index index => value $= updateInner innerMsg

        UpdateFocus index focus ->
            model & associations => Util.index index => focused .= focus



-- View


view :
    (innerModel -> Element Styles Variations innerMsg)
    -> Model innerModel
    -> Element Styles Variations (Msg innerMsg)
view viewInner model =
    render [] (\index -> viewAssociation viewInner index) model


viewAssociation :
    (innerModel -> Element Styles Variations innerMsg)
    -> Int
    -> Association innerModel
    -> Element Styles Variations (Msg innerMsg)
viewAssociation viewInner index =
    -- TODO: Implement UpdateFocus HERE
    renderAssociation []
        (Element.map (UpdateKey index) << ContentEditable.view Identifier)
        (Element.map (UpdateValue index) << viewInner)
        index



-- Render


render :
    List (Element.Attribute Variations msg)
    -> (Int -> Association innerModel -> Element Styles Variations msg)
    -> Model innerModel
    -> Element Styles Variations msg
render events renderAssoc record =
    let
        closingBracket =
            Util.styledTextAttr Keyword lastBrackedPadding "}"

        lastBrackedPadding =
            if record.oneline then
                [ paddingLeft 10 ]
            else
                []

        assocsRendered =
            -- probably because of strictness, I have to eta abstract.........
            List.indexedMap (\index -> renderAssoc index) record.associations

        combineAssociations events =
            if record.oneline then
                Element.row NoStyle events
            else
                Element.column NoStyle ([ spacing 4 ] ++ events)
    in
    combineAssociations events
        (assocsRendered ++ [ closingBracket ])


renderAssociation :
    List (Element.Attribute Variations msg)
    -> (Key -> Element Styles Variations msg)
    -> (innerModel -> Element Styles Variations msg)
    -> Int
    -> Association innerModel
    -> Element Styles Variations msg
renderAssociation attributes renderKey renderType index association =
    let
        openBracket =
            Util.styledTextAttr Keyword [ paddingRight 10 ] "{"

        comma =
            Util.styledTextAttr Keyword [ paddingRight 10 ] ","
    in
    Element.row NoStyle
        []
        [ if index == 0 then
            openBracket
          else
            comma
        , Element.row NoStyle
            ([ spacing 10 ] ++ attributes)
            [ renderKey association.key
            , Util.styledText Keyword ":"
            , renderType association.value
            ]
        ]



-- Plain render


plain : (innerModel -> Element Styles Variations msg) -> Model innerModel -> Element Styles Variations msg
plain plainInner =
    -- I have to eta abstract. I don't fucking know why.
    render [] (\index -> plainAssociation plainInner index)


plainAssociation : (innerModel -> Element Styles Variations msg) -> Int -> Association innerModel -> Element Styles Variations msg
plainAssociation plainInner =
    renderAssociation [] plainKey plainInner


plainKey : Key -> Element Styles Variations msg
plainKey key =
    Util.styledText Identifier key.liveContent



-- Lenses


associations : FieldSetter (Model a) (List (Association a))
associations f record =
    { record | associations = f record.associations }


key : FieldSetter (Association a) Key
key f association =
    { association | key = f association.key }


value : FieldSetter (Association a) a
value f association =
    { association | value = f association.value }


focused : FieldSetter (Association a) Bool
focused f association =
    { association | focused = f association.focused }



-- Smart constructors


assoc : ( String, a ) -> Association a
assoc ( name, innerValue ) =
    { key = ContentEditable.create name
    , value = innerValue
    , focused = False
    }


model : Bool -> List ( String, a ) -> Model a
model oneline associations =
    { associations = List.map assoc associations
    , oneline = oneline
    }
