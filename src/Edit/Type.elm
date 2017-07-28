module Edit.Type exposing (..)

import Edit.Record as Record
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import Styles exposing (..)
import Util exposing (FieldSetter)


-- Model


type Model
    = Hole Bool
    | Int
    | RecordType Bool (Record.Model Model)


type Msg
    = Replace Model
    | UpdateRecord (Record.Msg Msg)
    | SetFocus Bool



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Replace newModel ->
            newModel

        UpdateRecord recordMsg ->
            model & record $= Record.update update recordMsg

        SetFocus focus ->
            model & typeFocused .= focus


focusableAttributes : List (Element.Attribute Variations Msg)
focusableAttributes =
    [ Events.onBlur (SetFocus False)
    , Events.onFocus (SetFocus True)
    , tabindex 0 -- makes the element focusable
    ]



-- View


view : Model -> Element Styles Variations Msg
view typ =
    case typ of
        Hole focused ->
            renderTypeHole
                focusableAttributes
                |> Element.below
                    [ Element.when focused <|
                        viewTypeOptionList
                            [ Int
                            , recordType False
                                [ ( "key", hole )
                                , ( "test", Int )
                                ]
                            ]
                    ]

        Int ->
            renderTypeInt []

        RecordType focused record ->
            Element.el NoStyle
                [ Events.onFocus (SetFocus True)
                , Events.onBlur (SetFocus False)
                , tabindex 0
                ]
                (Element.map UpdateRecord (Record.view view record))


viewTypeOption : Model -> Element Styles Variations Msg
viewTypeOption typ =
    Element.el TypeOption
        [ padding 4
        , Events.onClick (Replace typ)
        ]
        (plain typ)


viewTypeOptionList : List Model -> Element Styles Variations Msg
viewTypeOptionList options =
    Element.column TypeOptionList
        [ spacing 2 ]
        (List.map viewTypeOption options)



-- "Rendering Functions". These dont create Messages on their own


renderTypeHole : List (Element.Attribute Variations msg) -> Element Styles Variations msg
renderTypeHole events =
    Element.el TypeHole
        ([ center
         , paddingXY 4 0
         ]
            ++ events
        )
        (Util.styledText TypeHoleText "?")


renderTypeInt : List (Element.Attribute Variations msg) -> Element Styles Variations msg
renderTypeInt events =
    Util.styledTextAttr Identifier ([ center ] ++ events) "Int"



-- render stuff plainly, i.e. deeply without any event handling (static images)


plain : Model -> Element Styles Variations msg
plain model =
    case model of
        Hole focused ->
            plainHole

        Int ->
            plainInt

        RecordType focused record ->
            Record.plain plain record


plainHole : Element Styles Variations msg
plainHole =
    renderTypeHole []


plainInt : Element Styles Variations msg
plainInt =
    renderTypeInt []



-- Smart constructors


hole : Model
hole =
    Hole False


recordType : Bool -> List ( String, Model ) -> Model
recordType oneline associations =
    RecordType False (Record.model oneline associations)



-- Lenses


{-| Ignores any other cases than Records.
-}
record : FieldSetter Model (Record.Model Model)
record f model =
    case model of
        RecordType focused record ->
            RecordType focused (f record)

        _ ->
            model


typeFocused : FieldSetter Model Bool
typeFocused f model =
    case model of
        Hole focused ->
            Hole (f focused)

        RecordType focused record ->
            RecordType (f focused) record

        _ ->
            model
