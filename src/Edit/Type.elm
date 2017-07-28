module Edit.Type exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import Styles exposing (..)
import Util exposing (FieldSetter)


-- Model


type alias Associations =
    List Association


type alias Association =
    ( RecordKey, Model )


type alias RecordKey =
    ContentEditable.Model


type alias RecordOf a =
    { associations : List ( RecordKey, a )
    , oneline : Bool
    , focused : Bool
    }


type Model
    = Hole Bool
    | Int
    | RecordType (RecordOf Model)


type Msg
    = Replace Model
    | UpdateRecordKey Int ContentEditable.Msg
    | UpdateRecord Int Msg
    | Blur
    | Focus



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Replace newModel ->
            newModel

        UpdateRecord index innerMsg ->
            model
                & record
                => associations
                => Util.index index
                => Util.snd
                $= update innerMsg

        UpdateRecordKey index contentEditableMsg ->
            model
                & record
                => associations
                => Util.index index
                => Util.fst
                $= ContentEditable.update contentEditableMsg

        Blur ->
            model & typeFocused .= False

        Focus ->
            model & typeFocused .= True


focusableAttributes : List (Element.Attribute Variations Msg)
focusableAttributes =
    [ Events.onBlur Blur
    , Events.onFocus Focus
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

        RecordType record ->
            viewRecord record


viewRecord : RecordOf Model -> Element Styles Variations Msg
viewRecord =
    renderRecord focusableAttributes (\index -> viewAssociation index)


viewAssociation : Int -> ( RecordKey, Model ) -> Element Styles Variations Msg
viewAssociation index =
    renderAssociation
        (Element.map (UpdateRecordKey index) << ContentEditable.view Identifier)
        (Element.map (UpdateRecord index) << view)
        index


viewTypeOption : Model -> Element Styles Variations Msg
viewTypeOption typ =
    Element.el TypeOption
        [ padding 4
        , Events.onClick (Replace typ)
        ]
        (plainType typ)


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


renderRecord :
    List (Element.Attribute Variations msg)
    -> (Int -> Association -> Element Styles Variations msg)
    -> RecordOf Model
    -> Element Styles Variations msg
renderRecord events renderAssoc record =
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
    (RecordKey -> Element Styles Variations msg)
    -> (Model -> Element Styles Variations msg)
    -> Int
    -> Association
    -> Element Styles Variations msg
renderAssociation renderKey renderType index ( key, typ ) =
    let
        openBracket =
            Util.styledText Keyword "{"

        comma =
            Util.styledText Keyword ","
    in
    Element.row NoStyle
        [ spacing 10 ]
        [ if index == 0 then
            openBracket
          else
            comma
        , renderKey key
        , Util.styledText Keyword ":"
        , renderType typ
        ]



-- render stuff plainly, i.e. deeply without any event handling (static images)


plainType : Model -> Element Styles Variations msg
plainType model =
    case model of
        Hole focused ->
            plainHole

        Int ->
            plainInt

        RecordType record ->
            plainRecord record


plainHole : Element Styles Variations msg
plainHole =
    renderTypeHole []


plainInt : Element Styles Variations msg
plainInt =
    renderTypeInt []


plainRecord : RecordOf Model -> Element Styles Variations msg
plainRecord =
    -- I have to eta abstract. I don't fucking know why.
    renderRecord [] (\index -> plainAssociation index)


plainAssociation : Int -> Association -> Element Styles Variations msg
plainAssociation =
    renderAssociation plainKey plainType


plainKey : RecordKey -> Element Styles Variations msg
plainKey key =
    Util.styledText Identifier key.liveContent



-- Smart constructors


hole : Model
hole =
    Hole False


recordType : Bool -> List ( String, Model ) -> Model
recordType oneline associations =
    RecordType
        { associations = List.map (Util.fst $= ContentEditable.create) associations
        , oneline = oneline
        , focused = False
        }



-- Lenses


{-| Ignores any other cases than Records.
-}
record : FieldSetter Model (RecordOf Model)
record f model =
    case model of
        RecordType record ->
            RecordType (f record)

        _ ->
            model


associations : FieldSetter (RecordOf a) (List ( RecordKey, a ))
associations f record =
    { record | associations = f record.associations }


typeFocused : FieldSetter Model Bool
typeFocused f model =
    case model of
        Hole focused ->
            Hole (f focused)

        RecordType record ->
            RecordType { record | focused = f record.focused }

        _ ->
            model
