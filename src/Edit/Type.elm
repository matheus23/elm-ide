module Edit.Type exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import Html5.DragDrop as DragDrop
import Styles exposing (..)
import Util exposing (FieldSetter)


-- Model


type alias Associations =
    List Association


type alias Association =
    ( RecordKey, Model )


type alias RecordKey =
    ContentEditable.Model


type Model
    = Hole Bool
    | Int
    | RecordType (List ( RecordKey, Model ))


type Msg
    = Replace Model
    | UpdateRecordKey Int ContentEditable.Msg
    | UpdateRecord Int Msg



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Replace newModel ->
            newModel

        UpdateRecord index innerMsg ->
            model
                & recordAssociations
                => Util.index index
                => Util.snd
                $= update innerMsg

        UpdateRecordKey index contentEditableMsg ->
            model
                & recordAssociations
                => Util.index index
                => Util.fst
                $= ContentEditable.update contentEditableMsg



-- View


view : Model -> Element Styles Variations Msg
view typ =
    case typ of
        Hole focused ->
            let
                typeHoleElement =
                    renderTypeHole
                        [ Events.onBlur (Replace (Hole False))
                        , Events.onFocus (Replace (Hole True))
                        ]
            in
            if focused then
                typeHoleElement
                    |> Element.below
                        [ renderTypeOptionList
                            [ renderTypeInt [ Events.onClick (Replace Int) ]
                            , let
                                exampleAssocs =
                                    [ ( ContentEditable.create "key", hole ) ]
                              in
                              renderRecord [ Events.onClick (Replace (RecordType exampleAssocs)) ]
                                -- what. THE FUCK? I cant use plainAssociationIndex?!?
                                (\_ -> plainAssociation)
                                exampleAssocs
                            ]
                        ]
            else
                typeHoleElement

        Int ->
            renderTypeInt []

        RecordType associations ->
            viewRecord associations


viewRecord : Associations -> Element Styles Variations Msg
viewRecord =
    renderRecord [] (\index -> viewAssociation index)


viewAssociation : Int -> ( RecordKey, Model ) -> Element Styles Variations Msg
viewAssociation index =
    renderAssociation
        (Element.map (UpdateRecordKey index) << ContentEditable.view Identifier)
        (Element.map (UpdateRecord index) << view)



-- "Rendering Functions". These dont create Messages on their own


renderTypeHole : List (Element.Attribute Variations msg) -> Element Styles Variations msg
renderTypeHole events =
    Element.el TypeHole
        ([ center
         , paddingXY 4 0
         , tabindex 0
         ]
            ++ events
        )
        (Util.styledText TypeHoleText "?")


renderTypeInt : List (Element.Attribute Variations msg) -> Element Styles Variations msg
renderTypeInt events =
    Util.styledTextAttr Identifier ([ center ] ++ events) "Int"


renderTypeOption : Element Styles Variations msg -> Element Styles Variations msg
renderTypeOption =
    Element.el TypeOption [ padding 4 ]


renderTypeOptionList : List (Element Styles Variations msg) -> Element Styles Variations msg
renderTypeOptionList options =
    Element.column TypeOptionList
        [ spacing 2 ]
        (List.map renderTypeOption options)


renderRecord :
    List (Element.Attribute Variations msg)
    -> (Int -> Association -> Element Styles Variations msg)
    -> Associations
    -> Element Styles Variations msg
renderRecord events renderAssoc associations =
    let
        openBracket =
            Util.styledText Keyword "{"

        closingBracket =
            Util.styledText Keyword "}"

        assocsRendered =
            List.indexedMap (\index association -> renderAssoc index association) associations
    in
    Element.row NoStyle
        ([ spacing 10 ] ++ events)
        ([ openBracket ] ++ assocsRendered ++ [ closingBracket ])


renderAssociation :
    (RecordKey -> Element Styles Variations msg)
    -> (Model -> Element Styles Variations msg)
    -> Association
    -> Element Styles Variations msg
renderAssociation renderKey renderType ( key, typ ) =
    Element.row NoStyle
        [ spacing 10 ]
        [ renderKey key
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

        RecordType associations ->
            plainRecord associations


plainHole : Element Styles Variations msg
plainHole =
    renderTypeHole []


plainInt : Element Styles Variations msg
plainInt =
    renderTypeInt []


plainRecord : Associations -> Element Styles Variations msg
plainRecord =
    -- plainAssociationIndex doesn't work. I don't fucking know why.
    renderRecord [] (\_ -> plainAssociation)


plainAssociation : Association -> Element Styles Variations msg
plainAssociation =
    renderAssociation plainKey plainType


plainAssociationIndex : Int -> Association -> Element Styles Variations msg
plainAssociationIndex index assoc =
    plainAssociation assoc


plainKey : RecordKey -> Element Styles Variations msg
plainKey key =
    Util.styledText Identifier key.liveContent



-- Smart constructors


hole : Model
hole =
    Hole False


recordType : List ( String, Model ) -> Model
recordType associations =
    RecordType (List.map (Util.fst $= ContentEditable.create) associations)



-- Lenses


{-| Ignores any other cases than Records.
-}
recordAssociations : FieldSetter Model (List ( RecordKey, Model ))
recordAssociations f model =
    case model of
        RecordType associations ->
            RecordType (f associations)

        _ ->
            model
