module Edit.Type exposing (..)

import Edit.Actionbar as Actionbar
import Edit.Record as Record
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Focusable
import Styles exposing (..)
import Util


-- Model


type alias Model =
    Focusable.Model
        { typeCase : TypeCase
        }


type TypeCase
    = Hole
    | Int
    | RecordType (Record.Model Model)


type Msg
    = Replace Model
    | UpdateRecord (Record.Msg Msg)
    | UpdateFocus Focusable.Msg



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Replace newModel ->
            newModel

        UpdateRecord recordMsg ->
            model & record $= Record.update update recordMsg

        UpdateFocus focusMsg ->
            Focusable.update focusMsg model


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.typeCase of
        RecordType record ->
            Sub.map UpdateRecord (Record.subscriptions subscriptions record)

        _ ->
            Sub.none



-- View


view : Actionbar.Model -> Model -> Element Styles Variations Msg
view actionbar model =
    case model.typeCase of
        Hole ->
            renderTypeHole
                ([]
                    |> Util.appendWhen (not (Actionbar.anyActive actionbar))
                        (Focusable.attributes UpdateFocus)
                )
                |> Element.below
                    [ Element.when (model.focused && not (Actionbar.anyActive actionbar)) <|
                        viewTypeOptionList
                            [ int
                            , recordType False
                                [ ( "key", hole )
                                , ( "test", int )
                                ]
                            ]
                    ]

        Int ->
            renderTypeInt []

        RecordType record ->
            Focusable.wrapFocusable NoStyle
                UpdateFocus
                (Element.map UpdateRecord (Record.view (view actionbar) actionbar record))


viewTypeOption : Model -> Element Styles Variations Msg
viewTypeOption typ =
    Element.el Button
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



-- render stuff plainly, i.e. deeply without any event handling (like static images)


plain : Model -> Element Styles Variations msg
plain model =
    case model.typeCase of
        Hole ->
            plainHole

        Int ->
            plainInt

        RecordType record ->
            Record.plain plain record


plainHole : Element Styles Variations msg
plainHole =
    renderTypeHole []


plainInt : Element Styles Variations msg
plainInt =
    renderTypeInt []



-- Smart constructors


fromCase : TypeCase -> Model
fromCase typeCase =
    { focused = False, typeCase = typeCase }


hole : Model
hole =
    fromCase Hole


int : Model
int =
    fromCase Int


recordType : Bool -> List ( String, Model ) -> Model
recordType oneline associations =
    { focused = False
    , typeCase = RecordType (Record.init oneline associations)
    }



-- Lenses


{-| Ignores all cases except for Records.
-}
record : FieldSetter Model (Record.Model Model)
record f model =
    case model.typeCase of
        RecordType record ->
            model & typeCase .= RecordType (f record)

        _ ->
            model


typeCase : FieldSetter Model TypeCase
typeCase f model =
    { model | typeCase = f model.typeCase }
