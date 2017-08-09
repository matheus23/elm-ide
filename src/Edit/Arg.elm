module Edit.Arg exposing (..)

import ContentEditable as ContentEditable
import DragAndDrop
import DragAndDrop.Divider as Divider
import DragAndDrop.ReorderList as ReorderList
import Edit.Type as Type
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Element.Keyed as Keyed
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Focusable
import Styles exposing (..)
import Tuple
import Util


-- Model


type alias Name =
    ContentEditable.Model


type alias Model =
    Focusable.Model
        { name : Name
        , itsType : Type.Model
        }


type Msg
    = UpdateName ContentEditable.Msg
    | UpdateType Type.Msg
    | FocusMsg Focusable.Msg
    | Remove


init : String -> Type.Model -> Model
init name itsType =
    { name = ContentEditable.create name
    , itsType = itsType
    , focused = False
    }



-- Update


update : Msg -> Model -> List Model
update msg model =
    case msg of
        UpdateName contentEditableMsg ->
            [ model & name $= ContentEditable.update contentEditableMsg ]

        UpdateType typeMsg ->
            [ model & itsType $= Type.update typeMsg ]

        FocusMsg focusMsg ->
            [ Focusable.update focusMsg model ]

        Remove ->
            []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UpdateType (Type.subscriptions model.itsType)



-- View


view : DragAndDrop.Model Int Int -> Model -> ( String, Element Styles Variations Msg )
view dragModel model =
    ( model.name.content
    , Element.column NoStyle
        ([ spacing 2
         ]
            ++ Focusable.attributes FocusMsg
        )
        [ Element.map UpdateType (Type.view model.itsType)
        , viewName model.name
        , Element.el DragGrab [ center ] (Element.text ":::")
        ]
        |> Focus.when model.focused viewGadgets
    )


viewName : Name -> Element Styles Variations Msg
viewName name =
    Element.map UpdateName (ContentEditable.viewAttr [ alignBottom ] Identifier name)


viewGadgets : Element Styles Variations Msg -> Element Styles Variations Msg
viewGadgets =
    Element.below
        [ Util.styledTextAttr Button [ Events.onClick Remove, paddingXY 8 0 ] "-"
        ]



-- Lenses


name : FieldSetter Model Name
name f model =
    { model | name = f model.name }


itsType : FieldSetter Model Type.Model
itsType f model =
    { model | itsType = f model.itsType }
