module Edit.Function exposing (..)

import ContentEditable as ContentEditable
import Edit.Type as Type
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import Html5.DragDrop as DragDrop
import Styles exposing (..)
import Util exposing (FieldSetter)


type alias Name =
    ContentEditable.Model


type alias ArgName =
    ContentEditable.Model


type alias Model =
    { name : Name
    , args : List ( ArgName, Type.Model )
    , dragDrop : DragDrop.Model Int Int
    }


type Msg
    = UpdateName ContentEditable.Msg
    | UpdateType Int Type.Msg
    | UpdateVar Int ContentEditable.Msg
    | DragDropMsg (DragDrop.Msg Int Int)



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName msg ->
            model & functionName $= ContentEditable.update msg

        UpdateType index typeMsg ->
            model & args => Util.index index => Util.snd $= Type.update typeMsg

        UpdateVar index msg ->
            model & args => Util.index index => Util.fst $= ContentEditable.update msg

        DragDropMsg msg ->
            let
                ( drag, maybeDrop ) =
                    DragDrop.update msg model.dragDrop
            in
            (model & dragDrop .= drag)
                |> applyDrop maybeDrop


applyDrop : Maybe ( Int, Int ) -> Model -> Model
applyDrop maybeDrop model =
    case maybeDrop of
        Nothing ->
            model

        Just ( dragId, dropId ) ->
            let
                dragged =
                    Util.getIndex dragId model.args

                dropped =
                    Util.getIndex dropId model.args

                subst =
                    Maybe.map2 substitute dragged dropped

                substitute dragged dropped =
                    (model & args => Util.index dragId .= dropped)
                        & args
                        => Util.index dropId
                        .= dragged
            in
            Maybe.withDefault model subst



-- View


view : Model -> Element Styles Variations Msg
view model =
    let
        argElements =
            viewArgs model.dragDrop model.args

        args =
            List.intersperse arrow argElements

        arrow =
            Util.styledText Keyword "→"

        hasType =
            Util.styledText Keyword ":"

        equalsSign =
            Util.styledTextAttr Keyword [ paddingLeft 4, alignBottom ] "="
    in
    Element.row NoStyle [ spacing 10, padding 5 ] (viewName model :: hasType :: args ++ [ equalsSign ])


viewArgs : DragDrop.Model Int Int -> List ( ArgName, Type.Model ) -> List (Element Styles Variations Msg)
viewArgs dragModel args =
    List.indexedMap (viewArg dragModel) args


viewArg : DragDrop.Model Int Int -> Int -> ( ArgName, Type.Model ) -> Element Styles Variations Msg
viewArg dragModel index ( argName, typ ) =
    let
        isDragging =
            Util.isJust (DragDrop.getDragId dragModel)

        draggableAttributes =
            if not isDragging then
                DragDrop.draggableElement DragDropMsg index
            else
                []

        isDraggedElem =
            Maybe.withDefault -1 (DragDrop.getDragId dragModel) == index

        isDroppedElem =
            Maybe.withDefault -1 (DragDrop.getDropId dragModel) == index

        droppableAttributes =
            if isDragging then
                DragDrop.droppableElement DragDropMsg index
            else
                []

        style =
            if isDraggedElem then
                Dragged
            else if isDroppedElem then
                DragHover
            else if isDragging then
                Droppable
            else
                Draggable
    in
    Element.column style
        ([ spacing 2 ] ++ draggableAttributes ++ droppableAttributes)
        [ Element.map (UpdateType index) (Type.view typ)
        , viewArgName index argName
        ]


viewArgName : Int -> ArgName -> Element Styles Variations Msg
viewArgName index argName =
    Element.map (UpdateVar index) (ContentEditable.view Identifier argName)


viewName : Model -> Element Styles Variations Msg
viewName model =
    Element.column NoStyle
        [ spacing 5 ]
        [ Element.map UpdateName (ContentEditable.view Identifier model.name)
        , Util.styledText Identifier model.name.liveContent
        ]



-- Smart Constructors


argName : String -> ArgName
argName =
    ContentEditable.create


name : String -> Name
name =
    ContentEditable.create



-- Lenses


dragDrop : FieldSetter Model (DragDrop.Model Int Int)
dragDrop f model =
    { model | dragDrop = f model.dragDrop }


functionName : FieldSetter Model Name
functionName f model =
    { model | name = f model.name }


args : FieldSetter Model (List ( ArgName, Type.Model ))
args f model =
    { model | args = f model.args }
