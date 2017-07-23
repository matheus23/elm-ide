module AstEdit exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Focus exposing (..)
import Html5.DragDrop as DragDrop
import Styles exposing (..)
import Util exposing (FieldSetter)


type alias Name =
    ContentEditable.Model


name : String -> Name
name =
    ContentEditable.create


type alias Var =
    ContentEditable.Model


var : String -> Var
var =
    ContentEditable.create


type Type
    = Int


type alias Model =
    { name : Name
    , args : List ( Var, Type )
    , dragDrop : DragDrop.Model Int Int
    }


dragDrop : FieldSetter Model (DragDrop.Model Int Int)
dragDrop f model =
    { model | dragDrop = f model.dragDrop }


functionName : FieldSetter Model Name
functionName f model =
    { model | name = f model.name }


args : FieldSetter Model (List ( Var, Type ))
args f model =
    { model | args = f model.args }


type Msg
    = UpdateName ContentEditable.Msg
    | UpdateType Int Type
    | UpdateVar Int ContentEditable.Msg
    | DragDropMsg (DragDrop.Msg Int Int)


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName msg ->
            model & functionName $= ContentEditable.update msg

        UpdateType index typ ->
            model & args => Util.index index => Util.snd .= typ

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

        dragId =
            Debug.log "dragId" ( DragDrop.getDragId model.dragDrop, DragDrop.getDropId model.dragDrop )
    in
    Element.row NoStyle [ spacing 10, padding 5 ] (viewName model :: hasType :: args)


viewArgs : DragDrop.Model Int Int -> List ( Var, Type ) -> List (Element Styles Variations Msg)
viewArgs dragModel args =
    List.indexedMap (viewArg dragModel) args


viewArg : DragDrop.Model Int Int -> Int -> ( Var, Type ) -> Element Styles Variations Msg
viewArg dragModel index ( var, typ ) =
    let
        draggableAttributes =
            DragDrop.draggableElement DragDropMsg index

        isDragging =
            Util.isJust (DragDrop.getDragId dragModel)

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
                NoStyle
    in
    Element.column style
        ([ spacing 4 ] ++ draggableAttributes ++ droppableAttributes)
        [ viewType index typ, viewVar index var ]


viewType : Int -> Type -> Element Styles Variations Msg
viewType index typ =
    Util.styledText Identifier (toString typ)


viewVar : Int -> Var -> Element Styles Variations Msg
viewVar index var =
    Element.map (UpdateVar index) (ContentEditable.view Identifier var)


viewName : Model -> Element Styles Variations Msg
viewName model =
    Element.column NoStyle
        [ spacing 4 ]
        [ Element.map UpdateName (ContentEditable.view Identifier model.name)
        , Util.styledText Identifier model.name.liveContent
        ]
