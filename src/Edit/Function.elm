module Edit.Function exposing (..)

import ContentEditable as ContentEditable
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


type Type
    = Hole Bool
    | Int
    | Record (List ( Name, Type ))


type alias Model =
    { name : Name
    , args : List ( ArgName, Type )
    , dragDrop : DragDrop.Model Int Int
    }


type Msg
    = UpdateName ContentEditable.Msg
    | UpdateType Int Type
    | UpdateRecordTypeName Int ContentEditable.Msg
    | UpdateRecordType Int Msg -- this is shit
    | UpdateVar Int ContentEditable.Msg
    | DragDropMsg (DragDrop.Msg Int Int)



-- TODO: Extract Edit.Type module so that it has its own Msgs.!!!!!!!!!!!!!
-- Update


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


viewArgs : DragDrop.Model Int Int -> List ( ArgName, Type ) -> List (Element Styles Variations Msg)
viewArgs dragModel args =
    List.indexedMap (viewArg dragModel) args


viewArg : DragDrop.Model Int Int -> Int -> ( ArgName, Type ) -> Element Styles Variations Msg
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
        [ viewType index typ, viewArgName index argName ]


viewType : Int -> Type -> Element Styles Variations Msg
viewType index typ =
    case typ of
        Hole focused ->
            let
                typeHoleElement =
                    renderTypeHole
                        [ Events.onBlur (UpdateType index (Hole False))
                        , Events.onFocus (UpdateType index (Hole True))
                        ]
            in
            if focused then
                typeHoleElement
                    |> Element.below
                        [ renderTypeOptionList
                            [ renderTypeInt [ Events.onClick (UpdateType index Int) ]
                            , renderTypeHole [ Events.onClick (UpdateType index (Hole True)) ]
                            ]
                        ]
            else
                typeHoleElement

        Int ->
            renderTypeInt []

        Record nameTypePairs ->
            let
                openBracket =
                    Util.styledText Keyword "{"

                closingBracket =
                    Util.styledText Keyword "}"
            in
            Element.row NoStyle
                [ spacing 10 ]
                ([ openBracket ] ++ List.map viewNameTypePair nameTypePairs ++ [ closingBracket ])


renderTypeHole : List (Element.Attribute Variations Msg) -> Element Styles Variations Msg
renderTypeHole events =
    Element.el TypeHole
        ([ center
         , paddingXY 4 0
         , tabindex 0
         ]
            ++ events
        )
        (Util.styledText TypeHoleText "?")


renderTypeInt : List (Element.Attribute Variations Msg) -> Element Styles Variations Msg
renderTypeInt events =
    Util.styledTextAttr Identifier ([ center ] ++ events) "Int"


renderTypeOption : Element Styles Variations Msg -> Element Styles Variations Msg
renderTypeOption =
    Element.el TypeOption [ padding 4 ]


renderTypeOptionList : List (Element Styles Variations Msg) -> Element Styles Variations Msg
renderTypeOptionList options =
    Element.column TypeOptionList
        [ spacing 2 ]
        (List.map renderTypeOption options)


viewNameTypePair : Int -> ( Name, Type ) -> Element Styles Variations Msg
viewNameTypePair index ( name, typ ) =
    Element.row NoStyle
        [ spacing 10 ]
        [ Element.map (UpdateRecordTypeName index) (ContentEditable.view Identifier name)
        , Util.styledText Keyword ":"
        , Element.map (UpdateRecordType index) (viewType typ)
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


args : FieldSetter Model (List ( ArgName, Type ))
args f model =
    { model | args = f model.args }
