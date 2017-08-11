module Edit.Function exposing (..)

import Actionbar as AB exposing (Event(DeactivateAction))
import ContentEditable as ContentEditable
import DividedList
import DragAndDrop
import Edit.Actionbar as Actionbar
import Edit.Arg as Arg
import Edit.Expression as Expression
import Edit.Type as Type
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Styles exposing (..)
import Util


type alias Name =
    ContentEditable.Model


type alias Model =
    { name : Name
    , args : List Arg.Model
    , body : Expression.Model
    , dragModel : DragAndDrop.Model Int Int
    }


type Msg
    = UpdateName ContentEditable.Msg
    | ArgMsg Int Arg.Msg
    | DragAndDropMsg (DragAndDrop.Msg Int Int)
    | ExpressionMsg Expression.Msg
    | ActionbarEvent Actionbar.Event
    | AddArg


init : String -> List ( String, Type.Model ) -> Expression.Model -> Model
init functionName args body =
    { name = name functionName
    , args = List.map (uncurry Arg.init) args
    , body = body
    , dragModel = DragAndDrop.init
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName msg ->
            model & functionName $= ContentEditable.update msg

        ArgMsg index argMsg ->
            model & args => Focus.indexConcat index $= Arg.update argMsg

        DragAndDropMsg msg ->
            let
                ( newDragModel, maybeEvent ) =
                    DragAndDrop.updateWithEvents True msg model.dragModel

                applyEvent =
                    case maybeEvent of
                        Just (DragAndDrop.SuccessfulDrop dragInd dropInd) ->
                            Util.moveByIndex dragInd dropInd

                        _ ->
                            identity
            in
            model
                |> (dragModel .= newDragModel)
                |> (args $= applyEvent)

        ExpressionMsg exprMsg ->
            model & body $= Expression.update exprMsg

        AddArg ->
            model & args $= (\args -> args ++ [ Arg.init "arg" Type.hole ])

        ActionbarEvent event ->
            if event == DeactivateAction then
                model & dragModel $= DragAndDrop.update DragAndDrop.StopDragging
            else
                model


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subscriptionForArg index arg =
            Sub.map (ArgMsg index) (Arg.subscriptions arg)
    in
    Sub.batch
        [ Sub.batch (List.indexedMap subscriptionForArg model.args)
        , Sub.map DragAndDropMsg (DragAndDrop.subscriptions model.dragModel)
        ]



-- View


view : Actionbar.Model -> Model -> Element Styles Variations Msg
view actionbar model =
    Element.column NoStyle
        [ spacing 4 ]
        [ viewNameAndArgs actionbar model
        , viewBody model.body
        ]


viewNameAndArgs : Actionbar.Model -> Model -> Element Styles Variations Msg
viewNameAndArgs actionbar model =
    let
        viewDivider elementsBefore elementsAfter =
            if elementsAfter == 0 then
                equalsSign elementsBefore
            else if elementsBefore == 0 then
                hasType elementsBefore
            else
                arrow elementsBefore

        wrap index =
            Element.el
                (if DragAndDrop.isHoveringDroppableId index model.dragModel then
                    OverDraggable
                 else if DragAndDrop.isDragging model.dragModel then
                    Draggable
                 else
                    NoStyle
                )
                (DragAndDrop.droppable model.dragModel DragAndDropMsg index)

        arrow index =
            ( "arrow", Util.styledText Keyword "→" )

        hasType index =
            ( "hasType", Util.styledText Keyword ":" )

        equalsSign index =
            ( "equalsSign", Util.styledTextAttr Keyword [ alignBottom ] "=" )
    in
    Element.row NoStyle
        [ spacing 10, padding 5 ]
        [ viewName model
        , DividedList.viewKeyed NoStyle [ spacing 10 ] viewDivider (viewArgs actionbar model.dragModel model.args)
        ]


viewArgs : Actionbar.Model -> DragAndDrop.Model Int Int -> List Arg.Model -> List ( String, Element Styles Variations Msg )
viewArgs actionbar dragModel =
    List.indexedMap (viewArg actionbar dragModel)


viewArg : Actionbar.Model -> DragAndDrop.Model Int Int -> Int -> Arg.Model -> ( String, Element Styles Variations Msg )
viewArg actionbar dragModel index arg =
    let
        overlay style index =
            Element.el
                (if DragAndDrop.isHoveringDroppableId index dragModel then
                    style
                 else
                    NoStyle
                )
                (height (fill 1) :: DragAndDrop.droppable dragModel DragAndDropMsg index)
                Element.empty

        ( key, argViewed ) =
            Arg.view actionbar arg & Focus.second => Element.map $= ArgMsg index

        attributes =
            []
                |> Util.appendWhen (Actionbar.actionActive Actionbar.Drag actionbar)
                    (DragAndDrop.draggableInstant dragModel DragAndDropMsg index)

        style =
            if DragAndDrop.isDraggingId index dragModel then
                Dragging
            else if DragAndDrop.isHoveringDraggableId index dragModel then
                OverDraggable
            else if Actionbar.actionActive Actionbar.Drag actionbar && not (DragAndDrop.isDragging dragModel) then
                Draggable
            else
                NoStyle

        wrap =
            if DragAndDrop.isDragging dragModel then
                Util.splitOverlay
                    (overlay OverlayLeft index)
                    (overlay OverlayRight (index + 1))
            else
                identity
    in
    ( key
    , wrap <|
        Element.el style
            attributes
            argViewed
    )


viewName : Model -> Element Styles Variations Msg
viewName model =
    Element.column NoStyle
        [ spacing 5 ]
        [ Element.map UpdateName (ContentEditable.view Identifier model.name)
        , Util.styledText Identifier model.name.liveContent
        ]


viewAddArg : Element Styles Variations Msg
viewAddArg =
    Element.el Button [ center, paddingXY 4 0, Events.onClick AddArg ] (Element.text "+")


viewBody : Expression.Model -> Element Styles Variations Msg
viewBody expression =
    Element.el NoStyle
        [ paddingLeft 20 ]
        (Element.map ExpressionMsg (Expression.view expression))



-- Smart Constructors


name : String -> Name
name =
    ContentEditable.create



-- Lenses


functionName : FieldSetter Model Name
functionName f model =
    { model | name = f model.name }


args : FieldSetter Model (List Arg.Model)
args f model =
    { model | args = f model.args }


body : FieldSetter Model Expression.Model
body f model =
    { model | body = f model.body }


dragModel : FieldSetter Model (DragAndDrop.Model Int Int)
dragModel f model =
    { model | dragModel = f model.dragModel }
