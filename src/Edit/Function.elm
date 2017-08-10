module Edit.Function exposing (..)

import ContentEditable as ContentEditable
import DividedList
import DragAndDrop
import DragAndDrop.Divider as Divider
import DragAndDrop.ReorderList as ReorderList
import Edit.Arg as Arg
import Edit.Expression as Expression
import Edit.Type as Type
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Element.Keyed as Keyed
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
    }


type Msg
    = UpdateName ContentEditable.Msg
    | ReorderListMsg ReorderList.Msg
    | ArgMsg Int Arg.Msg
    | ExpressionMsg Expression.Msg
    | AddArg


init : String -> List ( String, Type.Model ) -> Expression.Model -> Model
init functionName args body =
    { name = name functionName
    , args = List.map (uncurry Arg.init) args
    , body = body
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName msg ->
            model & functionName $= ContentEditable.update msg

        ReorderListMsg msg ->
            {-
               let
                   ( newReorderList, maybeEvents ) =
                       ReorderList.updateWithEvents msg model.args

                   justDropped =
                       case maybeEvents of
                           Just (DragAndDrop.SuccessfulDrop _ _) ->
                               True

                           _ ->
                               False
               in
               model
                   |> (args .= newReorderList)
                   |> Focus.when justDropped
                       (args => ReorderList.elements => List.map => Focusable.focused .= False)
            -}
            model

        ArgMsg index argMsg ->
            model & args => Focus.indexConcat index $= Arg.update argMsg

        ExpressionMsg exprMsg ->
            model & body $= Expression.update exprMsg

        AddArg ->
            model & args $= (\args -> args ++ [ Arg.init "arg" Type.hole ])


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subscriptionForArg index arg =
            Sub.map (ArgMsg index) (Arg.subscriptions arg)
    in
    Sub.batch
        [ Sub.batch (List.indexedMap subscriptionForArg model.args)

        --, Sub.map ReorderListMsg (ReorderList.subscriptions model.args)
        ]



-- View


view : Model -> Element Styles Variations Msg
view model =
    Element.column NoStyle
        [ spacing 4 ]
        [ viewNameAndArgs model
        , viewBody model.body
        ]


viewNameAndArgs : Model -> Element Styles Variations Msg
viewNameAndArgs model =
    let
        viewDivider elementsBefore elementsAfter =
            if elementsAfter == 0 then
                equalsSign
            else if elementsBefore == 0 then
                hasType
            else
                arrow

        arrow =
            ( "arrow", Util.styledText Keyword "→" )

        hasType =
            ( "hasType", Util.styledText Keyword ":" )

        equalsSign =
            ( "equalsSign", Util.styledTextAttr Keyword [ alignBottom, padding 4 ] "=" )
    in
    Element.row NoStyle
        [ spacing 10, padding 5 ]
        [ viewName model
        , DividedList.viewKeyed NoStyle [ spacing 10 ] viewDivider (viewArgs DragAndDrop.init model.args)
        ]



--Keyed.row NoStyle [ spacing 10, padding 5 ] (viewName model :: hasType :: args ++ [ equalsSign ])


viewArgs : DragAndDrop.Model Int Int -> List Arg.Model -> List ( String, Element Styles Variations Msg )
viewArgs dragModel =
    let
        viewArg index arg =
            Arg.view dragModel arg & Focus.second => Element.map $= ArgMsg index
    in
    List.indexedMap viewArg


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
