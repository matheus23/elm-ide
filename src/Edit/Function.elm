module Edit.Function exposing (..)

import ContentEditable as ContentEditable
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
import Focusable
import Styles exposing (..)
import Util


type alias Name =
    ContentEditable.Model


type alias Model =
    { name : Name
    , args : ReorderList.Model Arg.Model
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
    , args = ReorderList.init (List.map (uncurry Arg.init) args)
    , body = body
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName msg ->
            model & functionName $= ContentEditable.update msg

        ReorderListMsg msg ->
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

        ArgMsg index argMsg ->
            model & args => ReorderList.elements => Focus.indexConcat index $= Arg.update argMsg

        ExpressionMsg exprMsg ->
            model & body $= Expression.update exprMsg

        AddArg ->
            model & args => ReorderList.elements $= (\args -> args ++ [ Arg.init "arg" Type.hole ])


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subscriptionForArg index arg =
            Sub.map (ArgMsg index) (Arg.subscriptions arg)
    in
    Sub.batch
        [ Sub.map ReorderListMsg (ReorderList.subscriptions model.args)
        , Sub.batch (List.indexedMap subscriptionForArg model.args.elements)
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
        wrapHighlightedDivider =
            Element.el DividerHighlight [ height (fill 1), paddingXY 4 0 ]

        settings =
            { nostyle = NoStyle
            , dividerSize = 30
            , orientation = Divider.Vertical
            }

        argElements =
            ReorderList.viewKeyed settings ReorderListMsg (viewArgs model.args.dragModel) model.args

        args =
            List.intersperse arrow argElements

        arrow =
            ( "arrow", wrapHighlightedDivider (Util.styledText Keyword "→") )

        hasType =
            ( "hasType", wrapHighlightedDivider (Util.styledText Keyword ":") )

        equalsSign =
            ( "equalsSign"
            , Element.column NoStyle
                []
                [ viewAddArg
                , Util.styledTextAttr Keyword [ alignBottom, padding 4 ] "="
                ]
            )
    in
    Keyed.row NoStyle [ spacing 10, padding 5 ] (viewName model :: hasType :: args ++ [ equalsSign ])


viewArgs : DragAndDrop.Model Int Int -> List Arg.Model -> List ( String, Element Styles Variations Msg )
viewArgs dragModel =
    let
        viewArg index arg =
            Arg.view dragModel arg & Focus.second => Element.map $= ArgMsg index
    in
    List.indexedMap viewArg


viewName : Model -> ( String, Element Styles Variations Msg )
viewName model =
    ( "FunctionName"
    , Element.column NoStyle
        [ spacing 5 ]
        [ Element.map UpdateName (ContentEditable.view Identifier model.name)
        , Util.styledText Identifier model.name.liveContent
        ]
    )


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


args : FieldSetter Model (ReorderList.Model Arg.Model)
args f model =
    { model | args = f model.args }


body : FieldSetter Model Expression.Model
body f model =
    { model | body = f model.body }
