module Edit.Function exposing (..)

import ContentEditable as ContentEditable
import DragAndDrop
import DragAndDrop.Divider as Divider
import DragAndDrop.ReorderList as ReorderList
import Edit.Arg as Arg
import Edit.Type as Type
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Element.Keyed as Keyed
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Styles exposing (..)
import Tuple
import Util


type alias Name =
    ContentEditable.Model


type alias Model =
    { name : Name
    , args : ReorderList.Model Arg.Model
    }


type Msg
    = UpdateName ContentEditable.Msg
    | ReorderListMsg (ReorderList.Msg ArgsMsg)
    | AddArg


type ArgsMsg
    = UpdateArgIndex Int Arg.Msg



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName msg ->
            model & functionName $= ContentEditable.update msg

        ReorderListMsg msg ->
            model & args $= ReorderList.update updateArgs msg

        AddArg ->
            model & args => ReorderList.elements $= (\args -> args ++ [ Arg.init "arg" Type.hole ])


updateArgs : ArgsMsg -> List Arg.Model -> List Arg.Model
updateArgs (UpdateArgIndex index argMsg) =
    Focus.indexConcat index $= Arg.update argMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ReorderListMsg (ReorderList.subscriptions model.args)



-- View


view : Model -> Element Styles Variations Msg
view model =
    let
        wrapHighlightedDivider =
            Element.el DividerHighlight [ height (fill 1), paddingXY 4 0 ]

        settings =
            { nostyle = NoStyle
            , dividerSize = 30
            , orientation = Divider.Vertical
            , viewItems = viewArgs model.args.dragModel
            }

        argElements =
            ReorderList.viewKeyed settings model.args & List.map => Focus.second => Element.map $= ReorderListMsg

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


viewArgs : DragAndDrop.Model Int Int -> List Arg.Model -> List ( String, Element Styles Variations ArgsMsg )
viewArgs dragModel =
    let
        viewArg index arg =
            Arg.view dragModel arg & Focus.second => Element.map $= UpdateArgIndex index
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
