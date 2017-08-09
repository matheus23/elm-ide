module Edit.Function exposing (..)

import ContentEditable as ContentEditable
import DragAndDrop
import DragAndDrop.Divider as Divider
import DragAndDrop.ReorderList as ReorderList
import Edit.Type as Type
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Keyed as Keyed
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Styles exposing (..)
import Tuple
import Util


type alias Name =
    ContentEditable.Model


type alias ArgName =
    ContentEditable.Model


type alias Model =
    { name : Name
    , args : ReorderList.Model ( ArgName, Type.Model )
    }


type Msg
    = UpdateName ContentEditable.Msg
    | ReorderListMsg (ReorderList.Msg ArgsMsg)


type ArgsMsg
    = UpdateType Int Type.Msg
    | UpdateVar Int ContentEditable.Msg



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName msg ->
            model & functionName $= ContentEditable.update msg

        ReorderListMsg msg ->
            model & args $= ReorderList.update updateArgs msg


updateArgs : ArgsMsg -> List ( ArgName, Type.Model ) -> List ( ArgName, Type.Model )
updateArgs msg args =
    case msg of
        UpdateType index typeMsg ->
            args & Focus.index index => Focus.second $= Type.update typeMsg

        UpdateVar index msg ->
            args & Focus.index index => Focus.first $= ContentEditable.update msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ReorderListMsg (ReorderList.subscriptions model.args)



-- View


view : Model -> Element Styles Variations Msg
view model =
    let
        settings =
            { nostyle = NoStyle
            , dividerSize = 30
            , orientation = Divider.Vertical
            , viewItems = viewArgs model.args.dragModel
            }

        argElements =
            ReorderList.view settings model.args
                |> Util.zip (List.map (.liveContent << Tuple.first) model.args.elements)
                |> (List.map => Focus.second => Element.map $= ReorderListMsg)

        args =
            List.intersperse arrow argElements

        arrow =
            ( "arrow", Util.styledText Keyword "→" )

        hasType =
            ( "hasType", Util.styledText Keyword ":" )

        equalsSign =
            ( "equalsSign", Util.styledTextAttr Keyword [ paddingLeft 4, alignBottom ] "=" )
    in
    Keyed.row NoStyle [ spacing 10, padding 5 ] (viewName model :: hasType :: args ++ [ equalsSign ])


viewArgs : DragAndDrop.Model Int Int -> List ( ArgName, Type.Model ) -> List (Element Styles Variations ArgsMsg)
viewArgs dragModel args =
    List.indexedMap (viewArg dragModel) args


viewArg : DragAndDrop.Model Int Int -> Int -> ( ArgName, Type.Model ) -> Element Styles Variations ArgsMsg
viewArg dragModel index ( argName, typ ) =
    Element.column NoStyle
        [ spacing 2 ]
        [ Element.map (UpdateType index) (Type.view typ)
        , viewArgName index argName
        ]


viewArgName : Int -> ArgName -> Element Styles Variations ArgsMsg
viewArgName index argName =
    Element.map (UpdateVar index) (ContentEditable.viewAttr [ alignBottom ] Identifier argName)


viewName : Model -> ( String, Element Styles Variations Msg )
viewName model =
    ( "FunctionName"
    , Element.column NoStyle
        [ spacing 5 ]
        [ Element.map UpdateName (ContentEditable.view Identifier model.name)
        , Util.styledText Identifier model.name.liveContent
        ]
    )



-- Smart Constructors


argName : String -> ArgName
argName =
    ContentEditable.create


name : String -> Name
name =
    ContentEditable.create



-- Lenses


functionName : FieldSetter Model Name
functionName f model =
    { model | name = f model.name }


args : FieldSetter Model (ReorderList.Model ( ArgName, Type.Model ))
args f model =
    { model | args = f model.args }
