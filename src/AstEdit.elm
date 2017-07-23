module AstEdit exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Focus exposing (..)
import Styles exposing (..)
import Util


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
    }


args : Setter Model Model (List ( Var, Type )) (List ( Var, Type ))
args f model =
    { model | args = f model.args }


type Msg
    = UpdateName ContentEditable.Msg
    | UpdateType Int Type
    | UpdateVar Int ContentEditable.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName msg ->
            { model | name = ContentEditable.update msg model.name }

        UpdateType index typ ->
            model & args => Util.index index => Util.snd .= typ

        UpdateVar index msg ->
            model & args => Util.index index => Util.fst $= ContentEditable.update msg


view : Model -> Element Styles Variations Msg
view model =
    let
        argElements =
            viewArgs model.args

        args =
            List.intersperse arrow argElements

        arrow =
            Util.styledText Keyword "→"

        hasType =
            Util.styledText Keyword ":"
    in
    Element.row StdStyle [ spacing 10, padding 5 ] (viewName model :: hasType :: args)


viewArgs : List ( Var, Type ) -> List (Element Styles Variations Msg)
viewArgs args =
    List.indexedMap viewArg args


viewArg : Int -> ( Var, Type ) -> Element Styles Variations Msg
viewArg index ( var, typ ) =
    Element.column StdStyle [ spacing 4 ] [ viewType index typ, viewVar index var ]


viewType : Int -> Type -> Element Styles Variations Msg
viewType index typ =
    Util.styledText Identifier (toString typ)


viewVar : Int -> Var -> Element Styles Variations Msg
viewVar index var =
    Element.map (UpdateVar index) (ContentEditable.view Identifier var)


viewName : Model -> Element Styles Variations Msg
viewName model =
    Element.column StdStyle
        [ spacing 4 ]
        [ Element.map UpdateName (ContentEditable.view Identifier model.name)
        , Util.styledText Identifier model.name.liveContent
        ]
