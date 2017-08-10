module Edit.Expression exposing (..)

import ContentEditable as ContentEditable
import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Focusable
import PlainElement exposing (PlainAttribute, PlainElement)
import Styles exposing (..)
import Util


-- Model


type alias Model =
    Focusable.Model { exprCase : Case }


type Case
    = Application Model (List Model)
    | Hole
    | IntLiteral Int


type Msg
    = FocusMsg Focusable.Msg


application : Model -> List Model -> Model
application func args =
    { focused = False, exprCase = Application func args }


intLiteral : Int -> Model
intLiteral n =
    { focused = False, exprCase = IntLiteral n }


hole : Model
hole =
    { focused = False, exprCase = Hole }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        FocusMsg focusMsg ->
            Focusable.update focusMsg model



-- View


view : Model -> Element Styles Variations Msg
view model =
    Focusable.wrapFocusable NoStyle FocusMsg <|
        case model.exprCase of
            Hole ->
                renderTypeHole []

            IntLiteral i ->
                Util.styledText Literal (toString i)

            Application funcExpr arguments ->
                Element.row NoStyle [ spacing 4 ] (view funcExpr :: List.map view arguments)



-- Render


renderTypeHole : List (Element.Attribute Variations msg) -> Element Styles Variations msg
renderTypeHole events =
    Element.el TypeHole
        ([ center
         , paddingXY 4 0
         ]
            ++ events
        )
        (Util.styledText TypeHoleText "?")
