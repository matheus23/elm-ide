module Edit.Playground exposing (..)

import Edit.Actionbar as Actionbar
import Edit.Function as Function
import Element exposing (Element)
import Element.Attributes exposing (..)
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Styles exposing (..)
import Util


-- Model


type alias Model =
    { function : Function.Model
    , actionbar : Actionbar.Model
    }


type Msg
    = FunctionMsg Function.Msg
    | ActionbarMsg Actionbar.Msg


init : Function.Model -> Model
init function =
    { function = function
    , actionbar = Actionbar.init
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FunctionMsg msg ->
            (model & function $= Function.update msg) ! []

        ActionbarMsg msg ->
            let
                ( newActionbar, maybeActionbarEvent ) =
                    Actionbar.updateWithEvents msg model.actionbar
            in
            (model
                |> (actionbar .= newActionbar)
                |> (function $= Util.onMaybe (Function.update << Function.ActionbarEvent) maybeActionbarEvent)
            )
                ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ActionbarMsg (Actionbar.subscriptions model.actionbar)
        , Sub.map FunctionMsg (Function.subscriptions model.function)
        ]



-- View


view : Model -> Element Styles Variations Msg
view model =
    Actionbar.viewTo ActionbarMsg
        (Element.map FunctionMsg
            (Element.el NoStyle
                [ paddingLeft 10 ]
                (Function.view model.actionbar model.function)
            )
        )
        model.actionbar



-- Lenses


function : FieldSetter Model Function.Model
function f model =
    { model | function = f model.function }


actionbar : FieldSetter Model Actionbar.Model
actionbar f model =
    { model | actionbar = f model.actionbar }
