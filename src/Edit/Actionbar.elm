module Edit.Actionbar exposing (..)

import Actionbar
import Element exposing (Element)
import Styles exposing (..)


-- Model


type alias Model =
    Actionbar.Model Action


type alias Msg =
    Actionbar.Msg Action


type alias Event =
    Actionbar.Event Action


type Action
    = Drag
    | Add
    | Remove
    | Select


init : Model
init =
    Actionbar.init
        [ ( "Drag", 'D', Drag )
        , ( "Add", 'A', Add )
        , ( "Remove", 'R', Remove )
        , ( "Select", 'S', Select )
        ]



-- Querying


anyActive : Model -> Bool
anyActive model =
    case model.activeAction of
        Just _ ->
            True

        Nothing ->
            False


actionActive : Action -> Model -> Bool
actionActive action model =
    Just action == model.activeAction



-- Update


update : Msg -> Model -> Model
update =
    Actionbar.update


updateWithEvents : Msg -> Model -> ( Model, Maybe Event )
updateWithEvents =
    Actionbar.updateWithEvents


subscriptions : Model -> Sub Msg
subscriptions =
    Actionbar.subscriptions



-- View


viewTo : (Msg -> msg) -> Element Styles Variations msg -> Model -> Element Styles Variations msg
viewTo =
    Actionbar.viewTo
