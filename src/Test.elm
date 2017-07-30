module Test exposing (..)

import Html exposing (Html)
import Styles exposing (..)
import Util


type Model
    = Bool
    | Int


type alias Msg =
    Model


updateCmd : Msg -> Model -> ( Model, Cmd msg )
updateCmd msg model =
    msg ! []


view : Model -> Html Msg
view model =
    let
        renderRecord :
            (Int -> ( Int, Int ) -> Html msg)
            -> List ( Int, Int )
            -> Html msg
        renderRecord renderAssoc associations =
            let
                assocsRendered =
                    List.indexedMap renderAssoc associations
            in
            Html.div []
                assocsRendered

        renderAssociation :
            ( Int, Int )
            -> Html msg
        renderAssociation ( a, b ) =
            Html.div []
                [ Html.text (toString a)
                , Html.text (toString b)
                ]

        plainRecord : List ( Int, Int ) -> Html msg
        plainRecord =
            renderRecord (\_ -> plainAssociation)

        plainAssociation : ( Int, Int ) -> Html msg
        plainAssociation ( x, y ) =
            Html.text (toString ( x, y ))

        plainAssociationIndex : Int -> ( Int, Int ) -> Html msg
        plainAssociationIndex index =
            plainAssociation
    in
    plainRecord [ ( 1, 2 ), ( 3, 4 ) ]


main : Program Never Model Msg
main =
    Html.program
        { init = Bool ! []
        , update = updateCmd
        , subscriptions = always Sub.none
        , view = view
        }
