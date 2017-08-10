module Actionbar exposing (..)

import Char
import Element exposing (Element)
import Element.Attributes exposing (..)
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Keyboard
import MultiTouch
import Styles exposing (..)
import Util


type alias ActionModel action =
    { actionId : action
    , text : String
    , keyCode : Keyboard.KeyCode
    }


type alias Model action =
    { actionbuttons : List (ActionModel action)
    , width : Float
    , activeAction : Maybe action
    }


type Msg action
    = Activate action
    | Deactivate action
    | OtherKeyPressed Keyboard.KeyCode


actionFromKeyCode : Model action -> (action -> Msg action) -> Keyboard.KeyCode -> Msg action
actionFromKeyCode model makeMsg keyCode =
    let
        hasKeyCode actionModel =
            actionModel.keyCode == keyCode
    in
    model.actionbuttons
        |> List.filter hasKeyCode
        |> List.head
        |> Maybe.map (.actionId >> makeMsg)
        |> Maybe.withDefault (OtherKeyPressed keyCode)


actionModelInit : ( String, Char, actions ) -> ActionModel actions
actionModelInit ( actionText, keyChar, actionId ) =
    { actionId = actionId
    , text = actionText
    , keyCode = Char.toCode keyChar
    }


init : List ( String, Char, actions ) -> Model actions
init actionList =
    { actionbuttons = List.map actionModelInit actionList
    , width = 200
    , activeAction = Nothing
    }



-- Update


update : Msg action -> Model action -> Model action
update msg model =
    let
        replaceNothingIfEqual value maybe =
            case maybe of
                Just sth ->
                    if value == sth then
                        Nothing
                    else
                        Just sth

                Nothing ->
                    Nothing
    in
    case msg of
        Activate actionId ->
            model & activeAction .= Just actionId

        Deactivate actionId ->
            model & activeAction $= replaceNothingIfEqual actionId

        OtherKeyPressed _ ->
            model


subscriptions : Model action -> Sub (Msg action)
subscriptions model =
    Sub.batch
        [ Keyboard.downs (actionFromKeyCode model Activate)
        , Keyboard.ups (actionFromKeyCode model Deactivate)
        ]



-- View


viewTo : (Msg action -> msg) -> Element Styles Variations msg -> Model action -> Element Styles Variations msg
viewTo injectMsg elementUnder model =
    Element.row NoStyle
        []
        [ Element.map injectMsg (view model)
        , Element.el NoStyle [ paddingLeft model.width ] elementUnder
        ]


view : Model action -> Element Styles Variations (Msg action)
view model =
    Element.screen <|
        Element.column Actionbar
            [ spacing 2
            , width (px model.width)
            ]
            (List.map (viewAction model.activeAction) model.actionbuttons)


viewAction : Maybe action -> ActionModel action -> Element Styles Variations (Msg action)
viewAction activeAction actionModel =
    Element.column ActionbarButton
        [ width (fill 1)
        , height (px 100)
        , padding 20
        , justify
        , center
        , spacing 10
        , toAttr (MultiTouch.onStart (always (Activate actionModel.actionId)))
        , toAttr (MultiTouch.onEnd (always (Deactivate actionModel.actionId)))
        , vary ActionbarButtonActive (Util.equalsMaybe actionModel.actionId activeAction)
        ]
        [ Element.el NoStyle
            [ center ]
            (Element.text actionModel.text)
        , Element.el ActionbarKey
            [ center, paddingXY 8 4 ]
            (Element.text (String.fromChar (Char.fromCode actionModel.keyCode)))
        ]



-- Lenses


actionId : Setter (ActionModel a) (ActionModel b) a b
actionId f actionModel =
    { actionModel | actionId = f actionModel.actionId }


text : FieldSetter (ActionModel action) String
text f actionModel =
    { actionModel | text = f actionModel.text }


keyCode : FieldSetter (ActionModel action) Keyboard.KeyCode
keyCode f actionModel =
    { actionModel | keyCode = f actionModel.keyCode }


actionbuttons : FieldSetter (Model action) (List (ActionModel action))
actionbuttons f model =
    { model | actionbuttons = f model.actionbuttons }


activeAction : FieldSetter (Model action) (Maybe action)
activeAction f model =
    { model | activeAction = f model.activeAction }



-- Test


loremipsum : String
loremipsum =
    String.concat <|
        List.repeat 10
            "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."


type Actions
    = Drag
    | Add
    | Remove


main : Program Never (Model Actions) (Msg Actions)
main =
    styleElementsProgram
        { init = init [ ( "Drag", 'D', Drag ), ( "Add", 'A', Add ), ( "Remove", 'R', Remove ) ] ! []
        , update = \msg model -> update msg model ! []
        , subscriptions = subscriptions
        , view = viewTo identity (Element.text loremipsum)
        }
