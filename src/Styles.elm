module Styles exposing (..)

import Color
import Element exposing (Element)
import Html
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


type Variations
    = ActionbarButtonActive


type Styles
    = NoStyle
      -- general
    | Button
    | DragGrab
      -- Actionbar
    | Actionbar
    | ActionbarButton
    | ActionbarKey
      -- text
    | Keyword
    | Identifier
    | Literal
      -- types
    | TypeHole
    | TypeHoleText
    | TypeOptionList
      -- functions
    | DividerHighlight



-- functions


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet
        [ style NoStyle
            []

        -- general
        , style Button
            [ Style.prop "z-index" "2"
            , Style.prop "cursor" "pointer"
            , Color.background Color.lightGrey
            , hover [ Color.background (Color.rgb 200 230 255) ]
            ]
        , style DragGrab
            [ Color.text Color.darkGrey
            , Style.prop "cursor" "grab"
            ]

        -- ActionBar
        , style Actionbar
            [ Border.right 2
            , Color.border Color.darkGrey
            ]
        , style ActionbarButton
            [ Border.top 1
            , Border.bottom 1
            , Color.border Color.darkGrey
            , Font.typeface [ "Lucida Console", "Courier New" ]
            , Font.bold
            , Color.text (Color.rgb 80 80 80)
            , variation ActionbarButtonActive
                [ Color.background Color.lightGrey ]
            ]
        , style ActionbarKey
            [ Border.all 1
            , Border.rounded 5
            , Color.border Color.darkGrey
            , Color.background Color.lightGrey
            , Shadow.simple
            , Color.text Color.black
            , Font.typeface [ "Lucida Console", "Courier New" ]
            , Font.size 10
            ]

        -- Text styles
        , style Keyword
            [ Font.typeface [ "Lucida Console", "Courier New" ]
            , Color.text Color.lightBlue
            ]
        , style Identifier
            [ Font.typeface [ "Lucida Console", "Courier New" ]
            , Color.text (Color.rgb 30 30 30)
            ]
        , style Literal
            [ Font.typeface [ "Lucida Console", "Courier New" ]
            , Color.text Color.darkGreen
            ]

        -- Type styles
        , style TypeHoleText
            [ Font.typeface [ "Lucida Console", "Courier New" ]
            , Color.text Color.darkGrey
            ]
        , style TypeHole
            [ Border.all 1
            , Color.border Color.darkGrey
            ]
        , style TypeOptionList
            [ Color.background Color.lightGrey
            , Style.prop "z-index" "1"
            ]

        -- functions
        , style DividerHighlight
            [ Color.background (Color.rgb 255 251 233)
            , Style.prop "z-index" "-1"
            ]
        ]


type alias StyleElementsProgram model message =
    { init : ( model, Cmd message )
    , update : message -> model -> ( model, Cmd message )
    , subscriptions : model -> Sub message
    , view : model -> Element Styles Variations message
    }


styleElementsProgram : StyleElementsProgram model message -> Program Never model message
styleElementsProgram { init, update, subscriptions, view } =
    let
        viewElement model =
            Element.layout stylesheet (view model)
    in
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = viewElement
        }
