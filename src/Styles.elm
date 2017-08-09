module Styles exposing (..)

import Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


type alias Variations =
    Never


type Styles
    = NoStyle
      -- general
    | Button
    | DragGrab
      -- text
    | Keyword
    | Identifier
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

        -- Text styles
        , style Keyword
            [ Font.typeface [ "Lucida Console", "Courier New" ]
            , Color.text Color.lightBlue
            ]
        , style Identifier
            [ Font.typeface [ "Lucida Console", "Courier New" ]
            , Color.text (Color.rgb 30 30 30)
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
