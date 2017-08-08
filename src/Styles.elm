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
      -- text
    | Keyword
    | Identifier
      -- types
    | TypeHole
    | TypeHoleText
    | TypeOption
    | TypeOptionList


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet
        [ style NoStyle
            []

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
        , style TypeOption
            [ Style.prop "z-index" "2"
            , hover [ Color.background (Color.rgb 200 230 255) ]
            ]
        , style TypeOptionList
            [ Color.background Color.lightGray
            , Style.prop "z-index" "1"
            ]
        ]
