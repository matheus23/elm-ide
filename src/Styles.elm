module Styles exposing (..)

import Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


type alias Variations =
    Never


type Styles
    = StdStyle
    | TextArea
    | VarInput
    | Keyword
    | Identifier


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet
        [ style TextArea
            [ Font.typeface [ "Lucida Console" ]
            , Border.all 1
            ]
        , style Keyword
            [ Font.typeface [ "Lucida Console" ]
            , Color.text Color.lightBlue
            ]
        , style Identifier [ Font.typeface [ "Lucida Console" ] ]
        ]
