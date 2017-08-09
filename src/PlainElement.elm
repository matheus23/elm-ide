module PlainElement exposing (..)

import Element exposing (..)


type alias PlainElement style variant =
    Element style variant Never


type alias PlainAttribute variant =
    Attribute variant Never


view : PlainElement style variant -> Element style variant msg
view =
    Element.map never


el : style -> List (PlainAttribute variant) -> PlainElement style variant -> PlainElement style variant
el =
    Element.el
