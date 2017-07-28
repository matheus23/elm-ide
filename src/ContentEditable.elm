module ContentEditable exposing (..)

import Element exposing (Element)
import Element.Attributes exposing (contenteditable, property)
import Element.Events exposing (on, onBlur, onFocus)
import Json.Decode as Decode
import Json.Encode as Encode
import Util


type alias Model =
    { liveContent : String
    , content : String
    }


type Msg
    = UserInput String
    | Blur


create : String -> Model
create content =
    { content = content, liveContent = content }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserInput newText ->
            { model | liveContent = newText }

        Blur ->
            { model | content = model.liveContent }


view : style -> Model -> Element style variation Msg
view style model =
    Element.el style
        [ contenteditable True
        , on "input" (Decode.map UserInput innerHtmlDecoder)
        , onBlur Blur

        -- placing this here, instead of using Html.text
        -- fixes a "cannot read property 'replaceData' of undefined"-error
        , property "innerText" (Encode.string model.content)
        ]
        Element.empty


tightView : style -> Model -> Element style variation Model
tightView style =
    Util.tightElement update (view style)


innerHtmlDecoder : Decode.Decoder String
innerHtmlDecoder =
    Decode.at [ "target", "innerHTML" ] Decode.string
