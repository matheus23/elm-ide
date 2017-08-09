module Edit.Record exposing (..)

-- A Record is basically just a Edit.GroupLit with Edit.Associations inside.
-- The GroupLit is configured to look like this:
-- { _, _, _ }
-- or this:
-- { _
-- , _
-- }
-- (the _s are going to be Edit.Associations then)

import Edit.Association as Association
import Edit.GroupLit as GroupLit
import Element as Element exposing (Element)
import FocusMore as Focus exposing (FieldSetter)
import PlainElement exposing (PlainElement)
import Styles exposing (..)
import Util


-- Model


type alias Model a =
    GroupLit.Model (Association.Model a)


type alias Msg innerMsg =
    GroupLit.Msg (Association.Msg innerMsg)



-- Update


update : (innerMsg -> innerModel -> innerModel) -> Msg innerMsg -> Model innerModel -> Model innerModel
update updateInner =
    GroupLit.update (Association.update updateInner)


subscriptions : (a -> Sub innerMsg) -> Model a -> Sub (Msg innerMsg)
subscriptions innerSubs =
    GroupLit.subscriptions (Association.subscriptions innerSubs)



-- View


view :
    (innerModel -> Element Styles Variations innerMsg)
    -> Model innerModel
    -> Element Styles Variations (Msg innerMsg)
view viewInner model =
    GroupLit.view groupLitSettings (Association.view viewInner) model


groupLitSettings : GroupLit.Settings
groupLitSettings =
    let
        prefixFor attributes index =
            if index == 0 then
                Util.styledTextAttr Keyword attributes "{"
            else
                Util.styledTextAttr Keyword attributes ","

        suffix =
            Util.styledText Keyword "}"
    in
    { prefixFor = prefixFor
    , suffix = suffix
    }



-- Plain render


plain : (innerModel -> PlainElement Styles Variations) -> Model innerModel -> Element Styles Variations msg
plain plainInner model =
    PlainElement.view (GroupLit.plain groupLitSettings (Association.plain plainInner) model)



-- Smart constructors


init : Bool -> List ( String, a ) -> Model a
init oneline associations =
    GroupLit.init oneline (List.map Association.init associations)
