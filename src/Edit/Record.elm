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



-- View


view :
    (innerModel -> Element Styles Variations innerMsg)
    -> Model innerModel
    -> Element Styles Variations (Msg innerMsg)
view viewInner model =
    GroupLit.view (groupLitSettings viewInner) model


groupLitSettings :
    (innerModel -> Element Styles Variations innerMsg)
    -> GroupLit.Settings (Msg innerMsg) (Association.Model innerModel)
groupLitSettings viewInner =
    let
        prefixFor attributes index =
            if index == 0 then
                Util.styledTextAttr Keyword attributes "{"
            else
                Util.styledTextAttr Keyword attributes ","

        suffix =
            Util.styledText Keyword "}"

        viewInnerIndexed index =
            Element.map (GroupLit.UpdateIndex index) << Association.view viewInner
    in
    { prefixFor = prefixFor
    , suffix = suffix
    , viewInner = viewInnerIndexed
    }



-- Plain render


plain : (innerModel -> Element Styles Variations msg) -> Model innerModel -> Element Styles Variations msg
plain plainInner =
    GroupLit.view (plainGroupLitSettings plainInner)


plainGroupLitSettings : (innerModel -> Element Styles Variations msg) -> GroupLit.Settings msg (Association.Model innerModel)
plainGroupLitSettings plainInner =
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
    , viewInner = \_ -> Association.plain plainInner
    }



-- Smart constructors


model : Bool -> List ( String, a ) -> Model a
model oneline associations =
    { elements = List.map Association.model associations
    , oneline = oneline
    }
