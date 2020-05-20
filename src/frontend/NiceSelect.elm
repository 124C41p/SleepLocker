module NiceSelect exposing (niceSelect, option, optionGroup, selectedValue, searchable, onUpdate, nullable)
import Html exposing (Html, Attribute, node)
import Html.Attributes exposing (attribute)
import Html.Events exposing (on)
import Json.Encode as Json
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)

type SelectItem = Option String | OptGroup { label: String, options: (List String) }

option : String -> SelectItem
option = Option

optionGroup : String -> List String -> SelectItem
optionGroup label options = OptGroup { label = label, options = options }

selectedValue : Maybe String -> Attribute msg
selectedValue = attribute "value" << Maybe.withDefault ""

searchable : Attribute msg
searchable = attribute "searchable" ""

nullable : Attribute msg
nullable = attribute "nullable" ""

encodeItem : SelectItem -> Json.Value
encodeItem item = case item of
    Option opt -> Json.object
        [ ("type", Json.string "option" )
        , ("value", Json.string opt)            
        ]
    OptGroup { label, options } -> Json.object
        [ ( "type", Json.string "optgrp" )
        , ( "label", Json.string label )
        , ( "value", Json.list Json.string options )
        ]

encodeItemList : List SelectItem -> String
encodeItemList = Json.list encodeItem >> Json.encode 0

niceSelect : List (Attribute msg) -> List SelectItem -> Html msg
niceSelect attrs items = node "nice-select" (attribute "items" (encodeItemList items) :: attrs) []

selectUpdatedDecoder : Decoder (Maybe String)
selectUpdatedDecoder =
    Decode.succeed identity
        |> requiredAt ["detail", "value"] (Decode.nullable Decode.string)

onUpdate : (Maybe String -> msg) -> Attribute msg
onUpdate handler =
    selectUpdatedDecoder
        |> Decode.map handler
        |> on "select-user-update"