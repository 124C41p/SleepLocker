module Index exposing (main)

import Html exposing (Html, div, text, button, span, input, label, h4, textarea)
import Html.Attributes exposing (class, style, attribute, placeholder, maxlength, value, rows, disabled)
import NiceSelect exposing (niceSelect, option, selectedValue, nullable, onUpdate)
import Html.Events exposing (onClick, onInput)
import Helpers exposing (expectResponse)
-- import Maybe.Extra as MaybeX
import Browser

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { state : State
    , lootTables : List Dungeon
    }

type State
    = Empty
    | Join String (Maybe String)
    | Joining String
    | Create Raid (Maybe String)
    | Creating Raid

type alias Dungeon =
    { dungeonName : String
    , dungeonKey : String
    }

dungeonFromName : List Dungeon -> String -> Maybe Dungeon
dungeonFromName dungeonList name =
    List.filter (\d -> d.dungeonName == name) dungeonList
    |> List.head

type alias Flags = List Dungeon

isLoading : State -> Bool
isLoading state =
    case state of
        Joining _ -> True
        Creating _ -> True
        _ -> False

type alias Raid =
    { title : String
    , dungeon : Maybe Dungeon
    , comments : Maybe String
    }

isInvalid : Raid -> Bool
isInvalid { title } =
    String.isEmpty <| String.trim title

init : Flags -> (Model, Cmd Msg)
init lootTables =
    (Model Empty lootTables, Cmd.none)


type Msg
    = ToggleCreate
    | ToggleJoin
    | UpdateCreate Raid
    | UpdateJoin String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggleJoin ->
            case model.state of
                Join _ _ -> ({ model | state = Empty }, Cmd.none)
                _ -> ({ model | state = Join "" Nothing }, Cmd.none)
        ToggleCreate ->
            case model.state of
                Create _ _ -> ({ model | state = Empty }, Cmd.none)
                _ -> ({ model | state = Create (Raid "" Nothing Nothing) Nothing }, Cmd.none)
        UpdateCreate raid ->
            ({ model | state = Create raid Nothing }, Cmd.none)
        UpdateJoin newRaidID ->
            ({ model | state = Join newRaidID Nothing }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view { state, lootTables } =
    div [ class "container" ] <|
         if isLoading state then
            [ viewSpinner ]
         else
            [ viewButtons
            , case state of
                Join key error -> viewJoin key error
                Create raid error -> viewCreate raid lootTables error
                _ -> div [] []
            ]

viewButtons : Html Msg
viewButtons =
    div [ class "row", class "d-flex", class "justify-content-center", class "mt-5" ]
    [ div [ class "col", class "d-flex", class "justify-content-center" ]
        [ div [ class "btn-group" ]
            [ button [ class "btn", class "btn-primary", class "btn-lg", onClick ToggleJoin ] [ text "Raid beitreten" ]
            , button [ class "btn", class "btn-secondary", class "btn-lg", onClick ToggleCreate ] [ text "Raid erstellen" ]
            ]
        ]
    ]

viewSpinner : Html Msg
viewSpinner =
    div [ class "row", class "d-flex", class "justify-content-center", class "mt-5" ]
    [ div [ class "col", class "d-flex", class "justify-content-center" ]
        [ div
            [ class "spinner-border", class "text-primary", attribute "role" "status"
            , style "width" "3rem", style "height" "3rem", class "mt-5", class "mb-5"
            ]
            [ span [ class "sr-only" ] [ text "Lade..." ] ]
        ]
    ]

viewJoin : String -> Maybe String -> Html Msg
viewJoin raidID err = 
    div [ class "row", class "d-flex", class "justify-content-center", class "mt-5" ]
    [ div [ class "col-md-3", class "d-flex", class "justify-content-center" ]
        [ div [ class "input-group" ]
            [ input [ placeholder "Raid ID", class "form-control", maxlength 6 ] []
            , div [ class "input-group-append" ]
                [ button [ class "btn", class "btn-primary" ] [ text "Beitreten" ]
                ]
            ]
        ]
    ]

viewCreate : Raid -> List Dungeon -> Maybe String -> Html Msg
viewCreate raid dungeonList err =
    let
        { title, dungeon, comments } = raid
    in
        div [ class "row", class "d-flex", class "justify-content-center", class "mt-5" ]
        [ div [ class "col-md-8" ]
            [ div [ class "card" ]
                [ div [ class "card-body" ]
                    [ h4 [ class "card-title", class "text-center" ] [ text "Neuer Raid" ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Titel" ]
                        , input
                            [ class "form-control"
                            , value title
                            , onInput <| \newTitle -> UpdateCreate { raid | title = newTitle }
                            ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label [ ] [ text "Loot-Tabelle" ]
                        , niceSelect
                            [ selectedValue <| Maybe.map (\d -> d.dungeonName) dungeon
                            , nullable
                            , onUpdate <| \name ->
                                let
                                    newDungeon = Maybe.andThen (dungeonFromName dungeonList) name
                                in  UpdateCreate { raid | dungeon = newDungeon }
                            ]
                            (List.map (\d -> option d.dungeonName) dungeonList)
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Bemerkungen" ]
                        , textarea
                            [ class "form-control"
                            , value <| Maybe.withDefault "" comments
                            , rows 5
                            , onInput <| \newComments ->
                                UpdateCreate 
                                    { raid | comments =
                                        if String.isEmpty newComments then 
                                            Nothing
                                        else Just newComments
                                    }
                            ]
                            []
                        ]
                    , button
                        [ class "btn"
                        , class "btn-primary"
                        , disabled (isInvalid raid)
                        ]
                        [ text "Erstellen" ]
                    ]
                ]
            ]
        ]

viewErrorMsg : String -> Html Msg
viewErrorMsg err =
    div [ class "alert", class "alert-danger" ]
        [ span [] [ text err ]
        ]