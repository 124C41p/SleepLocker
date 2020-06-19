module Index exposing (main)

import Html exposing (Html, div, text, button, span, input, label, h4, textarea)
import Html.Attributes as Attr exposing (class, style, attribute, placeholder, maxlength, value, rows, disabled, classList, id, type_)
import NiceSelect exposing (niceSelect, option, selectedValue, nullable, onUpdate)
import Html.Events exposing (onClick, onInput)
import Helpers exposing (focus)
import Maybe.Extra as MaybeX
import Browser
import Api exposing (RaidAdminKey, RaidUserKey, Raid, navigateAdminPage, navigateUserPage, createRaid, validateRaidUserKey)

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
    | Create FormData (Maybe String)
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

type alias FormData =
    { title : String
    , dungeon : Maybe Dungeon
    , comments : String
    , numPriorities : Int
    }

emptyFormData : FormData
emptyFormData =
    { title = ""
    , dungeon = Nothing
    , comments = ""
    , numPriorities = 2
    }

validateFormData : FormData -> Maybe Raid
validateFormData { title, dungeon, comments, numPriorities } =
    let
        trimmedTitle = String.trim title
    in
        if String.length trimmedTitle == 0 then
            Nothing
        else Just
            { title = trimmedTitle
            , dungeonKey = Maybe.map (\d -> d.dungeonKey) dungeon
            , numPriorities = numPriorities
            , comments =
                if String.isEmpty (String.trim comments) then
                    Nothing
                else
                    Just comments
            }

invalidateRaid : List Dungeon -> Raid -> FormData
invalidateRaid dungeonList raid =
    { title = raid.title
    , dungeon = raid.dungeonKey
        |> Maybe.andThen
            (\key ->
                List.filter (\d -> d.dungeonKey == key) dungeonList
                |> List.head
            )
    , comments = Maybe.withDefault "" raid.comments
    , numPriorities = raid.numPriorities
    }

init : Flags -> (Model, Cmd Msg)
init lootTables =
    (Model Empty lootTables, Cmd.none)


type Msg
    = NoOp
    | ToggleCreate
    | ToggleJoin
    | UpdateCreate FormData (Maybe String)
    | UpdateJoin String (Maybe String)
    | CheckAndJoin String
    | DoJoin RaidUserKey
    | DoCreate Raid
    | DoAdministrate RaidAdminKey

checkRaid : String -> Cmd Msg
checkRaid raidID =
    validateRaidUserKey raidID
        ( \res -> case res of
                Err errMsg -> UpdateJoin raidID (Just errMsg)
                Ok confirmedID -> DoJoin confirmedID
        )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        ToggleJoin ->
            case model.state of
                Join _ _ -> ({ model | state = Empty }, Cmd.none)
                _ -> ({ model | state = Join "" Nothing }, focus "input-join" NoOp)
        ToggleCreate ->
            case model.state of
                Create _ _ -> ({ model | state = Empty }, Cmd.none)
                _ -> ({ model | state = Create emptyFormData Nothing }, focus "input-raid-title" NoOp)
        UpdateCreate data err ->
            ({ model | state = Create data err }, Cmd.none)
        UpdateJoin newRaidID err ->
            ({ model | state = Join newRaidID err }, Cmd.none)
        CheckAndJoin raidID ->
            ({ model | state = Joining raidID }, checkRaid raidID)
        DoJoin raidID ->
            (model, navigateUserPage raidID)
        DoCreate raid ->
            ( { model | state = Creating raid }
            , createRaid raid
                ( \res -> case res of
                    Err errMsg -> UpdateCreate (invalidateRaid model.lootTables raid) (Just errMsg)
                    Ok raidAdminKey -> DoAdministrate raidAdminKey
                )
            )
        DoAdministrate raidAdminKey ->
            (model, navigateAdminPage raidAdminKey)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view { state, lootTables } =
    div [ class "container", class "py-5" ] <|
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
            [ input
                [ placeholder "Raid ID"
                , id "input-join"
                , class "form-control"
                , maxlength 6
                , value raidID
                , classList [ ("is-invalid", MaybeX.isJust err) ]
                , onInput (\newRaidID -> UpdateJoin newRaidID Nothing)
                ]
                [ ]
            , div [ class "input-group-append" ]
                [ button
                    [ class "btn"
                    , class "btn-primary"
                    , onClick <| CheckAndJoin raidID
                    ]
                    [ text "Beitreten" ]
                ]
            , div [ class "invalid-feedback" ] [ text <| Maybe.withDefault "" err ]
            ]
        ]
    ]

viewCreate : FormData -> List Dungeon -> Maybe String -> Html Msg
viewCreate data dungeonList err =
    let
        { title, dungeon, comments, numPriorities } = data
    in
        div [ class "row", class "d-flex", class "justify-content-center", class "mt-5" ]
        [ div [ class "col-md-8" ]
            [ div [ class "card" ]
                [ div [ class "card-body" ]
                    [ h4 [ class "card-title", class "text-center" ] [ text "Neuer Raid" ]
                    , viewError err
                    , div [ class "form-group" ]
                        [ label [] [ text "Titel" ]
                        , input
                            [ class "form-control"
                            , id "input-raid-title"
                            , value title
                            , maxlength 50
                            , onInput <| \newTitle -> UpdateCreate { data | title = newTitle } Nothing
                            ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text ("Anzahl Softlocks: " ++ String.fromInt numPriorities ) ]
                        , input
                            [ type_ "range"
                            , class "custom-range"
                            , value (String.fromInt numPriorities)
                            , Attr.min "1"
                            , Attr.max "5"
                            , onInput
                                ( \newValue ->
                                    case String.toInt newValue of
                                        Just newNumPriorities -> UpdateCreate { data | numPriorities = newNumPriorities } Nothing
                                        Nothing -> NoOp
                                )
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
                                in  UpdateCreate { data | dungeon = newDungeon } Nothing
                            ]
                            (List.map (\d -> option d.dungeonName) dungeonList)
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Bemerkungen" ]
                        , textarea
                            [ class "form-control"
                            , value comments
                            , rows 5
                            , maxlength 1000
                            , onInput <| \newComments ->
                                UpdateCreate { data | comments = newComments } Nothing
                            ]
                            []
                        ]
                    , button
                        ( case validateFormData data of
                            Nothing -> [class "btn", class "btn-primary", disabled True]
                            Just raid -> [class "btn", class "btn-primary", onClick (DoCreate raid)]
                        )
                        [ text "Erstellen" ]
                    ]
                ]
            ]
        ]

viewError : Maybe String -> Html Msg
viewError err =
    case err of
        Just errMsg ->
            div [ class "alert", class "alert-danger" ]
                [ span [] [ text errMsg ]
                ]
        Nothing -> div [] []