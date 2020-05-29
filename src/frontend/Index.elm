module Index exposing (main)

import Html exposing (Html, div, text, button, span, input, label, h4, textarea)
import Html.Attributes exposing (class, style, attribute, placeholder, maxlength, value, rows, disabled, classList, id)
import NiceSelect exposing (niceSelect, option, selectedValue, nullable, onUpdate)
import Html.Events exposing (onClick, onInput)
import Helpers exposing (expectResponse, raidUserKeyEncoder, focus)
import Maybe.Extra as MaybeX
import Browser
import Browser.Navigation as Navigation
import Http
import Json.Decode as Decode
import Json.Encode as Encode

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
    , comments : String
    }

isInvalid : Raid -> Bool
isInvalid { title } =
    String.isEmpty <| String.trim title

init : Flags -> (Model, Cmd Msg)
init lootTables =
    (Model Empty lootTables, Cmd.none)


type Msg
    = NoOp
    | ToggleCreate
    | ToggleJoin
    | UpdateCreate Raid (Maybe String)
    | UpdateJoin String (Maybe String)
    | CheckAndJoin String
    | DoJoin String
    | DoCreate Raid
    | DoAdministrate String

raidEncoder : Raid -> Encode.Value
raidEncoder raid =
    Encode.object
        [ ( "title", Encode.string (String.trim raid.title) )
        , ( "dungeonKey", MaybeX.unwrap Encode.null (\d -> Encode.string d.dungeonKey) raid.dungeon)
        ,
            ( "comments"
            ,
                if String.isEmpty (String.trim raid.comments) then
                    Encode.null
                else
                    Encode.string raid.comments
            )
        ]

checkRaid : String -> Cmd Msg
checkRaid raidID =
    Http.post
        { url = "/api/getRaid"
        , body = Http.jsonBody (raidUserKeyEncoder raidID)
        , expect = expectResponse
            ( \res -> case res of
                Err errMsg -> UpdateJoin raidID (Just errMsg)
                Ok () -> DoJoin raidID
            )
            Nothing
            (Decode.succeed ())
        }
        
createRaid : Raid -> Cmd Msg
createRaid raid =
    Http.post
        { url = "/api/createRaid"
        , body = Http.jsonBody (raidEncoder raid)
        , expect = expectResponse
            ( \res -> case res of
                Err errMsg -> UpdateCreate raid (Just errMsg)
                Ok raidAdminKey -> DoAdministrate raidAdminKey
            )
            Nothing
            Decode.string
        }
        
navigateKey : String -> Cmd Msg
navigateKey key = Navigation.load("/" ++ key)

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
                _ -> ({ model | state = Create (Raid "" Nothing "") Nothing }, focus "input-raid-title" NoOp)
        UpdateCreate raid err ->
            ({ model | state = Create raid err }, Cmd.none)
        UpdateJoin newRaidID err ->
            ({ model | state = Join newRaidID err }, Cmd.none)
        CheckAndJoin raidID ->
            ({ model | state = Joining raidID }, checkRaid raidID)
        DoJoin raidID ->
            (model, navigateKey raidID)
        DoCreate raid ->
            ({ model | state = Creating raid }, createRaid raid)
        DoAdministrate raidAdminKey ->
            (model, navigateKey raidAdminKey)


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
                    , viewError err
                    , div [ class "form-group" ]
                        [ label [] [ text "Titel" ]
                        , input
                            [ class "form-control"
                            , id "input-raid-title"
                            , value title
                            , maxlength 50
                            , onInput <| \newTitle -> UpdateCreate { raid | title = newTitle } Nothing
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
                                in  UpdateCreate { raid | dungeon = newDungeon } Nothing
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
                                UpdateCreate { raid | comments = newComments } Nothing
                            ]
                            []
                        ]
                    , button
                        [ class "btn"
                        , class "btn-primary"
                        , disabled (isInvalid raid)
                        , onClick (DoCreate raid)
                        ]
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