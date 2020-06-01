module Admin exposing (main)

import Browser
import Helpers exposing (expectResponse)
import Html exposing (Html, button, div, input, label, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, disabled, scope, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe.Extra as MaybeX
import Result.Extra as ResultX
import Time


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { raidAdminKey : String
    , raidUserKey : String
    , userList : List UserData
    , errorMessage : Maybe String
    , raidMode : Maybe RaidMode
    }


type alias UserData =
    { userName : String
    , class : String
    , role : String
    , registeredOn : String
    }


userDataDecoder : Decoder UserData
userDataDecoder =
    Decode.map4 UserData
        (Decode.field "userName" Decode.string)
        (Decode.field "class" Decode.string)
        (Decode.field "role" Decode.string)
        (Decode.field "registeredOn" Decode.string)


type RaidMode
    = Registration
    | Released


raidModeEncoder : RaidMode -> Encode.Value
raidModeEncoder mode =
    case mode of
        Registration ->
            Encode.int 0

        Released ->
            Encode.int 1


raidModeDecoder : Decoder RaidMode
raidModeDecoder =
    Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed Registration

                    1 ->
                        Decode.succeed Released

                    _ ->
                        Decode.fail "Ungültiger Raidmodus."
            )


type alias RaidStatus =
    { registrations : List UserData
    , raidMode : RaidMode
    }


raidStatusDecoder : Decoder RaidStatus
raidStatusDecoder =
    Decode.map2 RaidStatus
        (Decode.field "registrations" (Decode.list userDataDecoder))
        (Decode.field "raidMode" raidModeDecoder)


type alias Flags =
    { raidAdminKey : String
    , raidUserKey : String
    }


init : Flags -> ( Model, Cmd Msg )
init { raidAdminKey, raidUserKey } =
    ( { raidAdminKey = raidAdminKey, raidUserKey = raidUserKey, userList = [], errorMessage = Nothing, raidMode = Nothing }
    , loadRegistrations raidAdminKey
    )


type Msg
    = DoUpdate
    | Updated RaidStatus
    | DoSetMode RaidMode
    | ConnectionLost
    | ModeSet RaidMode


loadRegistrations : String -> Cmd Msg
loadRegistrations raidAdminKey =
    Http.post
        { url = "/api/getRaidStatus"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "raidAdminKey", Encode.string raidAdminKey )
                    ]
        , expect =
            expectResponse
                (ResultX.unwrap ConnectionLost Updated)
                Nothing
                raidStatusDecoder
        }


setMode : String -> RaidMode -> Cmd Msg
setMode raidAdminKey mode =
    Http.post
        { url = "/api/setMode"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "raidAdminKey", Encode.string raidAdminKey )
                    , ( "mode", raidModeEncoder mode )
                    ]
        , expect =
            expectResponse
                (\res ->
                    case res of
                        Err _ ->
                            ConnectionLost

                        Ok () ->
                            ModeSet mode
                )
                Nothing
                (Decode.null ())
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectionLost ->
            ( { model | raidMode = Nothing, errorMessage = Just "Verbindung zum Server verloren..." }, Cmd.none )
        DoUpdate ->
            ( model, loadRegistrations model.raidAdminKey )

        Updated raidStatus ->
            ( { model | userList = raidStatus.registrations, raidMode = Just raidStatus.raidMode, errorMessage = Nothing }, Cmd.none )

        DoSetMode mode ->
            ( { model | errorMessage = Nothing, raidMode = Nothing }, setMode model.raidAdminKey mode )

        ModeSet mode ->
            ( { model | raidMode = Just mode, errorMessage = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 5000 (\_ -> DoUpdate)


view : Model -> Html Msg
view model =
    div [ class "container-fluid", class "pb-5" ]
        [ div [ class "row" ]
            [ div [ class "col-md-2" ] [ viewControls model.raidUserKey model.errorMessage model.raidMode ]
            , div [ class "col-md-10" ] [ viewUserList model.userList ]
            ]
        ]


viewControls : String -> Maybe String -> Maybe RaidMode -> Html Msg
viewControls raidUserKey errorMsg mode =
    div [ class "card" ]
        [ div [ class "card-body" ]
            [ MaybeX.unwrap (div [] []) viewErrorMsg errorMsg
            , div [ class "form-group" ]
                [ label [] [ text "Raid ID" ]
                , input [ class "form-control", disabled True, value raidUserKey ] []
                ]
            , case mode of
                Nothing ->
                    viewSpinner

                Just m ->
                    viewButtons m
            ]
        ]


viewButtons : RaidMode -> Html Msg
viewButtons mode =
    div [ class "list-group" ]
        [ button
            [ class "list-group-item"
            , class "list-group-item-action"
            , classList [ ( "active", mode == Registration ) ]
            , onClick (DoSetMode Registration)
            ]
            [ text "1: Anmeldephase" ]
        , button
            [ class "list-group-item"
            , class "list-group-item-action"
            , classList [ ( "active", mode == Released ) ]
            , onClick (DoSetMode Released)
            ]
            [ text "2: Veröffentlichungsphase" ]
        ]


viewUserList : List UserData -> Html Msg
viewUserList userList =
    table [ class "table", class "table-striped", class "table-bordered" ]
        [ thead []
            [ th [ scope "col" ] [ text "#" ]
            , th [ scope "col" ] [ text "Name" ]
            , th [ scope "col" ] [ text "Klasse" ]
            , th [ scope "col" ] [ text "Rolle" ]
            , th [ scope "col" ] [ text "Anmeldezeitpunkt" ]
            ]
        , tbody [] (List.indexedMap viewUserRow userList)
        ]


viewUserRow : Int -> UserData -> Html Msg
viewUserRow index userData =
    tr []
        [ th [ scope "row" ] [ text (String.fromInt (index + 1)) ]
        , td [] [ text userData.userName ]
        , td [] [ text userData.class ]
        , td [] [ text userData.role ]
        , td [] [ text userData.registeredOn ]
        ]


viewErrorMsg : String -> Html Msg
viewErrorMsg errorMsg =
    div [ class "alert", class "alert-danger" ]
        [ span [] [ text errorMsg ]
        ]


viewSpinner : Html Msg
viewSpinner =
    div [ class "d-flex", class "justify-content-center" ]
        [ div [ class "spinner-border", class "text-primary", attribute "role" "status" ]
            [ span [ class "sr-only" ] [ text "Lade..." ] ]
        ]
