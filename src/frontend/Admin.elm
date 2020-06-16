module Admin exposing (main)

import Browser
import Helpers exposing (loadTimeZone, viewTitle, dateString, timeString, viewInitError)
import Html exposing (Html, button, div, input, label, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, disabled, scope, value)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe.Extra as MaybeX
import Result.Extra as ResultX
import Time exposing (Posix, Zone)
import Api exposing
    ( RaidAdminKey
    , RaidUserKey
    , User
    , RaidMode(..)
    , RaidStatus
    , getRaidStatus
    , setRaidMode
    , showRaidUserKey
    , raidAdminKeyDecoder
    , raidUserKeyDecoder
    , timeStringDecoder
    )

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type Model
    = InvalidModel String
    | ValidModel ModelData

type alias ModelData =
    { raidAdminKey : RaidAdminKey
    , raidUserKey : RaidUserKey
    , title : String
    , createdOn : Posix
    , userList : List (User, Posix)
    , errorMessage : Maybe String
    , raidMode : Maybe RaidMode
    , timeZone : Zone
    }

flagsDecoder : Decoder ModelData
flagsDecoder = 
    Decode.map4 ModelData
        (Decode.field "raidAdminKey" raidAdminKeyDecoder)
        (Decode.field "raidUserKey" raidUserKeyDecoder)
        (Decode.field "title" Decode.string)
        (Decode.field "createdOn" timeStringDecoder)
    |> Decode.map (\p -> p [] Nothing Nothing Time.utc)

type alias Flags = Encode.Value

init : Flags -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue flagsDecoder flags of
        Ok data -> 
            ( ValidModel data
            , Cmd.batch
                [ loadRegistrations data.raidAdminKey
                , loadTimeZone NewTimeZone
                ]
            )
        Err err ->
            ( InvalidModel (Decode.errorToString err)
            , Cmd.none
            )

type Msg
    = DoUpdate
    | Updated RaidStatus
    | DoSetMode RaidMode
    | ConnectionLost
    | ModeSet RaidMode
    | NewTimeZone Zone

loadRegistrations : RaidAdminKey -> Cmd Msg
loadRegistrations raidAdminKey =
    getRaidStatus raidAdminKey (ResultX.unwrap ConnectionLost Updated)

setMode : RaidAdminKey -> RaidMode -> Cmd Msg
setMode raidAdminKey mode =
    setRaidMode raidAdminKey mode
    <| ResultX.unwrap ConnectionLost (\() -> ModeSet mode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        InvalidModel _ -> (model, Cmd.none)
        ValidModel data ->
            let
                (newData, newCmd) = updateValidModel msg data
            in
                (ValidModel newData, newCmd)

updateValidModel : Msg -> ModelData -> ( ModelData, Cmd Msg )
updateValidModel msg model =
    case msg of
        ConnectionLost ->
            ( { model | raidMode = Nothing, errorMessage = Just "Verbindung zum Server verloren..." }, Cmd.none )
        DoUpdate ->
            ( model, loadRegistrations model.raidAdminKey )

        Updated raidStatus ->
            ( { model | userList = raidStatus.registrations, raidMode = Just raidStatus.mode, errorMessage = Nothing }, Cmd.none )

        DoSetMode mode ->
            ( { model | errorMessage = Nothing, raidMode = Nothing }, setMode model.raidAdminKey mode )

        ModeSet mode ->
            ( { model | raidMode = Just mode, errorMessage = Nothing }, Cmd.none )
        
        NewTimeZone zone ->
            ( { model | timeZone = zone }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 5000 (\_ -> DoUpdate)


view : Model -> Html Msg
view model =
    case model of
       InvalidModel err -> viewInitError err
       ValidModel data -> viewValidModel data

viewValidModel : ModelData -> Html Msg
viewValidModel model =
    div [ class "container-fluid", class "py-5" ]
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ viewTitle model.timeZone model.title model.createdOn
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-md-2" ] [ viewControls model.raidUserKey model.errorMessage model.raidMode ]
            , div [ class "col-md-10" ] [ viewUserList model.timeZone model.userList ]
            ]
        ]

viewControls : RaidUserKey -> Maybe String -> Maybe RaidMode -> Html Msg
viewControls raidUserKey errorMsg mode =
    div [ class "card" ]
        [ div [ class "card-body" ]
            [ MaybeX.unwrap (div [] []) viewErrorMsg errorMsg
            , div [ class "form-group" ]
                [ label [] [ text "Raid ID" ]
                , input [ class "form-control", disabled True, value (showRaidUserKey raidUserKey) ] []
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
            , classList [ ( "active", mode == RegistrationMode ) ]
            , onClick (DoSetMode RegistrationMode)
            ]
            [ text "1: Anmeldephase" ]
        , button
            [ class "list-group-item"
            , class "list-group-item-action"
            , classList [ ( "active", mode == TablesMode ) ]
            , onClick (DoSetMode TablesMode)
            ]
            [ text "2: Veröffentlichungsphase" ]
        ]

viewUserList : Zone -> List (User, Posix) -> Html Msg
viewUserList zone userList =
    table [ class "table", class "table-striped", class "table-bordered" ]
        [ thead []
            [ th [ scope "col" ] [ text "#" ]
            , th [ scope "col" ] [ text "Name" ]
            , th [ scope "col" ] [ text "Klasse" ]
            , th [ scope "col" ] [ text "Rolle" ]
            , th [ scope "col" ] [ text "Anmeldezeitpunkt" ]
            ]
        , tbody [] (List.indexedMap (\index (user, time) -> viewUserRow zone index user time) userList)
        ]

viewUserRow : Zone -> Int -> User -> Posix -> Html Msg
viewUserRow zone index user registeredOn =
    tr []
        [ th [ scope "row" ] [ text (String.fromInt (index + 1)) ]
        , td [] [ text user.userName ]
        , td [] [ text user.class ]
        , td [] [ text user.role ]
        , td [] [ text (registeredOnString zone registeredOn) ]
        ]

registeredOnString : Zone -> Posix -> String
registeredOnString zone time = dateString zone time ++ " um " ++ timeString zone time

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
